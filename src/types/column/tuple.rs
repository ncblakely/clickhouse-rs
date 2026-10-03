use std::sync::Arc;

use chrono_tz::Tz;

use crate::{
    binary::{Encoder, ReadEx},
    errors::Result,
    types::{Column, Simple, SqlType, Value, ValueRef},
};

use super::{
    column_data::{BoxColumnData, ColumnData},
    new_column, ArcColumnWrapper,
};

pub(crate) struct TupleColumnData {
    columns: Vec<Column<Simple>>,
    sql_type: &'static SqlType,
    rows: usize,
}

impl TupleColumnData {
    pub(crate) fn load<R: ReadEx>(
        reader: &mut R,
        fields: Vec<(Option<String>, &str)>,
        rows: usize,
        tz: Tz,
    ) -> Result<Self> {
        if fields.is_empty() {
            // Native represents each empty tuple with one ignored byte.
            reader.read_bytes(&mut vec![0; rows])?;
        }
        let mut columns = Vec::with_capacity(fields.len());
        let mut types = Vec::with_capacity(fields.len());
        for (name, type_name) in fields {
            let data = <dyn ColumnData>::load_data_body::<ArcColumnWrapper, _>(
                reader, type_name, rows, tz,
            )?;
            types.push((name, data.sql_type()));
            columns.push(new_column("", data));
        }
        Ok(Self {
            columns,
            sql_type: SqlType::Tuple(types).into(),
            rows,
        })
    }
}

impl ColumnData for TupleColumnData {
    fn sql_type(&self) -> SqlType {
        self.sql_type.clone()
    }

    fn save(&self, encoder: &mut Encoder, start: usize, end: usize) {
        if self.columns.is_empty() {
            for _ in start..end {
                encoder.write(b'0');
            }
        }
        for column in &self.columns {
            column.data.save(encoder, start, end);
        }
    }

    fn len(&self) -> usize {
        self.rows
    }

    fn push(&mut self, value: Value) {
        if let Value::Tuple(sql_type, values) = value {
            assert_eq!(sql_type, self.sql_type, "tuple types must match");
            assert_eq!(values.len(), self.columns.len(), "tuple arity must match");
            for (column, value) in self.columns.iter_mut().zip(values.iter()) {
                column.push(value.clone());
            }
            self.rows += 1;
        } else {
            panic!("value should be a tuple");
        }
    }

    fn at(&self, index: usize) -> ValueRef<'_> {
        assert!(index < self.rows, "tuple row index out of bounds");
        let values = self.columns.iter().map(|column| column.at(index)).collect();
        ValueRef::Tuple(self.sql_type, Arc::new(values))
    }

    fn clone_instance(&self) -> BoxColumnData {
        Box::new(Self {
            columns: self.columns.clone(),
            sql_type: self.sql_type,
            rows: self.rows,
        })
    }

    fn get_timezone(&self) -> Option<Tz> {
        self.columns
            .iter()
            .find_map(|column| column.data.get_timezone())
    }
}

pub(super) fn parse_tuple_type(source: &str) -> Option<Vec<(Option<String>, &str)>> {
    let body = source.strip_prefix("Tuple(")?.strip_suffix(')')?;
    if body.trim().is_empty() {
        return Some(Vec::new());
    }

    let mut parts = Vec::new();
    let mut start = 0;
    let mut depth = 0_usize;
    let mut quote = None;
    let mut escaped = false;
    for (index, ch) in body.char_indices() {
        if escaped {
            escaped = false;
        } else if let Some(delimiter) = quote {
            if ch == '\\' {
                escaped = true;
            } else if ch == delimiter {
                quote = None;
            }
        } else {
            match ch {
                '\'' | '"' | '`' => quote = Some(ch),
                '(' => depth += 1,
                ')' => depth = depth.checked_sub(1)?,
                ',' if depth == 0 => {
                    parts.push(body[start..index].trim());
                    start = index + 1;
                }
                _ => {}
            }
        }
    }
    if depth != 0 || quote.is_some() {
        return None;
    }
    parts.push(body[start..].trim());

    let fields: Vec<_> = parts
        .into_iter()
        .map(parse_tuple_field)
        .collect::<Option<_>>()?;
    let named = fields.first()?.0.is_some();
    if fields.iter().any(|(name, _)| name.is_some() != named) {
        return None;
    }
    Some(fields)
}

fn parse_tuple_field(source: &str) -> Option<(Option<String>, &str)> {
    let first = source.chars().next()?;
    if first == '`' || first == '"' {
        let mut name = String::new();
        let mut chars = source.char_indices().skip(1).peekable();
        while let Some((index, ch)) = chars.next() {
            match ch {
                '\\' => {
                    let (_, escaped) = chars.next()?;
                    name.push(match escaped {
                        '0' => '\0',
                        'b' => '\u{0008}',
                        'f' => '\u{000c}',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    });
                }
                ch if ch == first => {
                    if chars.peek().map(|(_, ch)| *ch) == Some(first) {
                        chars.next();
                        name.push(first);
                        continue;
                    }
                    let rest = &source[index + ch.len_utf8()..];
                    if !rest.starts_with(char::is_whitespace) || rest.trim().is_empty() {
                        return None;
                    }
                    return Some((Some(name), rest.trim()));
                }
                other => name.push(other),
            }
        }
        return None;
    }

    let end = source
        .find(|ch: char| !ch.is_ascii_alphanumeric() && ch != '_')
        .unwrap_or(source.len());
    let rest = &source[end..];
    if (first.is_ascii_alphabetic() || first == '_')
        && rest.starts_with(char::is_whitespace)
        && !rest.trim_start().starts_with('(')
    {
        if rest.trim().is_empty() {
            return None;
        }
        Some((Some(source[..end].to_owned()), rest.trim()))
    } else {
        Some((None, source))
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, io::Cursor};

    use super::*;
    use crate::{
        binary::protocol,
        errors::{Error, FromSqlError},
        types::{Block, FromSql},
    };

    fn decode_block(type_name: &str, rows: usize, data: &[u8]) -> Result<Block> {
        let revision = protocol::DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION;
        let mut encoder = Encoder::new();
        encoder.uvarint(0);
        encoder.uvarint(2);
        encoder.uvarint(rows as u64);
        encoder.string("t");
        encoder.string(type_name);
        encoder.write(0_u8);
        encoder.write_bytes(data);
        encoder.string("tail");
        encoder.string("UInt8");
        encoder.write(0_u8);
        encoder.write_bytes(&vec![99; rows]);

        let bytes = encoder.get_buffer();
        let mut reader = Cursor::new(&bytes);
        let block = Block::load(&mut reader, Tz::UTC, false, revision)?;
        assert_eq!(reader.position() as usize, bytes.len());
        assert_eq!(block.row_count(), rows);
        for row in 0..rows {
            assert_eq!(block.get::<u8, _>(row, "tail")?, 99);
        }
        Ok(block)
    }

    #[test]
    fn test_tuple_columnar_decode() -> Result<()> {
        // Tuple fields are whole columns, not interleaved row values.
        let block = decode_block("Tuple(UInt8, String)", 2, &[1, 2, 1, b'a', 2, b'b', b'c'])?;
        assert_eq!(block.get::<(u8, &str), _>(0, "t")?, (1, "a"));
        assert_eq!(block.get::<(u8, String), _>(1, "t")?, (2, "bc".into()));
        assert_eq!(
            block.rows().next().unwrap().get::<(u8, &str), _>("t")?,
            (1, "a")
        );

        let combined = Block::concat(&[block.clone(), block]);
        assert_eq!(combined.get::<(u8, &str), _>(3, "t")?, (2, "bc"));
        Ok(())
    }

    #[test]
    fn test_tuple_named_values_preserve_schema() -> Result<()> {
        let block = decode_block("Tuple(id UInt8, `display name` String)", 1, &[7, 1, b'x'])?;
        let expected = SqlType::Tuple(vec![
            (Some("id".into()), SqlType::UInt8),
            (Some("display name".into()), SqlType::String),
        ]);
        assert_eq!(block.get_column("t")?.sql_type(), expected);
        assert_eq!(block.get::<(u8, &str), _>(0, "t")?, (7, "x"));
        let borrowed = block.get::<ValueRef<'_>, _>(0, "t")?;
        let owned = block.get::<Value, _>(0, "t")?;
        assert_eq!(SqlType::from(borrowed.clone()), expected);
        assert_eq!(SqlType::from(owned.clone()), expected);
        assert_eq!(ValueRef::from(&owned), borrowed);
        assert_eq!(Value::from(borrowed), owned);
        assert_eq!(owned.to_string(), "(7, x)");
        assert_eq!(
            expected.to_string(),
            "Tuple(`id` UInt8, `display name` String)"
        );
        Ok(())
    }

    #[test]
    fn test_tuple_nested_nullable_and_array() -> Result<()> {
        let mut data = Encoder::new();
        data.write_bytes(&[7, 8]);
        data.string("first");
        data.string("second");
        data.write_bytes(&[0, 1]);
        data.write(-5_i16);
        data.write(0_i16);
        data.write(2_u64);
        data.write(3_u64);
        for value in [10_u32, 11, 12] {
            data.write(value);
        }
        let block = decode_block(
            "Tuple(UInt8, Tuple(String, Nullable(Int16)), Array(UInt32))",
            2,
            data.get_buffer_ref(),
        )?;
        type Row<'a> = (u8, (&'a str, Option<i16>), Vec<u32>);
        assert_eq!(
            block.get::<Row<'_>, _>(0, "t")?,
            (7, ("first", Some(-5)), vec![10, 11])
        );
        assert_eq!(
            block.get::<Row<'_>, _>(1, "t")?,
            (8, ("second", None), vec![12])
        );
        Ok(())
    }

    #[test]
    fn test_tuple_inside_array() -> Result<()> {
        let mut data = Encoder::new();
        for offset in [2_u64, 2, 3] {
            data.write(offset);
        }
        data.write_bytes(&[1, 2, 3]);
        for value in ["one", "two", "three"] {
            data.string(value);
        }
        let block = decode_block("Array(Tuple(UInt8, String))", 3, data.get_buffer_ref())?;
        assert_eq!(
            block.get::<Vec<(u8, &str)>, _>(0, "t")?,
            vec![(1, "one"), (2, "two")]
        );
        assert!(block.get::<Vec<(u8, &str)>, _>(1, "t")?.is_empty());
        assert_eq!(block.get::<Vec<(u8, &str)>, _>(2, "t")?, vec![(3, "three")]);
        Ok(())
    }

    #[test]
    fn test_tuple_inside_map() -> Result<()> {
        let mut data = Encoder::new();
        data.write(1_u64);
        data.string("key");
        data.write(42_u8);
        data.string("value");
        let block = decode_block(
            "Map(String, Tuple(UInt8, String))",
            1,
            data.get_buffer_ref(),
        )?;
        assert_eq!(
            block.get::<HashMap<&str, (u8, &str)>, _>(0, "t")?,
            HashMap::from([("key", (42, "value"))])
        );
        Ok(())
    }

    #[test]
    fn test_tuple_inside_simple_aggregate_function() -> Result<()> {
        let block = decode_block(
            "Tuple(SimpleAggregateFunction(any, Tuple(UInt8, String)))",
            1,
            &[7, 1, b'x'],
        )?;
        assert_eq!(block.get::<((u8, &str),), _>(0, "t")?, ((7, "x"),));
        Ok(())
    }

    fn write_dictionary(data: &mut Encoder, dictionary: &[&str], indexes: &[u8]) {
        data.write(0x600_u64);
        data.write(dictionary.len() as u64);
        for value in dictionary {
            data.string(value);
        }
        data.write(indexes.len() as u64);
        data.write_bytes(indexes);
    }

    #[test]
    fn test_tuple_low_cardinality_prefixes_precede_all_values() -> Result<()> {
        let mut data = Encoder::new();
        data.write(1_u64);
        data.write(1_u64);
        data.write_bytes(&[7, 8]);
        write_dictionary(&mut data, &["", "a", "b"], &[1, 2]);
        write_dictionary(&mut data, &["", "x"], &[1, 1]);
        let block = decode_block(
            "Tuple(UInt8, LowCardinality(String), Tuple(LowCardinality(String)))",
            2,
            data.get_buffer_ref(),
        )?;
        assert_eq!(
            block.get::<(u8, &str, (&str,)), _>(0, "t")?,
            (7, "a", ("x",))
        );
        assert_eq!(
            block.get::<(u8, &str, (&str,)), _>(1, "t")?,
            (8, "b", ("x",))
        );
        decode_block("Tuple(UInt8, LowCardinality(String))", 0, &[])?;
        Ok(())
    }

    #[test]
    fn test_tuple_low_cardinality_prefix_precedes_array_offsets() -> Result<()> {
        let mut data = Encoder::new();
        data.write(1_u64);
        data.write(1_u64);
        data.write(1_u64);
        data.write(7_u8);
        write_dictionary(&mut data, &["", "x"], &[1]);
        let block = decode_block(
            "Array(Tuple(UInt8, LowCardinality(String)))",
            2,
            data.get_buffer_ref(),
        )?;
        assert_eq!(block.get::<Vec<(u8, &str)>, _>(0, "t")?, vec![(7, "x")]);
        assert!(block.get::<Vec<(u8, &str)>, _>(1, "t")?.is_empty());
        Ok(())
    }

    #[test]
    fn test_tuple_invalid_low_cardinality_prefix() {
        let result = decode_block("Tuple(UInt8, LowCardinality(String))", 1, &[2; 8]);
        assert!(matches!(
            result,
            Err(Error::Driver(crate::errors::DriverError::Deserialize(_)))
        ));
    }

    #[test]
    fn test_tuple_empty_and_singleton() -> Result<()> {
        let empty = decode_block("Tuple()", 2, b"00")?;
        empty.get::<(), _>(0, "t")?;
        empty.get::<(), _>(1, "t")?;
        let singleton = decode_block("Tuple(UInt8)", 1, &[42])?;
        assert_eq!(singleton.get::<(u8,), _>(0, "t")?, (42,));
        let no_rows = decode_block("Tuple(UInt8, Tuple(String, Nullable(Int16)))", 0, &[])?;
        assert_eq!(
            no_rows.get_column("t")?.sql_type().to_string(),
            "Tuple(UInt8, Tuple(String, Nullable(Int16)))"
        );
        Ok(())
    }

    #[test]
    fn test_tuple_dynamic_large_arity() -> Result<()> {
        let type_name = format!("Tuple({})", vec!["UInt8"; 13].join(", "));
        let block = decode_block(&type_name, 1, &[42; 13])?;
        match block.get::<ValueRef<'_>, _>(0, "t")? {
            ValueRef::Tuple(_, values) => {
                assert_eq!(values.len(), 13);
                assert!(values.iter().all(|value| value == &ValueRef::UInt8(42)));
            }
            other => panic!("expected tuple, got {other:?}"),
        }
        Ok(())
    }

    #[test]
    fn test_tuple_twelve_element_conversion() -> Result<()> {
        type Twelve = (u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8);
        let type_name = format!("Tuple({})", vec!["UInt8"; 12].join(", "));
        let block = decode_block(&type_name, 1, &[42; 12])?;
        assert_eq!(
            block.get::<Twelve, _>(0, "t")?,
            (42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42)
        );
        Ok(())
    }

    #[test]
    fn test_tuple_insert_rejected() -> Result<()> {
        for block in [
            decode_block("Tuple(UInt8)", 1, &[42])?,
            decode_block("Array(Tuple(UInt8))", 1, &[1, 0, 0, 0, 0, 0, 0, 0, 42])?,
        ] {
            assert!(matches!(
                block.cast_to(&block),
                Err(Error::Driver(
                    crate::errors::DriverError::TupleInsertUnsupported
                ))
            ));
            let mut output = Block::new();
            assert!(matches!(
                output.push(vec![("t".into(), block.get::<Value, _>(0, "t")?)]),
                Err(Error::Driver(
                    crate::errors::DriverError::TupleInsertUnsupported
                ))
            ));
        }
        Ok(())
    }

    #[test]
    fn test_tuple_conversion_errors() -> Result<()> {
        let block = decode_block("Tuple(UInt8, String)", 1, &[1, 1, b'a'])?;
        for error in [
            block.get::<(u8,), _>(0, "t").unwrap_err(),
            block.get::<(u32, &str), _>(0, "t").unwrap_err(),
            block.get::<(), _>(0, "t").unwrap_err(),
            <(u8,)>::from_sql(ValueRef::UInt8(1)).unwrap_err(),
            Vec::<(u8,)>::from_sql(ValueRef::UInt8(1)).unwrap_err(),
        ] {
            assert!(matches!(
                error,
                Error::FromSql(FromSqlError::InvalidType { .. })
            ));
        }
        Ok(())
    }

    #[test]
    fn test_tuple_truncated_payload() {
        let bytes = [1, 2, 1, b'a', 2, b'b', b'c'];
        for end in 0..bytes.len() {
            let result = <dyn ColumnData>::load_data::<ArcColumnWrapper, _>(
                &mut Cursor::new(&bytes[..end]),
                "Tuple(UInt8, String)",
                2,
                Tz::UTC,
            );
            assert!(
                result.err().unwrap().is_would_block(),
                "prefix length {end}"
            );
        }
    }

    #[test]
    fn test_tuple_parse_nested_and_quoted() {
        assert_eq!(
            parse_tuple_type("Tuple(UInt8, Decimal(9, 2), Tuple(String, Array(Int32)))"),
            Some(vec![
                (None, "UInt8"),
                (None, "Decimal(9, 2)"),
                (None, "Tuple(String, Array(Int32))"),
            ])
        );
        assert_eq!(
            parse_tuple_type(
                r#"Tuple(`a,b` Enum8('x,y' = 1, 'a\')b' = 2), "a""b" Tuple(UInt8, String), `a\\b\`c` UInt8)"#
            ),
            Some(vec![
                (Some("a,b".into()), r"Enum8('x,y' = 1, 'a\')b' = 2)"),
                (Some("a\"b".into()), "Tuple(UInt8, String)"),
                (Some("a\\b`c".into()), "UInt8"),
            ])
        );
    }

    #[test]
    fn test_tuple_parse_invalid() {
        for source in [
            "Tuple",
            "Tuple(",
            "Tuple(UInt8",
            "Tuple(UInt8))",
            "Tuple(UInt8,)",
            "Tuple(,UInt8)",
            "Tuple(UInt8,,String)",
            "Tuple(UInt8, name String)",
            "Tuple(`name`)",
            "Tuple(`name`UInt8)",
            "Tuple(`name UInt8)",
            "Tuple(Enum8('unterminated = 1))",
            "Tuple(Array(UInt8)",
            "Tuple(UInt8) trailing",
        ] {
            assert!(parse_tuple_type(source).is_none(), "{source}");
        }
        assert!(decode_block("Tuple(Unsupported)", 0, &[]).is_err());
        for type_name in ["Array", "Nullable", "Map", "FixedString", "LowCardinality"] {
            assert!(decode_block(&format!("Tuple({type_name})"), 0, &[]).is_err());
        }
    }
}
