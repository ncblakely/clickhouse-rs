use chrono::{prelude::*, Duration};
use chrono_tz::Tz;
use either::Either;
use std::{
    collections::HashMap,
    hash::Hash,
    net::{Ipv4Addr, Ipv6Addr},
};

use crate::{
    errors::{Error, FromSqlError, Result},
    types::{
        column::datetime64::to_datetime,
        value::{decode_ipv4, decode_ipv6},
        Decimal, Enum16, Enum8, SqlType, Value, ValueRef,
    },
};

pub type FromSqlResult<T> = Result<T>;

/// Converts a decoded ClickHouse value into a Rust value.
///
/// Tuples are read positionally as Rust tuples of up to twelve elements, including
/// `()` and `(T,)`. Use `Value` or `ValueRef` to inspect tuples of arbitrary arity.
pub trait FromSql<'a>: Sized {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self>;
}

impl<'a> FromSql<'a> for ValueRef<'a> {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        Ok(value)
    }
}

impl<'a> FromSql<'a> for Value {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        Ok(value.into())
    }
}

macro_rules! from_sql_tuple_impl {
    ($($len:literal => ($($t:ident: $index:tt),*));* $(;)?) => {
        $(
            impl<'a, $($t: FromSql<'a>),*> FromSql<'a> for ($($t,)*) {
                fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
                    match value {
                        ValueRef::Tuple(_, values) if values.len() == $len => {
                            Ok(($($t::from_sql(values[$index].clone())?,)*))
                        }
                        value => Err(Error::FromSql(FromSqlError::InvalidType {
                            src: SqlType::from(value).to_string(),
                            dst: std::any::type_name::<Self>().into(),
                        })),
                    }
                }
            }

            impl<'a, $($t: FromSql<'a>),*> FromSql<'a> for Vec<($($t,)*)> {
                fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
                    match value {
                        ValueRef::Array(SqlType::Tuple(_), values) => values
                            .iter()
                            .cloned()
                            .map(<($($t,)*)>::from_sql)
                            .collect(),
                        value => Err(Error::FromSql(FromSqlError::InvalidType {
                            src: SqlType::from(value).to_string(),
                            dst: std::any::type_name::<Self>().into(),
                        })),
                    }
                }
            }
        )*
    };
}

from_sql_tuple_impl! {
    0 => ();
    1 => (A: 0);
    2 => (A: 0, B: 1);
    3 => (A: 0, B: 1, C: 2);
    4 => (A: 0, B: 1, C: 2, D: 3);
    5 => (A: 0, B: 1, C: 2, D: 3, E: 4);
    6 => (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5);
    7 => (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6);
    8 => (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7);
    9 => (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8);
    10 => (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9);
    11 => (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10);
    12 => (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10, L: 11);
}

macro_rules! from_sql_impl {
    ( $( $t:ident: $k:ident ),* ) => {
        $(
            impl<'a> FromSql<'a> for $t {
                fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
                    match value {
                        ValueRef::$k(v) => Ok(v),
                        _ => {
                            let from = SqlType::from(value.clone()).to_string();
                            Err(Error::FromSql(FromSqlError::InvalidType { src: from, dst: stringify!($t).into() }))
                        }
                    }
                }
            }
        )*
    };
}

impl<'a> FromSql<'a> for Decimal {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        match value {
            ValueRef::Decimal(v) => Ok(v),
            _ => {
                let from = SqlType::from(value.clone()).to_string();
                Err(Error::FromSql(FromSqlError::InvalidType {
                    src: from,
                    dst: "Decimal".into(),
                }))
            }
        }
    }
}

impl<'a> FromSql<'a> for Enum8 {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        match value {
            ValueRef::Enum8(_enum_values, e) => Ok(e),
            _ => {
                let from = SqlType::from(value.clone()).to_string();

                Err(Error::FromSql(FromSqlError::InvalidType {
                    src: from,
                    dst: "Enum8".into(),
                }))
            }
        }
    }
}

impl<'a> FromSql<'a> for Enum16 {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        match value {
            ValueRef::Enum16(_enum_values, e) => Ok(e),
            _ => {
                let from = SqlType::from(value.clone()).to_string();

                Err(Error::FromSql(FromSqlError::InvalidType {
                    src: from,
                    dst: "Enum16".into(),
                }))
            }
        }
    }
}

impl<'a> FromSql<'a> for &'a str {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<&'a str> {
        value.as_str()
    }
}

impl<'a> FromSql<'a> for &'a [u8] {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<&'a [u8]> {
        value.as_bytes()
    }
}

impl<'a> FromSql<'a> for String {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        value.as_str().map(str::to_string)
    }
}

impl<'a, K, V> FromSql<'a> for HashMap<K, V>
where
    K: FromSql<'a> + Eq + PartialEq + Hash,
    V: FromSql<'a>,
{
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        if let ValueRef::Map(_k, _v, hm) = value {
            let mut res = HashMap::with_capacity(hm.capacity());

            for (k, v) in hm.iter() {
                res.insert(K::from_sql(k.clone())?, V::from_sql(v.clone())?);
            }

            return Ok(res);
        }

        Err(Error::from("ohh no"))
    }
}

impl<'a> FromSql<'a> for Ipv4Addr {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        match value {
            ValueRef::Ipv4(ip) => Ok(decode_ipv4(&ip)),
            _ => {
                let from = SqlType::from(value.clone()).to_string();
                Err(Error::FromSql(FromSqlError::InvalidType {
                    src: from,
                    dst: "Ipv4".into(),
                }))
            }
        }
    }
}

impl<'a> FromSql<'a> for Ipv6Addr {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        match value {
            ValueRef::Ipv6(ip) => Ok(decode_ipv6(&ip)),
            _ => {
                let from = SqlType::from(value.clone()).to_string();
                Err(Error::FromSql(FromSqlError::InvalidType {
                    src: from,
                    dst: "Ipv6".into(),
                }))
            }
        }
    }
}

impl<'a> FromSql<'a> for uuid::Uuid {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        match value {
            ValueRef::Uuid(row) => {
                let mut buffer = row;
                buffer[..8].reverse();
                buffer[8..].reverse();
                Ok(uuid::Uuid::from_bytes(buffer))
            }
            _ => {
                let from = SqlType::from(value.clone()).to_string();
                Err(Error::FromSql(FromSqlError::InvalidType {
                    src: from,
                    dst: "Uuid".into(),
                }))
            }
        }
    }
}

macro_rules! from_sql_vec_impl {
    ( $( $t:ty: $k:pat => $f:expr ),* ) => {
        $(
            impl<'a> FromSql<'a> for Vec<$t> {
                fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
                    match value {
                        ValueRef::Array($k, vs) => {
                            let f: fn(ValueRef<'a>) -> FromSqlResult<$t> = $f;
                            let mut result = Vec::with_capacity(vs.len());
                            for r in vs.iter() {
                                let value: $t = f(r.clone())?;
                                result.push(value);
                            }
                            Ok(result)
                        }
                        _ => {
                            let from = SqlType::from(value.clone()).to_string();
                            Err(Error::FromSql(FromSqlError::InvalidType {
                                src: from,
                                dst: format!("Vec<{}>", stringify!($t)).into(),
                            }))
                        }
                    }
                }
            }
        )*
    };
}

from_sql_vec_impl! {
    &'a str: SqlType::String => |r| r.as_str(),
    String: SqlType::String => |r| r.as_string(),
    &'a [u8]: SqlType::String => |r| r.as_bytes(),
    Vec<u8>: SqlType::String => |r| r.as_bytes().map(<[u8]>::to_vec),
    NaiveDate: SqlType::Date => |r| Ok(r.into()),
    DateTime<Tz>: SqlType::DateTime(_) => |r| Ok(r.into()),
    Enum8: SqlType::Enum8(_) => |r| Ok(r.into()),
    Enum16: SqlType::Enum16(_) => |r| Ok(r.into())
}

impl<'a> FromSql<'a> for Vec<u8> {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        match value {
            ValueRef::Array(SqlType::UInt8, vs) => {
                let mut result = Vec::with_capacity(vs.len());
                for r in vs.iter() {
                    result.push(r.clone().into());
                }
                Ok(result)
            }
            _ => value.as_bytes().map(|bs| bs.to_vec()),
        }
    }
}

macro_rules! from_sql_vec_impl {
    ( $( $t:ident: $k:ident ),* ) => {
        $(
            impl<'a> FromSql<'a> for Vec<$t> {
                fn from_sql(value: ValueRef<'a>) -> Result<Self> {
                    match value {
                        ValueRef::Array(SqlType::$k, vs) => {
                            let mut result = Vec::with_capacity(vs.len());
                            for v in vs.iter() {
                                let val: $t = v.clone().into();
                                result.push(val);
                            }
                            Ok(result)
                        }
                        _ => {
                            let from = SqlType::from(value.clone()).to_string();
                            Err(Error::FromSql(FromSqlError::InvalidType { src: from, dst: stringify!($t).into() }))
                        }
                    }
                }
            }
        )*
    };
}

from_sql_vec_impl! {
    bool: Bool,

    i8: Int8,
    i16: Int16,
    i32: Int32,
    i64: Int64,
    i128: Int128,

    u16: UInt16,
    u32: UInt32,
    u64: UInt64,
    u128: UInt128,

    f32: Float32,
    f64: Float64
}

impl<'a, T> FromSql<'a> for Option<T>
where
    T: FromSql<'a>,
{
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        match value {
            ValueRef::Nullable(e) => match e {
                Either::Left(_) => Ok(None),
                Either::Right(u) => {
                    let value_ref = u.as_ref().clone();
                    Ok(Some(T::from_sql(value_ref)?))
                }
            },
            _ => {
                let from = SqlType::from(value.clone()).to_string();
                Err(Error::FromSql(FromSqlError::InvalidType {
                    src: from,
                    dst: stringify!($t).into(),
                }))
            }
        }
    }
}

impl<'a> FromSql<'a> for NaiveDate {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        match value {
            ValueRef::Date(v) => NaiveDate::from_ymd_opt(1970, 1, 1)
                .map(|unix_epoch| unix_epoch + Duration::days(v.into()))
                .ok_or(Error::FromSql(FromSqlError::OutOfRange)),
            _ => {
                let from = SqlType::from(value).to_string();
                Err(Error::FromSql(FromSqlError::InvalidType {
                    src: from,
                    dst: "NaiveDate".into(),
                }))
            }
        }
    }
}

impl<'a> FromSql<'a> for DateTime<Tz> {
    fn from_sql(value: ValueRef<'a>) -> FromSqlResult<Self> {
        match value {
            ValueRef::DateTime(v, tz) => {
                let time = tz.timestamp_opt(i64::from(v), 0).unwrap();
                Ok(time)
            }
            ValueRef::DateTime64(v, params) => {
                let (precision, tz) = *params;
                Ok(to_datetime(v, precision, tz))
            }
            _ => {
                let from = SqlType::from(value).to_string();
                Err(Error::FromSql(FromSqlError::InvalidType {
                    src: from,
                    dst: "DateTime<Tz>".into(),
                }))
            }
        }
    }
}

from_sql_impl! {
    bool: Bool,

    u8: UInt8,
    u16: UInt16,
    u32: UInt32,
    u64: UInt64,
    u128: UInt128,

    i8: Int8,
    i16: Int16,
    i32: Int32,
    i64: Int64,
    i128: Int128,

    f32: Float32,
    f64: Float64
}

#[cfg(test)]
mod test {
    use crate::types::{from_sql::FromSql, DateTimeType, SqlType, ValueRef};
    use chrono::prelude::*;
    use chrono_tz::Tz;
    use either::Either;

    #[test]
    fn test_u8() {
        let v = ValueRef::from(42_u8);
        let actual = u8::from_sql(v).unwrap();
        assert_eq!(actual, 42_u8);
    }

    #[test]
    fn test_bad_convert() {
        let v = ValueRef::from(42_u16);
        match u32::from_sql(v) {
            Ok(_) => panic!("should fail"),
            Err(e) => assert_eq!(
                "From SQL error: `SqlType::UInt16 cannot be cast to u32.`".to_string(),
                format!("{e}")
            ),
        }
    }

    #[test]
    fn null_to_datetime() {
        let null_value = ValueRef::Nullable(Either::Left(
            SqlType::DateTime(DateTimeType::DateTime32).into(),
        ));
        let date = Option::<DateTime<Tz>>::from_sql(null_value);
        assert_eq!(date.unwrap(), None);
    }
}
