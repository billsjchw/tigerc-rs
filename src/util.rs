pub fn parse_integer_literal(literal: &str) -> i64 {
    let mut result = 0i64;

    for &c in literal.as_bytes() {
        result = result.wrapping_mul(10).wrapping_add((c - b'0') as i64);
    }

    result
}

pub fn parse_string_literal(literal: &str) -> String {
    let mut result = String::new();
    let bytes = literal.as_bytes();
    let mut i = 1;

    while i + 1 < bytes.len() {
        if bytes[i] != b'\\' {
            result.push(bytes[i] as char);
            i += 1;
        } else if bytes[i + 1] == b'n' {
            result.push('\n');
            i += 2;
        } else if bytes[i + 1] == b't' {
            result.push('\t');
            i += 2;
        } else if bytes[i + 1] == b'"' {
            result.push('"');
            i += 2;
        } else if bytes[i + 1] == b'\\' {
            result.push('\\');
            i += 2;
        } else if bytes[i + 1] >= b'0' && bytes[i + 1] <= b'9' {
            result.push(
                (bytes[i + 1] - b'0')
                    .wrapping_mul(64)
                    .wrapping_add((bytes[i + 2] - b'0') * 8)
                    .wrapping_add(bytes[i + 3] - b'0') as char,
            );
            i += 4;
        } else {
            i += 1;
            while bytes[i] != b'\\' {
                i += 1;
            }
            i += 1;
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::parse_integer_literal;
    use super::parse_string_literal;

    #[test]
    fn test_parse_integer_literal() {
        assert_eq!(parse_integer_literal("0"), 0);
        assert_eq!(parse_integer_literal("123"), 123);
        assert_eq!(parse_integer_literal("9223372036854775807"), i64::MAX);
        assert_eq!(parse_integer_literal("18446744073709551616"), 0);
    }

    #[test]
    fn test_parse_string_literal() {
        assert_eq!(
            parse_string_literal("\"123\\n\\t\\\"\\\\xyz\""),
            "123\n\t\"\\xyz"
        );
        assert_eq!(
            parse_string_literal("\"\\110\\145\\154\\154\\157\""),
            "Hello"
        );
        assert_eq!(
            parse_string_literal("\"Hello,\\\n\t  \\ world!\""),
            "Hello, world!"
        );
    }
}
