pub trait StrUtils {
    fn split_first_whitespace(&self) -> (&str, &str);

    fn is_only_whitespace(&self) -> bool;
}

impl StrUtils for str {
    // Split the string at the first whitespace
    fn split_first_whitespace(&self) -> (&str, &str) {
        for (i, c) in self.char_indices() {
            if c.is_whitespace() {
                let first = &self[..i];
                // +c.len_utf8() чтобы срез начинался после пробела
                let second = &self[i + c.len_utf8()..];
                return (first, second);
            }
        }
        (self, "")
    }

    fn is_only_whitespace(&self) -> bool {
        self.chars().all(|c| c.is_whitespace())
    }
}

// Macros
// Specify the path to the test files
#[macro_export]
macro_rules! include_test_file {
    ($filename:expr) => {
        include_str!(concat!("../resources/tests/", $filename))
    };
}

// Recursive call to the tokenize method of the lexer
#[macro_export]
macro_rules! recursive_tokenize {
    ($tail:expr) => {
        Lexer::tokenize($tail)
    };
    ($tail:expr, $ret:expr) => {
        if $tail.len() > 0 {
            if let Ok(tail_tokens) = Lexer::tokenize($tail) {
                // Push all the tokens in the result vector
                for token in tail_tokens {
                    $ret.push(token);
                }
            }
        }
    };
}

#[macro_export]
macro_rules! recursive_tokenize_with_init {
    ($init:expr, $tail:expr) => {{
        let mut ret = vec![$init];
        recursive_tokenize!($tail, ret);
        return Ok(ret);
    }};
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_split_first_whitespace() {
        let text = r"\b I'm a bold string";
        let split = text.split_first_whitespace();
        assert_eq!(split, (r"\b", r"I'm a bold string"));
    }
}
