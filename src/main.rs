#[derive(Debug)]
enum Errors {
    CannotCreateStringWhileInOtherToken,
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum TokenType {
    SemiColon,
    StringLiteral,
    DoubleQuote,
    Ident,
    Number,
    Bind,
}
#[derive(Debug, Clone, Eq, PartialEq)]
struct Token {
    pub ty: TokenType,
    pub value: String,
}

fn tokenize(code: &str) -> Result<Vec<Token>, Errors> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut current_token: Option<Token> = None;
    for c in code.chars() {
        println!("current char is {}", c);
        if tokens.last().is_some()
            && tokens.last().unwrap().ty == TokenType::DoubleQuote
            && c != '"'
        {
            if let Some(tok) = &mut current_token {
                tok.value.push(c);
            } else {
                current_token = Some(Token {
                    ty: TokenType::StringLiteral,
                    value: c.to_string(),
                })
            }
        } else if c == ';' {
            println!("1");
            if let Some(tok) = &current_token {
                tokens.push(tok.clone());
            }
            tokens.push(Token {
                ty: TokenType::SemiColon,
                value: String::from(";"),
            });

            current_token = None;
        } else if c == '=' {
            println!("2");
            if let Some(tok) = &current_token {
                tokens.push(tok.clone());
            }
            tokens.push(Token {
                ty: TokenType::Bind,
                value: String::from("="),
            });
        } else if (c >= 'A' && c <= 'z')
            || c == '_'
            || (current_token.is_some()
                && current_token.clone().unwrap().ty == TokenType::Ident
                && c != ' ')
        {
            println!("4");
            if let Some(tok) = &mut current_token {
                tok.value.push(c);
            } else {
                current_token = Some(Token {
                    ty: TokenType::Ident,
                    value: String::from(c.to_string()),
                });
            }
        } else if c == ' ' {
            println!("5");
            if let Some(tok) = &current_token {
                tokens.push(tok.clone());
            }
            current_token = None;
        } else if c >= '0' && c <= '9' {
            println!("6");
            if let Some(tok) = &mut current_token {
                tok.value.push(c);
            } else {
                current_token = Some(Token {
                    ty: TokenType::Number,
                    value: String::from(c.to_string()),
                });
            }
        } else if c == '"' {
            println!("7");
            if current_token.is_some()
                && current_token.clone().unwrap().ty != TokenType::StringLiteral
            {
                return Err(Errors::CannotCreateStringWhileInOtherToken);
            }

            tokens.push(Token {
                ty: TokenType::DoubleQuote,
                value: String::from("\""),
            });
        }
    }
    if let Some(tok) = current_token {
        tokens.push(tok);
    }
    Ok(tokens)
}
fn main() {}

#[cfg(test)]
mod tests {
    fn eq_vecs<T: Eq>(v1: Vec<T>, v2: Vec<T>) -> bool {
        if v1.len() != v2.len() {
            return false;
        }
        for i in 0..v1.len() {
            if v1[i] != v2[i] {
                return false;
            }
        }
        return true;
    }
    use super::*;
    #[test]
    fn number_token() {
        let tokens = tokenize("123");
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        eq_vecs(
            tokens,
            vec![Token {
                ty: TokenType::Number,
                value: String::from("123"),
            }],
        );
    }
    #[test]
    fn test_assign_number() {
        let tokens = tokenize("x  =   123;");
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        assert_eq!(tokens.len(), 4);
        assert!(eq_vecs(
            tokens,
            vec![
                Token {
                    ty: TokenType::Ident,
                    value: String::from("x")
                },
                Token {
                    ty: TokenType::Bind,
                    value: String::from("=")
                },
                Token {
                    ty: TokenType::Number,
                    value: String::from("123")
                },
                Token {
                    ty: TokenType::SemiColon,
                    value: String::from(";")
                }
            ]
        ));
    }

    #[test]
    fn test_assign_string() {
        let tokens = tokenize("x  =   \"Salam\";");
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        assert!(eq_vecs(
            tokens,
            vec![
                Token {
                    ty: TokenType::Ident,
                    value: String::from("x")
                },
                Token {
                    ty: TokenType::Bind,
                    value: String::from("=")
                },
                Token {
                    ty: TokenType::DoubleQuote,
                    value: String::from("\"")
                },
                Token {
                    ty: TokenType::StringLiteral,
                    value: String::from("Salam")
                },
                Token {
                    ty: TokenType::DoubleQuote,
                    value: String::from("\"")
                },
                Token {
                    ty: TokenType::SemiColon,
                    value: String::from(";")
                }
            ]
        ));
    }
}
