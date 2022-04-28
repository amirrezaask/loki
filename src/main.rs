#[derive(Debug)]
enum Errors {
    CannotCreateStringWhileInOtherToken,
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum TokenType {
    SemiColon,
    StringLiteral,
    IfKeyword,
    ForKeyword,
    InterfaceKeyword,
    StructKeyword,
    DoubleQuoteStart,
    DoubleQuoteEnd,
    CuBracketOpen,
    CuBracketClose,
    SqBracketOpen,
    SqBracketClose,
    Comma,
    Ident,
    Number,
    AssignOp,
    Colon,
    IntKeyword,
}
#[derive(Debug, Clone, Eq, PartialEq)]
struct Token {
    pub ty: TokenType,
    pub value: Option<String>,
}

fn tokenize(code: &str) -> Result<Vec<Token>, Errors> {
    let mut tokens: Vec<Token> = Vec::new();
    // TODO can we use Rc<Option<Token>> ?
    let mut current_token: Option<Token> = None;
    for c in code.chars() {
        println!("current char is {}", c);
        if tokens.last().is_some()
            && tokens.last().unwrap().ty == TokenType::DoubleQuoteStart
            && c != '"'
        {
            println!("inside string literal");
            if let Some(tok) = &mut current_token {
                match &mut tok.value {
                    Some(s) => s.push(c),
                    None => tok.value = Some(c.to_string()),
                };
            } else {
                current_token = Some(Token {
                    ty: TokenType::StringLiteral,
                    value: Some(c.to_string()),
                })
            }
        } else if c == ';' {
            println!("SEMI COLON");
            if let Some(tok) = &current_token {
                tokens.push(tok.clone());
            }
            tokens.push(Token {
                ty: TokenType::SemiColon,
                value: None,
            });

            current_token = None;
        } else if c == ':' {
            println!("COLON");
            if let Some(tok) = &current_token {
                tokens.push(tok.clone());
            }
            tokens.push(Token {
                ty: TokenType::Colon,
                value: None,
            });

            current_token = None;
        } else if c == '=' {
            println!("assign");
            if let Some(tok) = &current_token {
                tokens.push(tok.clone());
            }
            tokens.push(Token {
                ty: TokenType::AssignOp,
                value: None,
            });
        } else if c == '[' {
            println!("OPEN SQ BRACKET");
            if current_token.is_some() {
                tokens.push(current_token.clone().unwrap());
            }
            tokens.push(Token {
                ty: TokenType::SqBracketOpen,
                value: None,
            });
            current_token = None;
        } else if c == ']' {
            println!("CLOSE SQ BRACKET");
            if current_token.is_some() {
                tokens.push(current_token.clone().unwrap());
            }
            tokens.push(Token {
                ty: TokenType::SqBracketClose,
                value: None,
            });
            current_token = None;
        } else if c == '{' {
            println!("OPEN CU BRACKET");
            if current_token.is_some() {
                tokens.push(current_token.clone().unwrap());
            }
            tokens.push(Token {
                ty: TokenType::CuBracketOpen,
                value: None,
            });
            current_token = None;
        } else if c == '}' {
            println!("CLOSE CU BRACKET");
            if current_token.is_some() {
                tokens.push(current_token.clone().unwrap());
            }
            tokens.push(Token {
                ty: TokenType::CuBracketClose,
                value: None,
            });
            current_token = None;
        } else if c == ',' {
            println!("COMMA State");
            if current_token.is_some() {
                tokens.push(current_token.clone().unwrap());
            }
            tokens.push(Token {
                ty: TokenType::Comma,
                value: None,
            });
            current_token = None;
        } else if (c >= 'A' && c <= 'z' && c != '[' && c != ']')
            || c == '_'
            || (current_token.is_some()
                && current_token.clone().unwrap().ty == TokenType::Ident
                && c != ' '
                && c != '\t'
                && c != '\n')
        {
            println!("identifier/keyword");
            if let Some(tok) = &mut current_token {
                match &mut tok.value {
                    Some(s) => {
                        s.push(c);
                        if s == "if" {
                            tokens.push(Token {
                                ty: TokenType::IfKeyword,
                                value: None,
                            });
                            current_token = None;
                            continue;
                        } else if s == "interface" {
                            tokens.push(Token {
                                ty: TokenType::InterfaceKeyword,
                                value: None,
                            });
                            current_token = None;
                            continue;
                        } else if s == "struct" {
                            tokens.push(Token {
                                ty: TokenType::StructKeyword,
                                value: None,
                            });
                            current_token = None;
                            continue;
                        } else if s == "for" {
                            tokens.push(Token {
                                ty: TokenType::ForKeyword,
                                value: None,
                            });
                            current_token = None;
                            continue;
                        } else if s == "int" {
                            tokens.push(Token {
                                ty: TokenType::IntKeyword,
                                value: None,
                            });
                            current_token = None;
                            continue;
                        }
                    }
                    None => tok.value = Some(c.to_string()),
                };
            } else {
                current_token = Some(Token {
                    ty: TokenType::Ident,
                    value: Some(String::from(c.to_string())),
                });
            }
        } else if c == ' ' || c == '\n' || c == '\t' {
            println!("space|newline");
            if let Some(tok) = &current_token {
                tokens.push(tok.clone());
            }
            current_token = None;
        } else if c >= '0' && c <= '9' {
            println!("Number state");
            if let Some(tok) = &mut current_token {
                match &mut tok.value {
                    Some(s) => s.push(c),
                    None => tok.value = Some(c.to_string()),
                }
            } else {
                current_token = Some(Token {
                    ty: TokenType::Number,
                    value: Some(String::from(c.to_string())),
                });
            }
        } else if c == '"' {
            println!("Double Quote state");
            if current_token.is_some()
                && current_token.clone().unwrap().ty != TokenType::StringLiteral
            {
                return Err(Errors::CannotCreateStringWhileInOtherToken);
            } else if current_token.is_some()
                && current_token.clone().unwrap().ty == TokenType::StringLiteral
            {
                println!("ending string literal");
                tokens.push(current_token.clone().unwrap());
                tokens.push(Token {
                    ty: TokenType::DoubleQuoteEnd,
                    value: None,
                });
                current_token = None;
            } else {
                tokens.push(Token {
                    ty: TokenType::DoubleQuoteStart,
                    value: None,
                });
            }
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
                value: Some(String::from("123")),
            }],
        );
    }

    #[test]
    fn test_assign_struct() {
        let tokens = tokenize("x = struct{\n\ty: int\n};");
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        println!("{:?}", tokens);
        assert!(eq_vecs(
            tokens,
            vec![
                Token {
                    ty: TokenType::Ident,
                    value: Some(String::from("x"))
                },
                Token {
                    ty: TokenType::AssignOp,
                    value: None,
                },
                Token {
                    ty: TokenType::StructKeyword,
                    value: None,
                },
                Token {
                    ty: TokenType::CuBracketOpen,
                    value: None,
                },
                Token {
                    ty: TokenType::Ident,
                    value: Some(String::from("y")),
                },
                Token {
                    ty: TokenType::Colon,
                    value: None,
                },
                Token {
                    ty: TokenType::IntKeyword,
                    value: None,
                },
                Token {
                    ty: TokenType::CuBracketClose,
                    value: None,
                },
                Token {
                    ty: TokenType::SemiColon,
                    value: None,
                }
            ]
        ));
    }

    #[test]
    fn test_assign_interface() {
        let tokens = tokenize("x = interface{};");
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        println!("{:?}", tokens);
        assert!(eq_vecs(
            tokens,
            vec![
                Token {
                    ty: TokenType::Ident,
                    value: Some(String::from("x"))
                },
                Token {
                    ty: TokenType::AssignOp,
                    value: None,
                },
                Token {
                    ty: TokenType::InterfaceKeyword,
                    value: None,
                },
                Token {
                    ty: TokenType::CuBracketOpen,
                    value: None,
                },
                Token {
                    ty: TokenType::CuBracketClose,
                    value: None,
                },
                Token {
                    ty: TokenType::SemiColon,
                    value: None,
                }
            ]
        ));
    }

    #[test]
    fn string_token() {
        let tokens = tokenize("\"amirreza\"");
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        eq_vecs(
            tokens,
            vec![
                Token {
                    ty: TokenType::DoubleQuoteStart,
                    value: None,
                },
                Token {
                    ty: TokenType::StringLiteral,
                    value: Some(String::from("amirreza")),
                },
                Token {
                    ty: TokenType::DoubleQuoteEnd,
                    value: None,
                },
            ],
        );
    }
    #[test]
    fn test_assign_number_mixed_with_sq_brackets() {
        let tokens = tokenize("x  =   123[12, \"name\"];");
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        println!("{:?}", tokens);
        assert!(eq_vecs(
            tokens,
            vec![
                Token {
                    ty: TokenType::Ident,
                    value: Some(String::from("x"))
                },
                Token {
                    ty: TokenType::AssignOp,
                    value: None,
                },
                Token {
                    ty: TokenType::Number,
                    value: Some(String::from("123"))
                },
                Token {
                    ty: TokenType::SqBracketOpen,
                    value: None,
                },
                Token {
                    ty: TokenType::Number,
                    value: Some(String::from("12"))
                },
                Token {
                    ty: TokenType::Comma,
                    value: None,
                },
                Token {
                    ty: TokenType::DoubleQuoteStart,
                    value: None,
                },
                Token {
                    ty: TokenType::StringLiteral,
                    value: Some(String::from("name")),
                },
                Token {
                    ty: TokenType::DoubleQuoteEnd,
                    value: None,
                },
                Token {
                    ty: TokenType::SqBracketClose,
                    value: None,
                },
                Token {
                    ty: TokenType::SemiColon,
                    value: None
                }
            ]
        ));
    }

    #[test]
    fn test_assign_string() {
        let tokens = tokenize("x  =   \" Sal am \";");
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        assert!(eq_vecs(
            tokens,
            vec![
                Token {
                    ty: TokenType::Ident,
                    value: Some(String::from("x"))
                },
                Token {
                    ty: TokenType::AssignOp,
                    value: None,
                },
                Token {
                    ty: TokenType::DoubleQuoteStart,
                    value: None,
                },
                Token {
                    ty: TokenType::StringLiteral,
                    value: Some(String::from(" Sal am ")),
                },
                Token {
                    ty: TokenType::DoubleQuoteEnd,
                    value: None,
                },
                Token {
                    ty: TokenType::SemiColon,
                    value: None,
                }
            ]
        ));
    }
    #[test]
    fn test_slices() {
        let tokens = tokenize("x  =  [12, \"name\"];");
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        println!("{:?}", tokens);
        assert!(eq_vecs(
            tokens,
            vec![
                Token {
                    ty: TokenType::Ident,
                    value: Some(String::from("x"))
                },
                Token {
                    ty: TokenType::AssignOp,
                    value: None,
                },
                Token {
                    ty: TokenType::SqBracketOpen,
                    value: None,
                },
                Token {
                    ty: TokenType::Number,
                    value: Some(String::from("12"))
                },
                Token {
                    ty: TokenType::Comma,
                    value: None,
                },
                Token {
                    ty: TokenType::DoubleQuoteStart,
                    value: None,
                },
                Token {
                    ty: TokenType::StringLiteral,
                    value: Some(String::from("name")),
                },
                Token {
                    ty: TokenType::DoubleQuoteEnd,
                    value: None,
                },
                Token {
                    ty: TokenType::SqBracketClose,
                    value: None,
                },
                Token {
                    ty: TokenType::SemiColon,
                    value: None
                }
            ]
        ));
    }
}
