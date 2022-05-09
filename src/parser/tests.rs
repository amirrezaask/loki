use super::*;

#[test]
fn test_parse_decl_bool() {
    let decl_res = decl("a = false".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("a".to_string())));
        assert_eq!(be, Box::new(Node::Bool(false)));
    } else {
        assert!(false);
    }
}

#[test]
fn test_parse_decl_int() {
    let decl_res = decl("a = -2".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("a".to_string())));
        assert_eq!(be, Box::new(Node::Int(-2)));
    } else {
        assert!(false);
    }
}

#[test]
fn test_parse_decl_str() {
    let decl_res = decl("a = \"amirreza\"".to_string());
    assert!(decl_res.is_ok());

    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("a".to_string())));
        assert_eq!(be, Box::new(Node::Str("amirreza".to_string())));
    } else {
        assert!(false);
    }
}

#[test]
fn test_parse_decl_uint() {
    let decl_res = decl("a = 2".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("a".to_string())));
        assert_eq!(be, Box::new(Node::Uint(2)));
    } else {
        assert!(false);
    }
}

#[test]
fn test_parse_single_digit() {
    assert_eq!(
        digit()("1AB".to_string()),
        ParseResult::Ok(("AB".to_string(), Node::Char('1'),))
    );
}

#[test]
fn test_parse_float() {
    assert_eq!(
        float("4.2AB".to_string()),
        ParseResult::Ok(("AB".to_string(), Node::Float(4.2)))
    );
}

#[test]
fn test_parse_string() {
    assert_eq!(
        string("\"amirreza\"".to_string()),
        ParseResult::Ok(("".to_string(), Node::Str("amirreza".to_string())))
    );
}

#[test]
fn test_parse_int() {
    assert_eq!(
        int("-1234AB".to_string()),
        ParseResult::Ok(("AB".to_string(), Node::Int(-1234)))
    );
    assert_eq!(
        int("1234AB".to_string()),
        ParseResult::Ok(("AB".to_string(), Node::Int(1234)))
    );
}

#[test]
fn test_parse_uint() {
    assert_eq!(
        uint("1234AB".to_string()),
        ParseResult::Ok(("AB".to_string(), Node::Uint(1234)))
    );
}

#[test]
fn test_parse_keyword() {
    assert_eq!(
        keyword("struct".to_string())("struct name".to_string()),
        ParseResult::Ok((" name".to_string(), Node::Keyword("struct".to_string())))
    );
}

#[test]
fn test_parse_fn_ty() {
    assert_eq!(
        fn_ty("fn(a: int) string".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnTy(Box::new(FnTy {
                args: vec![IdentAndTy {
                    ident: Node::Ident("a".to_string()),
                    ty: Node::IntTy
                }],
                return_ty: Node::StringTy,
            }))
        ))
    )
}

#[test]
fn test_parse_fn_def() {
    assert_eq!(
        fn_def("fn(a: int) string {\n\tprint(a);\n\t}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnDef(Box::new(FnDef {
                ty: FnTy {
                    args: vec![IdentAndTy {
                        ident: Node::Ident("a".to_string()),
                        ty: Node::IntTy
                    }],
                    return_ty: Node::StringTy 
                },
                block: Node::Block(vec![Node::FnCall(Box::new(FnCall {
                    name: Node::Ident("print".to_string()),
                    args: vec![Node::Ident("a".to_string())],
                }))])
            }))
        ))
    );
}

#[test]
fn test_parse_decl_fn() {
    let decl_res = decl("f = fn() void {\n\tprintln(\"Salam donya!\");\n}".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, f)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("f".to_string())));
        assert_eq!(
            f,
            Box::new(Node::FnDef(Box::new(FnDef {
                ty: FnTy {
                    args: vec![],
                    return_ty: Node::Ident("void".to_string()),
                },
                block: Node::Block(vec![Node::FnCall(Box::new(FnCall {
                    name: Node::Ident("println".to_string()),
                    args: vec![Node::Str("Salam donya!".to_string())],
                }))])
            }))),
        );
    } else {
        assert!(false);
    }
}

#[test]
fn test_parse_ident() {
    assert_eq!(
        ident("name".to_string()),
        ParseResult::Ok(("".to_string(), Node::Ident("name".to_string())))
    );
    assert_eq!(
        ident("name_str".to_string()),
        ParseResult::Ok(("".to_string(), Node::Ident("name_str".to_string()),))
    );
}

#[test]
fn test_parse_payload_string_as_ident() {
    assert_eq!(
        ident("payload".to_string()),
        ParseResult::Ok(("".to_string(), Node::Ident("payload".to_string())))
    );
}

#[test]
fn test_parse_fn_call() {
    assert_eq!(
        fn_call("name()".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnCall(Box::new(FnCall {
                name: Node::Ident("name".to_string()),
                args: vec![],
            }))
        ))
    );
    assert_eq!(
        fn_call("name(1,2)".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnCall(Box::new(FnCall {
                name: Node::Ident("name".to_string()),
                args: vec![Node::Uint(1), Node::Uint(2)],
            }))
        ))
    );
    assert_eq!(
        fn_call("name(1,fn(2))".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnCall(Box::new(FnCall {
                name: Node::Ident("name".to_string()),
                args: vec![
                    Node::Uint(1),
                    Node::FnCall(Box::new(FnCall {
                        name: Node::Ident("fn".to_string()),
                        args: vec![Node::Uint(2)],
                    })),
                ],
            }))
        ))
    );
}

#[test]
fn test_parse_bool() {
    assert_eq!(
        bool("truesomeshitaftertrue".to_string()),
        ParseResult::Ok(("someshitaftertrue".to_string(), Node::Bool(true)))
    );
    assert_eq!(
        bool("falsesomeshitaftertrue".to_string()),
        ParseResult::Ok(("someshitaftertrue".to_string(), Node::Bool(false),))
    );
}

#[test]
fn test_parse_struct() {
    assert_eq!(
        _struct("struct {\n\tname: string,\n\tage:int\n}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::StructTy(vec![
                IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy
                },
                IdentAndTy {
                    ident: Node::Ident("age".to_string()),
                    ty: Node::IntTy
                },
            ])
        ))
    );
}

#[test]
fn test_parse_decl_struct() {
    let decl_res =
        decl("s = struct {name: string, age: int, meta: struct {mature: bool}}".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("s".to_string())));
        assert_eq!(
            be,
            Box::new(Node::StructTy(vec![
                IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy,
                },
                IdentAndTy {
                    ident: Node::Ident("age".to_string()),
                    ty: Node::IntTy 
                },
                IdentAndTy {
                    ident: Node::Ident("meta".to_string()),
                    ty: Node::StructTy(vec![IdentAndTy {
                        ident: Node::Ident("mature".to_string()),
                        ty: Node::BooleanTy 
                    }])
                },
            ]))
        );
    } else {
        assert!(false);
    }
}

#[test]
fn test_parse_array_type() {
    assert_eq!(
        expr("[int]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::ArrayTy(Box::new(ArrayTy {
                size: None,
                inner_ty: Node::IntTy,
            }))
        ))
    );
    assert_eq!(
        expr("[int;2]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::ArrayTy(Box::new(ArrayTy {
                size: Some(Node::Uint(2)),
                inner_ty: Node::IntTy,
            }))
        ))
    );
}

// #[test]
// fn test_parse_for_c() {
//     assert_eq!(
//         _if("for i=0;i<10;i++ { print(i); }".to_string()),
//         ParseResult::Ok((
//             "".to_string(),
//             ParseObj::ForC(
//                 Box::new(ParseObj::Decl(ParseObj::Ident("i".to_string()), Box::new(None), Box::new(ParseObj::Uint(0)))),
//                 )
//             )
//         ));
// }
#[test]
fn test_parse_if() {
    assert_eq!(
        _if("if true {\n\tfn(1);\n\tfn(2);}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::If(Box::new(If {
                cond: Node::Bool(true),
                block: Node::Block(vec![
                    Node::FnCall(Box::new(FnCall {
                        name: Node::Ident("fn".to_string()),
                        args: vec![Node::Uint(1)],
                    })),
                    Node::FnCall(Box::new(FnCall {
                        name: Node::Ident("fn".to_string()),
                        args: vec![Node::Uint(2)],
                    })),
                ]),
            }))
        ))
    );
}

#[test]
fn test_parse_expr() {
    assert_eq!(
        expr("true".to_string()),
        ParseResult::Ok(("".to_string(), Node::Bool(true)))
    );
    assert_eq!(
        expr("false".to_string()),
        ParseResult::Ok(("".to_string(), Node::Bool(false)))
    );
    assert_eq!(
        expr("12".to_string()),
        ParseResult::Ok(("".to_string(), Node::Uint(12)))
    );
    assert_eq!(
        expr("-12".to_string()),
        ParseResult::Ok(("".to_string(), Node::Int(-12)))
    );
    assert_eq!(
        expr("12.2".to_string()),
        ParseResult::Ok(("".to_string(), Node::Float(12.2)))
    );
    assert_eq!(
        expr("-12.2".to_string()),
        ParseResult::Ok(("".to_string(), Node::Float(-12.2)))
    );
    assert_eq!(
        expr("name".to_string()),
        ParseResult::Ok(("".to_string(), Node::Ident("name".to_string())))
    );
    assert_eq!(
        expr("\"name\"".to_string()),
        ParseResult::Ok(("".to_string(), Node::Str("name".to_string())))
    );
    assert_eq!(
        expr("struct {\n\tname: string,\n\tupdated_at: date}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::StructTy(vec![
                IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy 
                },
                IdentAndTy {
                    ident: Node::Ident("updated_at".to_string()),
                    ty: Node::Ident("date".to_string())
                },
            ])
        ))
    );

    assert_eq!(
        expr("struct {\n\tname: string,\n\tpayload: struct {created_at: date}}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::StructTy(vec![
                IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy 
                },
                IdentAndTy {
                    ident: Node::Ident("payload".to_string()),
                    ty: Node::StructTy(vec![IdentAndTy {
                        ident: Node::Ident("created_at".to_string()),
                        ty: Node::Ident("date".to_string())
                    }])
                },
            ])
        ))
    );
    assert_eq!(
        expr("fn_call(1,2)".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnCall(Box::new(FnCall {
                name: Node::Ident("fn_call".to_string()),
                args: vec![Node::Uint(1), Node::Uint(2)],
            }))
        ))
    );
    assert_eq!(
        expr("[struct {name: string}]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::ArrayTy(Box::new(ArrayTy {
                size: None,
                inner_ty: Node::StructTy(vec![IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy
                }]),
            }))
        ))
    );
    assert_eq!(
        expr("[struct {name: string};2]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::ArrayTy(Box::new(ArrayTy {
                size: Some(Node::Uint(2)),
                inner_ty: Node::StructTy(vec![IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy
                }]),
            }))
        ))
    );
    assert_eq!(
        expr("if cond(true) {\n\ta = 1;fn(a);\n}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::If(Box::new(If {
                cond: Node::FnCall(Box::new(FnCall {
                    name: Node::Ident("cond".to_string()),
                    args: vec![Node::Bool(true)],
                })),
                block: Node::Block(vec![
                    Node::Decl(
                        Box::new(Node::Ident("a".to_string())),
                        Box::new(None),
                        Box::new(Node::Uint(1)),
                    ),
                    Node::FnCall(Box::new(FnCall {
                        name: Node::Ident("fn".to_string()),
                        args: vec![Node::Ident("a".to_string())],
                    })),
                ]),
            }))
        ))
    );
    assert_eq!(
        expr("fn() void {\n\t print(\"salam\");\n\t}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnDef(Box::new(FnDef {
                ty: FnTy {
                    args: vec![],
                    return_ty: Node::Ident("void".to_string()),
                },
                block: Node::Block(vec![Node::FnCall(Box::new(FnCall {
                    name: Node::Ident("print".to_string()),
                    args: vec![Node::Str("salam".to_string())],
                }))])
            }))
        ))
    );
    assert_eq!(
        expr("fn(a: struct { b: string }) void {\n\t print(\"salam\");\n\t}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnDef(Box::new(FnDef {
                ty: FnTy {
                    args: vec![IdentAndTy {
                        ident: Node::Ident("a".to_string()),
                        ty: Node::StructTy(vec![IdentAndTy {
                            ident: Node::Ident("b".to_string()),
                            ty: Node::StringTy
                        }]),
                    }],
                    return_ty: Node::Ident("void".to_string()),
                },
                block: Node::Block(vec![Node::FnCall(Box::new(FnCall {
                    name: Node::Ident("print".to_string()),
                    args: vec![Node::Str("salam".to_string())],
                }))])
            }))
        ))
    );
}

#[test]
fn test_parse_module() {
    assert_eq!(
        module(
"
a = 2;
b = 32.2;
c = false;
main = fn() void {
    println(\"Hello World\");
};"

.to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::List(vec![
                Node::Decl(
                    Box::new(Node::Ident("a".to_string())),
                    Box::new(None),
                    Box::new(Node::Uint(2)),
                ),
                Node::Decl(
                    Box::new(Node::Ident("b".to_string())),
                    Box::new(None),
                    Box::new(Node::Float(32.2)),
                ),
                Node::Decl(
                    Box::new(Node::Ident("c".to_string())),
                    Box::new(None),
                    Box::new(Node::Bool(false)),
                ),
                Node::Decl(
                    Box::new(Node::Ident("main".to_string())),
                    Box::new(None),
                    Box::new(Node::FnDef(Box::new(FnDef {
                        ty: FnTy {
                            args: vec![],
                            return_ty: Node::Ident("void".to_string()),
                        },
                        block: Node::Block(vec![Node::FnCall(Box::new(FnCall {
                            name: Node::Ident("println".to_string()),
                            args: vec![Node::Str("Hello World".to_string())]
                        }))])
                    })))
            )])
        ))
    );
}
