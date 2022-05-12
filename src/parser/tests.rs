use super::*;
// use pretty_assertions::assert_eq;

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
fn test_parse_char() {
    assert_eq!(
        _char("'c'".to_string()),
        ParseResult::Ok(("".to_string(), Node::Char('c')))
    );
}

#[test]
fn test_parse_string() {
    assert_eq!(
        string("\"amirreza\" abc".to_string()),
        ParseResult::Ok((" abc".to_string(), Node::Str("amirreza".to_string())))
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
        fn_def("fn(a: int) string {\n\tprint(a);\n\t }".to_string()),
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
                block: Node::Block(vec![Node::Application(Box::new(Application {
                    name: Node::Ident("print".to_string()),
                    args: vec![Node::Ident("a".to_string())],
                }))])
            }))
        ))
    );
}

#[test]
fn test_parse_decl_fn_2() {
    let decl_res = decl(
        "sum = fn() void {
    return 1;
};"
        .to_string(),
    );
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, f)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("sum".to_string())));
        assert_eq!(
            f,
            Box::new(Node::FnDef(Box::new(FnDef {
                ty: FnTy {
                    args: vec![],
                    return_ty: Node::VoidTy,
                },
                block: Node::Block(vec![Node::Return(Box::new(Node::Uint(1)))])
            }))),
        );
    } else {
        assert!(false);
    }
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
                    return_ty: Node::VoidTy,
                },
                block: Node::Block(vec![Node::Application(Box::new(Application {
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
            Node::Application(Box::new(Application {
                name: Node::Ident("name".to_string()),
                args: vec![],
            }))
        ))
    );
    assert_eq!(
        fn_call("name(1,2)".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::Application(Box::new(Application {
                name: Node::Ident("name".to_string()),
                args: vec![Node::Uint(1), Node::Uint(2)],
            }))
        ))
    );
    assert_eq!(
        fn_call("name(1,fn(2))".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::Application(Box::new(Application {
                name: Node::Ident("name".to_string()),
                args: vec![
                    Node::Uint(1),
                    Node::Application(Box::new(Application {
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
        _bool("truesomeshitaftertrue".to_string()),
        ParseResult::Ok(("someshitaftertrue".to_string(), Node::Bool(true)))
    );
    assert_eq!(
        _bool("falsesomeshitaftertrue".to_string()),
        ParseResult::Ok(("someshitaftertrue".to_string(), Node::Bool(false),))
    );
}
#[test]
fn test_block() {
    assert_eq!(
        block(
            "


        printf(\"salam %d\", i);
        
        
        
        "
            .to_string()
        ),
        Ok((
            "".to_string(),
            Node::Block(vec![Node::Application(Box::new(Application {
                name: Node::Ident("printf".to_string()),
                args: vec![
                    Node::Str("salam %d".to_string()),
                    Node::Ident("i".to_string())
                ],
            })),])
        ))
    )
}
#[test]
fn test_parse_for_while() {
    assert_eq!(
        _for("for i<=10 {\n\tprintf(\"salam %d\", i);\n   }".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::While(Box::new(While {
                cond: Node::Operation(Box::new(Operation {
                    lhs: Node::Ident("i".to_string()),
                    op: Operator::LesserEq,
                    rhs: Node::Uint(10)
                })),
                block: Node::Block(vec![Node::Application(Box::new(Application {
                    name: Node::Ident("printf".to_string()),
                    args: vec![
                        Node::Str("salam %d".to_string()),
                        Node::Ident("i".to_string())
                    ]
                }))])
            }))
        ))
    );
}
#[test]
fn test_parse_for_c() {
    assert_eq!(
        _for("for i:int = 0;i<=10;inc i {\n\tprintf(\"salam %d\", i);\n}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::For(Box::new(For {
                init: Node::Decl(
                    Box::new(Node::Ident("i".to_string())),
                    Box::new(Some(Node::IntTy)),
                    Box::new(Node::Uint(0))
                ),
                cond: Node::Operation(Box::new(Operation {
                    lhs: Node::Ident("i".to_string()),
                    op: Operator::LesserEq,
                    rhs: Node::Uint(10)
                })),
                cont: Node::Inc(Box::new(Node::Ident("i".to_string()))),
                body: Node::Block(vec![Node::Application(Box::new(Application {
                    name: Node::Ident("printf".to_string()),
                    args: vec![
                        Node::Str("salam %d".to_string()),
                        Node::Ident("i".to_string())
                    ]
                }))])
            }))
        ))
    );
}

#[test]
fn test_parse_import() {
    assert_eq!(
        _import("import \"c:stdio.h\" abc".to_string()),
        ParseResult::Ok((
            "abc".to_string(),
            Node::Import(Box::new(Import {
                path: Node::Str("c:stdio.h".to_string()),
                _as: None,
            }))
        ))
    );
    assert_eq!(
        _import("    import         \"c:stdio.h\"".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::Import(Box::new(Import {
                path: Node::Str("c:stdio.h".to_string()),
                _as: None,
            }))
        ))
    )
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

#[test]
fn test_parse_if() {
    assert_eq!(
        _if("if true {\n\tfn(1);\n\tfn(2);}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::If(Box::new(If {
                cond: Node::Bool(true),
                block: Node::Block(vec![
                    Node::Application(Box::new(Application {
                        name: Node::Ident("fn".to_string()),
                        args: vec![Node::Uint(1)],
                    })),
                    Node::Application(Box::new(Application {
                        name: Node::Ident("fn".to_string()),
                        args: vec![Node::Uint(2)],
                    })),
                ]),
            }))
        ))
    );
}

#[test]
fn test_parse_return() {
    assert_eq!(
        _return("return 1".to_string()),
        ParseResult::Ok(("".to_string(), Node::Return(Box::new(Node::Uint(1)))))
    )
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
        expr("'c'".to_string()),
        ParseResult::Ok(("".to_string(), Node::Char('c')))
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
            Node::Application(Box::new(Application {
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
                cond: Node::Application(Box::new(Application {
                    name: Node::Ident("cond".to_string()),
                    args: vec![Node::Bool(true)],
                })),
                block: Node::Block(vec![
                    Node::Decl(
                        Box::new(Node::Ident("a".to_string())),
                        Box::new(None),
                        Box::new(Node::Uint(1)),
                    ),
                    Node::Application(Box::new(Application {
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
                    return_ty: Node::VoidTy,
                },
                block: Node::Block(vec![Node::Application(Box::new(Application {
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
                    return_ty: Node::VoidTy,
                },
                block: Node::Block(vec![Node::Application(Box::new(Application {
                    name: Node::Ident("print".to_string()),
                    args: vec![Node::Str("salam".to_string())],
                }))])
            }))
        ))
    );
    assert_eq!(
        expr("a*2".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Ident("a".to_string()),
                op: Operator::Multiply,
                rhs: Node::Uint(2),
            }))
        ))
    );
    assert_eq!(
        expr("a%2".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Ident("a".to_string()),
                op: Operator::Mod,
                rhs: Node::Uint(2),
            }))
        ))
    );
    assert_eq!(
        expr("a/2".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Ident("a".to_string()),
                op: Operator::Div,
                rhs: Node::Uint(2),
            }))
        ))
    );
    assert_eq!(
        expr("a-2".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Ident("a".to_string()),
                op: Operator::Minus,
                rhs: Node::Uint(2),
            }))
        ))
    );
    assert_eq!(
        expr("sum(1,2)+mins(1,2,3,4)".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Application(Box::new(Application {
                    name: Node::Ident("sum".to_string()),
                    args: vec![Node::Uint(1), Node::Uint(2)]
                })),
                op: Operator::Plus,
                rhs: Node::Application(Box::new(Application {
                    name: Node::Ident("mins".to_string()),
                    args: vec![Node::Uint(1), Node::Uint(2), Node::Uint(3), Node::Uint(4)]
                })),
            }))
        ))
    );
    assert_eq!(
        expr("a+2".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Ident("a".to_string()),
                op: Operator::Plus,
                rhs: Node::Uint(2),
            }))
        ))
    );
}

#[test]
fn test_stmt_import() {
    assert_eq!(
        statement("import \"c:stdio.h\";".to_string()),
        Ok((
            "".to_string(),
            Node::Import(Box::new(Import {
                path: Node::Str("c:stdio.h".to_string()),
                _as: None,
            }))
        ))
    );
}

#[test]
fn test_parse_import_in_module() {
    assert_eq!(
        module(
            "import \"stdio.h\";
main = fn() void {
    printf(\"Hello world\");
};"
            .to_string()
        ),
        ParseResult::Ok((
            "".to_string(),
            Node::Block(vec![
                Node::Import(Box::new(Import {
                    path: Node::Str("stdio.h".to_string()),
                    _as: None,
                })),
                Node::Decl(
                    Box::new(Node::Ident("main".to_string())),
                    Box::new(None),
                    Box::new(Node::FnDef(Box::new(FnDef {
                        ty: FnTy {
                            args: vec![],
                            return_ty: Node::VoidTy,
                        },
                        block: Node::Block(vec![
                            Node::Application(Box::new(Application {
                                name: Node::Ident("printf".to_string()),
                                args: vec![Node::Str("Hello world".to_string())]
                            }))
                        ])
                    })))
                )
            ])
        ))
    );
}

#[test]
fn another_test() {
    let code = "


   i: int = 0;
   for i <= 10 {
     printf(\"salam %d\", i); 
     inc i;
   };

";
    assert_eq!(
        block(code.to_string()),
        Ok((
            "".to_string(),
            Node::Block(vec![
                Node::Decl(
                    Box::new(Node::Ident("i".to_string())),
                    Box::new(Some(Node::IntTy)),
                    Box::new(Node::Uint(0))
                ),
                Node::While(Box::new(While {
                    cond: Node::Operation(Box::new(Operation {
                        lhs: Node::Ident("i".to_string()),
                        op: Operator::LesserEq,
                        rhs: Node::Uint(10),
                    })),
                    block: Node::Block(vec![
                        Node::Application(Box::new(Application {
                            name: Node::Ident("printf".to_string()),
                            args: vec![
                                Node::Str("salam %d".to_string()),
                                Node::Ident("i".to_string())
                            ],
                        })),
                        Node::Inc(Box::new(Node::Ident("i".to_string())))
                    ])
                })),
            ]),
        ))
    )
}

#[test]
fn test_parse_module() {
    assert_eq!(
        module(
            "import \"stdio.h\";
a =2;
b =    32.2;
c   = false;
sum = fn ( a :int , b : int) int {
     return   a   + b;
};
main = fn() void {
     println ( \"Hello World\");
     return sum (1, 2);
};"
            .to_string()
        ),
        Ok((
            "".to_string(),
            Node::Block(vec![
                Node::Import(Box::new(Import {
                    path: Node::Str("stdio.h".to_string()),
                    _as: None,
                })),
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
                    Box::new(Node::Ident("sum".to_string())),
                    Box::new(None),
                    Box::new(Node::FnDef(Box::new(FnDef {
                        ty: FnTy {
                            args: vec![
                                IdentAndTy {
                                    ident: Node::Ident("a".to_string()),
                                    ty: Node::IntTy
                                },
                                IdentAndTy {
                                    ident: Node::Ident("b".to_string()),
                                    ty: Node::IntTy
                                }
                            ],
                            return_ty: Node::IntTy,
                        },
                        block: Node::Block(vec![Node::Return(Box::new(Node::Operation(
                            Box::new(Operation {
                                lhs: Node::Ident("a".to_string()),
                                op: Operator::Plus,
                                rhs: Node::Ident("b".to_string())
                            })
                        )))])
                    })))
                ),
                Node::Decl(
                    Box::new(Node::Ident("main".to_string())),
                    Box::new(None),
                    Box::new(Node::FnDef(Box::new(FnDef {
                        ty: FnTy {
                            args: vec![],
                            return_ty: Node::VoidTy,
                        },
                        block: Node::Block(vec![
                            Node::Application(Box::new(Application {
                                name: Node::Ident("println".to_string()),
                                args: vec![Node::Str("Hello World".to_string())]
                            })),
                            Node::Return(Box::new(Node::Application(Box::new(Application {
                                name: Node::Ident("sum".to_string()),
                                args: vec![Node::Uint(1), Node::Uint(2)]
                            }))))
                        ])
                    })))
                )
            ])
        ))
    );
}
