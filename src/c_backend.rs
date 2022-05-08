use crate::parser;

use super::parser::ASTNode;
use anyhow::Result;
const OUTPUT_DIR: &str = "./c_out/";
const ZIG_BUILD: &str = "const std = @import(\"std\");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable(\"main\", \"main.c\");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step(\"run\", \"Run the app\");
    run_step.dependOn(&run_cmd.step);
}";

fn boostrap_zig_build(output_dir: &str) -> Result<()> {
    std::fs::create_dir(output_dir)?;
    std::fs::write(format!("{}/build.zig", output_dir), ZIG_BUILD)?;
    Ok(())
}

fn handle_decl(name: String, ty: Box<Option<ASTNode>>, value: Box<ASTNode>) -> Result<String> {
    match *value {
        ASTNode::Fn(_, output, block) => {
            if let ASTNode::Ident(return_ty) = *output {
                if let ASTNode::List(stmts) = *block {
                    let codes:Vec<String> = vec![];
                    for stmt in stmts {
                        if let ASTNode::FnCall(name, args) = stmt {
                            if let ASTNode::Str(s) = &args[0] {
                                return Ok(format!("{}({})", name, s));
                            } else {
                                unreachable!()
                            }
                        } else {
                            unreachable!();
                        }
                    }
                } else {
                    unreachable!();
                }
            } else {
                unreachable!();
            }
        }
        _ => panic!("C backend does not support {:?}", value),
    };

    return Ok("".to_string());

}

fn handle_decls(nodes: Vec<ASTNode>) -> Result<String> {
    let mut output_code: Vec<String> = vec!["#include \"stdio.h\";".to_string()];
    for node in nodes {
        match node {
            ASTNode::Decl(name, ty, value) => {
                output_code.push(handle_decl(name, ty, value)?);
            }
            _ => panic!("C backend does not support {:?}", node),
        }
    }

    Ok(output_code.join("\n"))
}

pub fn codegen(root: ASTNode) -> Result<String> {
    boostrap_zig_build(OUTPUT_DIR)?;
    let output = match root {
        parser::ASTNode::List(decls) => {
            handle_decls(decls)?
        }
        _ => panic!("C backend does not support {:?}", root),
    };

    std::fs::write(format!("{}/main.c", OUTPUT_DIR), output)?;
    Ok("".to_string())
}
