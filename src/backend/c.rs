use crate::parser;
use super::CodeGen;
use crate::parser::Node;
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

pub struct C {
    arch: String,
    os: String,
}

fn boostrap_zig_build(output_dir: &str) -> Result<()> {
    std::fs::create_dir(output_dir)?;
    std::fs::write(format!("{}/build.zig", output_dir), ZIG_BUILD)?;
    Ok(())
}

pub fn codegen(root: Node) -> Result<String> {
    boostrap_zig_build(OUTPUT_DIR)?;
    // std::fs::write(format!("{}/main.c", OUTPUT_DIR), output)?;
    Ok("".to_string())
}
