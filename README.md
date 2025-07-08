# qbe-zig

A Zig port of [qbe-rs](https://github.com/garritfra/qbe-rs), a library for generating QBE IR programmatically.

This library allows you to build QBE IR in pure Zig, suitable for backends, compilers, or tooling that targets [QBE](https://c9x.me/compile/).

![Static Badge](https://img.shields.io/badge/Zig-0.14.1-ec915c?style=flat-square&logo=zig)
![Tests](https://img.shields.io/github/actions/workflow/status/ciathefed/qbe-zig/zig.yml?label=Tests%20%F0%9F%A7%AA&style=flat-square)

## Install

Run this command in the root of your Zig project:

```shell
zig fetch --save "git+https://github.com/ciathefed/qbe-zig"
```

Add this snippet to your `build.zig`:

```zig
const qbe = b.dependency("qbe", .{
    .target = target,
    .optimize = optimize,
});
exe_mod.addImport("qbe", qbe.module("qbe"));
```

## API Reference

For the full Zig-style API reference, visit:

https://ciathefed.github.io/qbe-zig

## License

This project is licensed under the [MIT License](./LICENSE)
