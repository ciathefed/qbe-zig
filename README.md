# qbe-zig

A Zig port of [qbe-rs](https://github.com/garritfra/qbe-rs), a library for generating QBE IR programmatically.

This library allows you to build QBE IR in pure Zig, suitable for backends, compilers, or tooling that targets [QBE](https://c9x.me/compile/).

![Static Badge](https://img.shields.io/badge/Zig-0.15.1-ec915c?style=flat-square&logo=zig)
![Tests](https://img.shields.io/github/actions/workflow/status/ciathefed/qbe-zig/zig.yml?label=Tests%20%F0%9F%A7%AA&style=flat-square)

## Install

Run this command in the root of your Zig project:

### Latest Release (recommended)

```shell
zig fetch --save "git+https://github.com/ciathefed/qbe-zig#v0.1.1"
````

### Latest on `main` (bleeding edge)

```shell
zig fetch --save "git+https://github.com/ciathefed/qbe-zig#main"
```

Add `qbe-zig` to your imports in `build.zig`:

```zig
const qbe = b.dependency("qbe", .{
    .target = target,
    .optimize = optimize,
});

const exe = b.addExecutable(.{
    // ...
    .root_module = b.createModule(.{
        // ...
        .imports = &.{
            .{ .name = "qbe", .module = qbe.module("qbe") },
        },
    }),
});
```

## API Reference

For the full Zig-style API reference, visit:

https://ciathefed.github.io/qbe-zig

## License

This project is licensed under the [MIT License](./LICENSE)
