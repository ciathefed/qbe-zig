const std = @import("std");
const Allocator = std.mem.Allocator;
const Module = @import("qbe").Module;
const Function = @import("qbe").Function;
const Linkage = @import("qbe").Linkage;
const DataDef = @import("qbe").DataDef;
const DataItem = @import("qbe").DataItem;

fn generateAddFunction(module: *Module, allocator: Allocator) !void {
    var func = Function.init(Linkage.private(), "add", &.{
        .{ .ty = .word, .val = .{ .temporary = "a" } },
        .{ .ty = .word, .val = .{ .temporary = "b" } },
    }, .word, allocator);

    _ = try func.addBlock("start");
    try func.assignInstr(
        .{ .temporary = "c" },
        .word,
        .{ .add = .{ .lhs = .{ .temporary = "a" }, .rhs = .{ .temporary = "b" } } },
    );
    try func.addInstr(.{ .ret = .{ .temporary = "c" } });

    _ = try module.addFunction(func);
}

fn generateMainFunction(module: *Module, allocator: Allocator) !void {
    var func = Function.init(Linkage.public(), "main", &.{}, .word, allocator);

    _ = try func.addBlock("start");
    try func.assignInstr(
        .{ .temporary = "r" },
        .word,
        .{ .call = .{ .name = "add", .args = &.{
            .{ .ty = .word, .val = .{ .@"const" = 1 } },
            .{ .ty = .word, .val = .{ .@"const" = 1 } },
        }, .variadic_i = null } },
    );
    try func.addInstr(.{ .call = .{
        .name = "printf",
        .args = &.{
            .{ .ty = .long, .val = .{ .global = "fmt" } },
            .{ .ty = .word, .val = .{ .temporary = "r" } },
        },
        .variadic_i = 1,
    } });
    try func.addInstr(.{ .ret = .{ .@"const" = 0 } });

    _ = try module.addFunction(func);
}

fn generateData(module: *Module) !void {
    const items: []const DataDef.Item = &.{
        .{ .ty = .byte, .item = .{ .str = "One and one make %d!\\n" } },
        .{ .ty = .byte, .item = .{ .@"const" = 0 } },
    };
    const data = DataDef.init(Linkage.private(), "fmt", null, items);
    _ = try module.addData(data);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var module = Module.init(allocator);
    defer module.deinit();

    try generateAddFunction(&module, allocator);
    try generateMainFunction(&module, allocator);
    try generateData(&module);

    std.debug.print("{}\n", .{module});
}
