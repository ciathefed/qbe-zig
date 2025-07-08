const std = @import("std");
const ArrayList = std.ArrayList;
const testing = std.testing;
const Value = @import("root.zig").Value;
const Block = @import("root.zig").Block;
const BlockItem = @import("root.zig").BlockItem;
const Function = @import("root.zig").Function;
const Linkage = @import("root.zig").Linkage;
const DataDef = @import("root.zig").DataDef;
const TypeDef = @import("root.zig").TypeDef;
const Type = @import("root.zig").Type;
const Module = @import("root.zig").Module;
const Instr = @import("root.zig").Instr;
const Statement = @import("root.zig").Statement;
const DataItem = @import("root.zig").DataItem;

fn testFmtEquals(T: type, value: T, expected: []const u8) !void {
    const string = try std.fmt.allocPrint(testing.allocator, "{}", .{value});
    defer testing.allocator.free(string);
    try testing.expectEqualSlices(u8, expected, string);
}

test "qbe value" {
    try testFmtEquals(Value, .{ .temporary = "temp42" }, "%temp42");
    try testFmtEquals(Value, .{ .global = "main" }, "$main");
    try testFmtEquals(Value, .{ .@"const" = 1337 }, "1337");
}

test "block" {
    var blk1 = Block.init("start", testing.allocator);
    defer blk1.deinit();
    try blk1.addInstr(.{ .ret = null });
    try testFmtEquals(Block, blk1, "@start\n\tret\n");

    var blk2 = Block.init("start", testing.allocator);
    defer blk2.deinit();
    try blk2.addComment("Comment");
    try blk2.assignInstr(.{ .temporary = "foo" }, .word, .{ .add = .{ .lhs = .{ .@"const" = 2 }, .rhs = .{ .@"const" = 2 } } });
    try blk2.addInstr(.{ .ret = .{ .temporary = "foo" } });
    try testFmtEquals(Block, blk2, "@start\n\t# Comment\n\t%foo =w add 2, 2\n\tret %foo\n");
}

test "instr blit" {
    var blk = Block.init("start", testing.allocator);
    defer blk.deinit();
    try blk.addInstr(.{ .blit = .{ .src = .{ .temporary = "src" }, .dest = .{ .temporary = "dest" }, .n = 4 } });
    try testFmtEquals(Block, blk, "@start\n\tblit %src, %dest, 4\n");
}

test "function" {
    var func = Function.init(Linkage.public(), "main", &.{}, null, testing.allocator);
    defer func.deinit();
    var block = try func.addBlock("start");
    try block.addInstr(.{ .ret = null });
    try testFmtEquals(Function, func, "export function $main() {\n@start\n\tret\n}");
}

test "data def" {
    const data_def = DataDef.init(Linkage.public(), "hello", null, &.{
        .{ .ty = .byte, .item = .{ .str = "Hello, World!" } },
        .{ .ty = .byte, .item = .{ .@"const" = 0 } },
    });
    try testFmtEquals(DataDef, data_def, "export data $hello = { b \"Hello, World!\", b 0 }");
}

test "type def" {
    const type_def = TypeDef.init("person", null, &.{
        .{ .ty = .long, .repeat = 1 },
        .{ .ty = .word, .repeat = 2 },
        .{ .ty = .byte, .repeat = 1 },
    });
    try testFmtEquals(TypeDef, type_def, "type :person = { l, w 2, b }");
    try testFmtEquals(Type, .{ .aggregate = &type_def }, ":person");
}

test "type size" {
    try testing.expectEqual(Type.size(.byte), 1);
    try testing.expectEqual(Type.size(.signed_byte), 1);
    try testing.expectEqual(Type.size(.unsigned_byte), 1);
    try testing.expectEqual(Type.size(.halfword), 2);
    try testing.expectEqual(Type.size(.signed_halfword), 2);
    try testing.expectEqual(Type.size(.unsigned_halfword), 2);
    try testing.expectEqual(Type.size(.word), 4);
    try testing.expectEqual(Type.size(.single), 4);
    try testing.expectEqual(Type.size(.long), 8);
    try testing.expectEqual(Type.size(.double), 8);

    const type_def = TypeDef.init("person", null, &.{
        .{ .ty = .long, .repeat = 1 },
        .{ .ty = .word, .repeat = 2 },
        .{ .ty = .byte, .repeat = 1 },
    });
    const aggregate: Type = .{ .aggregate = &type_def };
    try testing.expectEqual(aggregate.size(), 24);
}

test "type size nested aggregate" {
    const inner = TypeDef.init("dog", null, &.{
        .{ .ty = .long, .repeat = 2 },
    });
    const inner_aggregate: Type = .{ .aggregate = &inner };
    try testing.expectEqual(inner_aggregate.size(), 16);

    const type_def = TypeDef.init("person", null, &.{
        .{ .ty = .long, .repeat = 1 },
        .{ .ty = .word, .repeat = 2 },
        .{ .ty = .byte, .repeat = 1 },
        .{ .ty = .{ .aggregate = &inner }, .repeat = 1 },
    });
    const aggregate: Type = .{ .aggregate = &type_def };
    try testing.expectEqual(aggregate.size(), 40);
}

test "type into abi" {
    const unchanged = struct {
        fn func(ty: Type) !void {
            try testing.expectEqual(ty.intoAbi(), ty);
        }
    }.func;

    try unchanged(.word);
    try unchanged(.long);
    try unchanged(.single);
    try unchanged(.double);
    const type_def = TypeDef.init("foo", null, &.{});
    try unchanged(.{ .aggregate = &type_def });

    try testing.expectEqual(Type.intoAbi(.byte), Type.word);
    try testing.expectEqual(Type.intoAbi(.unsigned_byte), Type.word);
    try testing.expectEqual(Type.intoAbi(.signed_byte), Type.word);
    try testing.expectEqual(Type.intoAbi(.halfword), Type.word);
    try testing.expectEqual(Type.intoAbi(.unsigned_halfword), Type.word);
    try testing.expectEqual(Type.intoAbi(.signed_halfword), Type.word);
}

test "type into base" {
    const unchanged = struct {
        fn func(ty: Type) !void {
            try testing.expectEqual(ty.intoBase(), ty);
        }
    }.func;

    try unchanged(.word);
    try unchanged(.long);
    try unchanged(.single);
    try unchanged(.double);

    try testing.expectEqual(Type.intoBase(.byte), Type.word);
    try testing.expectEqual(Type.intoBase(.unsigned_byte), Type.word);
    try testing.expectEqual(Type.intoBase(.signed_halfword), Type.word);
    try testing.expectEqual(Type.intoBase(.halfword), Type.word);
    try testing.expectEqual(Type.intoBase(.unsigned_halfword), Type.word);
    try testing.expectEqual(Type.intoBase(.signed_halfword), Type.word);
    const type_def = TypeDef.init("foo", null, &.{});
    try testing.expectEqual(Type.intoBase(.{ .aggregate = &type_def }), Type.long);
}

test "add function to module" {
    var module = Module.init(testing.allocator);
    defer module.deinit();

    const function = Function.init(Linkage.private(), "foo", &.{}, null, testing.allocator);

    _ = try module.addFunction(function);

    try testing.expectEqual(module.functions.items[0], function);
}

test "variadic call" {
    const instr = Instr{ .call = .{
        .name = "printf",
        .args = &.{
            .{ .ty = .long, .val = .{ .global = "fmt" } },
            .{ .ty = .word, .val = .{ .@"const" = 0 } },
        },
        .variadic_i = 1,
    } };
    try testFmtEquals(Instr, instr, "call $printf(l $fmt, ..., w 0)");
}

test "module fmt order" {
    var module = Module.init(testing.allocator);
    defer module.deinit();

    const type_def = TypeDef.init("test_type", null, &.{});
    _ = try module.addType(type_def);

    var func = Function.init(Linkage.public(), "test_func", &.{}, null, testing.allocator);

    var block = try func.addBlock("entry");
    try block.addInstr(.{ .ret = null });

    _ = try module.addFunction(func);

    const data = DataDef.init(Linkage.private(), "test_data", null, &.{
        .{ .ty = .word, .item = .{ .@"const" = 42 } },
    });
    _ = try module.addData(data);

    const formatted = try std.fmt.allocPrint(testing.allocator, "{}", .{module});
    defer testing.allocator.free(formatted);

    const type_pos = std.mem.indexOf(u8, formatted, ":test_type") orelse {
        @panic("Type definition not found");
    };

    const func_pos = std.mem.indexOf(u8, formatted, "export function $test_func") orelse {
        @panic("Function not found");
    };

    const data_pos = std.mem.indexOf(u8, formatted, "data $test_data") orelse {
        @panic("Data definition not found");
    };

    try testing.expect(type_pos < func_pos);
    try testing.expect(func_pos < data_pos);
}

test "comparison types" {
    const ordered_cmp: Statement = .{ .@"volatile" = .{ .cmp = .{
        .ty = .double,
        .cmp = .o,
        .lhs = .{ .temporary = "a" },
        .rhs = .{ .temporary = "b" },
    } } };
    try testFmtEquals(Statement, ordered_cmp, "cod %a, %b");

    const unordered_cmp: Statement = .{ .@"volatile" = .{ .cmp = .{
        .ty = .single,
        .cmp = .uo,
        .lhs = .{ .temporary = "a" },
        .rhs = .{ .temporary = "b" },
    } } };
    try testFmtEquals(Statement, unordered_cmp, "cuos %a, %b");

    const unsigned_lt: Statement = .{ .@"volatile" = .{ .cmp = .{
        .ty = .word,
        .cmp = .ult,
        .lhs = .{ .temporary = "a" },
        .rhs = .{ .temporary = "b" },
    } } };
    try testFmtEquals(Statement, unsigned_lt, "cultw %a, %b");

    const unsigned_le: Statement = .{ .@"volatile" = .{ .cmp = .{
        .ty = .long,
        .cmp = .ule,
        .lhs = .{ .temporary = "a" },
        .rhs = .{ .temporary = "b" },
    } } };
    try testFmtEquals(Statement, unsigned_le, "culel %a, %b");

    const unsigned_gt: Statement = .{ .@"volatile" = .{ .cmp = .{
        .ty = .word,
        .cmp = .ugt,
        .lhs = .{ .temporary = "a" },
        .rhs = .{ .temporary = "b" },
    } } };
    try testFmtEquals(Statement, unsigned_gt, "cugtw %a, %b");

    const unsigned_ge: Statement = .{ .@"volatile" = .{ .cmp = .{
        .ty = .long,
        .cmp = .uge,
        .lhs = .{ .temporary = "a" },
        .rhs = .{ .temporary = "b" },
    } } };
    try testFmtEquals(Statement, unsigned_ge, "cugel %a, %b");
}

test "unsigned arithmetic instructions" {
    const udiv: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .udiv = .{ .lhs = .{ .temporary = "a" }, .rhs = .{ .temporary = "b" } } },
    } };
    try testFmtEquals(Statement, udiv, "%result =w udiv %a, %b");

    const urem: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .long,
        .instr = .{ .urem = .{ .lhs = .{ .temporary = "a" }, .rhs = .{ .temporary = "b" } } },
    } };
    try testFmtEquals(Statement, urem, "%result =l urem %a, %b");
}

test "shift instructions" {
    const sar: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .sar = .{ .lhs = .{ .temporary = "a" }, .rhs = .{ .temporary = "b" } } },
    } };
    try testFmtEquals(Statement, sar, "%result =w sar %a, %b");

    const shr: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .long,
        .instr = .{ .shr = .{ .lhs = .{ .temporary = "a" }, .rhs = .{ .temporary = "b" } } },
    } };
    try testFmtEquals(Statement, shr, "%result =l shr %a, %b");

    const shl: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .shl = .{ .lhs = .{ .temporary = "a" }, .rhs = .{ .temporary = "b" } } },
    } };
    try testFmtEquals(Statement, shl, "%result =w shl %a, %b");
}

test "cast instruction" {
    const cast_int_to_float: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .single,
        .instr = .{ .cast = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, cast_int_to_float, "%result =s cast %a");

    const cast_float_to_int: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .cast = .{ .temporary = "f" } },
    } };
    try testFmtEquals(Statement, cast_float_to_int, "%result =w cast %f");
}

test "extension operations" {
    const extsw: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .long,
        .instr = .{ .extsw = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, extsw, "%result =l extsw %a");

    const extuw: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .long,
        .instr = .{ .extuw = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, extuw, "%result =l extuw %a");

    const extsh: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .extsh = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, extsh, "%result =w extsh %a");

    const extuh: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .extuh = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, extuh, "%result =w extuh %a");

    const extsb: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .extsb = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, extsb, "%result =w extsb %a");

    const extub: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .extub = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, extub, "%result =w extub %a");
}

test "float precision conversion" {
    const exts: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .double,
        .instr = .{ .exts = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, exts, "%result =d exts %a");

    const truncd: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .single,
        .instr = .{ .truncd = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, truncd, "%result =s truncd %a");
}

test "float integer conversions" {
    const stosi: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .stosi = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, stosi, "%result =w stosi %a");

    const stoui: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .stoui = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, stoui, "%result =w stoui %a");

    const dtosi: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .long,
        .instr = .{ .dtosi = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, dtosi, "%result =l dtosi %a");

    const dtoui: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .long,
        .instr = .{ .dtoui = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, dtoui, "%result =l dtoui %a");

    const swtof: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .single,
        .instr = .{ .swtof = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, swtof, "%result =s swtof %a");

    const uwtof: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .single,
        .instr = .{ .uwtof = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, uwtof, "%result =s uwtof %a");

    const sltof: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .double,
        .instr = .{ .sltof = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, sltof, "%result =d sltof %a");

    const ultof: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .double,
        .instr = .{ .ultof = .{ .temporary = "a" } },
    } };
    try testFmtEquals(Statement, ultof, "%result =d ultof %a");
}

test "variadic instructions" {
    const vastart: Statement = .{ .@"volatile" = .{ .vastart = .{ .temporary = "ap" } } };
    try testFmtEquals(Statement, vastart, "vastart %ap");

    const vaarg: Statement = .{ .assign = .{
        .temp = .{ .temporary = "arg" },
        .ty = .word,
        .instr = .{ .vaarg = .{ .ty = .word, .val = .{ .temporary = "ap" } } },
    } };
    try testFmtEquals(Statement, vaarg, "%arg =w vaargw %ap");
}

test "phi instruction" {
    const phi1: Instr = .{ .phi = .{
        .label1 = "ift",
        .val1 = .{ .@"const" = 2 },
        .label2 = "iff",
        .val2 = .{ .temporary = "3" },
    } };
    try testFmtEquals(Instr, phi1, "phi @ift 2, @iff %3");

    const phi2: Statement = .{ .assign = .{
        .temp = .{ .temporary = "result" },
        .ty = .word,
        .instr = .{ .phi = .{
            .label1 = "start",
            .val1 = .{ .temporary = "1" },
            .label2 = "loop",
            .val2 = .{ .global = "tmp" },
        } },
    } };
    try testFmtEquals(Statement, phi2, "%result =w phi @start %1, @loop $tmp");
}

test "halt instruction" {
    const hlt: Statement = .{ .@"volatile" = .hlt };
    try testFmtEquals(Statement, hlt, "hlt");
}

test "thread local linkage" {
    const thread_local = Linkage.threadLocal();
    try testFmtEquals(Linkage, thread_local, "thread ");

    const exported_thread_local = Linkage.exportedThreadLocal();
    try testFmtEquals(Linkage, exported_thread_local, "export thread ");

    const thread_local_with_section = Linkage.threadLocalWithSection("data");
    try testFmtEquals(Linkage, thread_local_with_section, "thread section \"data\" ");

    const data_def = DataDef.init(Linkage.threadLocal(), "thread_var", null, &.{
        .{ .ty = .word, .item = .{ .@"const" = 42 } },
    });
    try testFmtEquals(DataDef, data_def, "thread data $thread_var = { w 42 }");
}

test "zero initialized data" {
    const zero_data: DataItem = .{ .zero = 1000 };
    try testFmtEquals(DataItem, zero_data, "z 1000");

    const data_def1 = DataDef.init(Linkage.private(), "zero_array", null, &.{
        .{ .ty = .byte, .item = .{ .zero = 1000 } },
    });
    try testFmtEquals(DataDef, data_def1, "data $zero_array = { b z 1000 }");

    const data_def2 = DataDef.init(Linkage.private(), "mixed_data", null, &.{
        .{ .ty = .word, .item = .{ .@"const" = 1 } },
        .{ .ty = .byte, .item = .{ .zero = 10 } },
        .{ .ty = .word, .item = .{ .@"const" = 2 } },
    });
    try testFmtEquals(DataDef, data_def2, "data $mixed_data = { w 1, b z 10, w 2 }");
}

test "complex block with multiple instructions" {
    var block = Block.init("test_block", testing.allocator);
    defer block.deinit();

    try block.assignInstr(
        .{ .temporary = "udiv_result" },
        .word,
        .{ .udiv = .{ .lhs = .{ .temporary = "a" }, .rhs = .{ .temporary = "b" } } },
    );

    try block.assignInstr(
        .{ .temporary = "shift_result" },
        .word,
        .{ .shl = .{ .lhs = .{ .temporary = "a" }, .rhs = .{ .temporary = "b" } } },
    );

    try block.assignInstr(
        .{ .temporary = "cast_result" },
        .single,
        .{ .cast = .{ .temporary = "shift_result" } },
    );

    try block.assignInstr(
        .{ .temporary = "cmp_result" },
        .word,
        .{ .cmp = .{
            .ty = .single,
            .cmp = .uo,
            .lhs = .{ .temporary = "cast_result" },
            .rhs = .{ .temporary = "x" },
        } },
    );

    try block.addInstr(.hlt);

    try testFmtEquals(Block, block, "@test_block\n\t%udiv_result =w udiv %a, %b\n\t%shift_result =w shl %a, %b\n\t%cast_result =s cast %shift_result\n\t%cmp_result =w cuos %cast_result, %x\n\thlt\n");
}
