const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Argument = struct {
    ty: Type,
    val: Value,
};

/// QBE comparison operations used in conditional instructions.
pub const Cmp = enum {
    /// Returns 1 if first value is less than second, respecting signedness
    slt,
    /// Returns 1 if first value is less than or equal to second, respecting signedness
    sle,
    /// Returns 1 if first value is greater than second, respecting signedness
    sgt,
    /// Returns 1 if first value is greater than or equal to second, respecting signedness
    sge,
    /// Returns 1 if values are equal
    eq,
    /// Returns 1 if values are not equal
    ne,
    /// Returns 1 if both operands are not NaN (ordered comparison)
    o,
    /// Returns 1 if at least one operand is NaN (unordered comparison)
    uo,
    /// Returns 1 if first value is less than second, unsigned comparison
    ult,
    /// Returns 1 if first value is less than or equal to second, unsigned comparison
    ule,
    /// Returns 1 if first value is greater than second, unsigned comparison
    ugt,
    /// Returns 1 if first value is greater than or equal to second, unsigned comparison
    uge,

    pub fn toString(self: Cmp) []const u8 {
        return switch (self) {
            .slt => "slt",
            .sle => "sle",
            .sgt => "sgt",
            .sge => "sge",
            .eq => "eq",
            .ne => "ne",
            .o => "o",
            .uo => "uo",
            .ult => "ult",
            .ule => "ule",
            .ugt => "ugt",
            .uge => "uge",
        };
    }
};

/// QBE instructions representing operations in the intermediate language
pub const Instr = union(enum) {
    /// Adds values of two temporaries together
    add: BinaryOp,
    /// Subtracts the second value from the first one
    sub: BinaryOp,
    /// Multiplies values of two temporaries
    mul: BinaryOp,
    /// Divides the first value by the second one
    div: BinaryOp,
    /// Returns a remainder from division
    rem: BinaryOp,
    /// Performs a comparion between values
    cmp: Compare,
    /// Performs a bitwise AND on values
    and_op: BinaryOp,
    /// Performs a bitwise OR on values
    or_op: BinaryOp,
    /// Copies either a temporary or a literal value
    copy: Value,
    /// Return from a function, optionally with a value
    ret: ?Value,
    /// Jumps to first label if a value is nonzero or to the second one otherwise
    jnz: JumpNotZero,
    /// Unconditionally jumps to a label
    jmp: []const u8,
    /// Calls a function
    call: Call,
    /// Allocates a 4-byte aligned area on the stack
    alloc4: u32,
    /// Allocates a 8-byte aligned area on the stack
    alloc8: u64,
    /// Allocates a 16-byte aligned area on the stack
    alloc16: u128,
    /// Stores a value into memory pointed to by destination.
    store: Store,
    /// Loads a value from memory pointed to by source
    load: Load,
    /// Copy `n` bytes from the source address to the destination address
    blit: Blit,

    /// Debug file
    dbg_file: []const u8,
    /// Debug line
    dbg_loc: DbgLoc,

    /// Performs unsigned division of the first value by the second one
    udiv: BinaryOp,
    /// Returns the remainder from unsigned division
    urem: BinaryOp,

    /// Shift arithmetic right (preserves sign)
    sar: BinaryOp,
    /// Shift logical right (fills with zeros)
    shr: BinaryOp,
    /// Shift left (fills with zeros)
    shl: BinaryOp,

    /// Cast between integer and floating point of the same width
    cast: Value,

    /// Sign-extends a word to a long
    extsw: Value,
    /// Zero-extends a word to a long
    extuw: Value,
    /// Sign-extends a halfword to a word or long
    extsh: Value,
    /// Zero-extends a halfword to a word or long
    extuh: Value,
    /// Sign-extends a byte to a word or long
    extsb: Value,
    /// Zero-extends a byte to a word or long
    extub: Value,
    /// Extends a single-precision float to double-precision
    exts: Value,
    /// Truncates a double-precision float to single-precision
    truncd: Value,

    /// Converts a single-precision float to a signed integer
    stosi: Value,
    /// Converts a single-precision float to an unsigned integer
    stoui: Value,
    /// Converts a double-precision float to a signed integer
    dtosi: Value,
    /// Converts a double-precision float to an unsigned integer
    dtoui: Value,
    /// Converts a signed word to a float
    swtof: Value,
    /// Converts an unsigned word to a float
    uwtof: Value,
    /// Converts a signed long to a float
    sltof: Value,
    /// Converts an unsigned long to a float
    ultof: Value,

    /// Initializes a variable argument list
    vastart: Value,
    /// Fetches the next argument from a variable argument list
    vaarg: Vaarg,

    /// Selects value based on the control flow path into a block.
    phi: Phi,

    /// Terminates the program with an error
    hlt,

    pub const BinaryOp = struct { lhs: Value, rhs: Value };
    pub const Compare = struct { ty: Type, cmp: Cmp, lhs: Value, rhs: Value };
    pub const JumpNotZero = struct { cond: Value, non_zero: []const u8, zero: []const u8 };
    pub const Call = struct { name: []const u8, args: []const Argument, variadic_i: ?u64 };
    pub const Store = struct { ty: Type, dest: Value, src: Value };
    pub const Load = struct { ty: Type, src: Value };
    pub const Blit = struct { src: Value, dest: Value, n: u64 };
    pub const DbgLoc = struct { line: u64, column: ?u64 };
    pub const Vaarg = struct { ty: Type, val: Value };
    pub const Phi = struct { label1: []const u8, val1: Value, label2: []const u8, val2: Value };

    pub fn format(
        self: Instr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .add => |i| try writer.print("add {}, {}", .{ i.lhs, i.rhs }),
            .sub => |i| try writer.print("sub {}, {}", .{ i.lhs, i.rhs }),
            .mul => |i| try writer.print("mul {}, {}", .{ i.lhs, i.rhs }),
            .div => |i| try writer.print("div {}, {}", .{ i.lhs, i.rhs }),
            .rem => |i| try writer.print("rem {}, {}", .{ i.lhs, i.rhs }),
            .cmp => |i| {
                if (i.ty == .aggregate) {
                    @panic("Cannot compare aggregate types");
                }
                try writer.print("c{s}{} {}, {}", .{ i.cmp.toString(), i.ty, i.lhs, i.rhs });
            },
            .and_op => |i| try writer.print("and {}, {}", .{ i.lhs, i.rhs }),
            .or_op => |i| try writer.print("or {}, {}", .{ i.lhs, i.rhs }),
            .copy => |i| try writer.print("copy {}", .{i}),
            .ret => |i| {
                if (i) |val| {
                    try writer.print("ret {}", .{val});
                } else {
                    try writer.writeAll("ret");
                }
            },
            .dbg_file => |i| try writer.print("dbgfile \"{s}\"", .{i}),
            .dbg_loc => |i| {
                if (i.column) |col| {
                    try writer.print("dbgloc {}, {}", .{ i.line, col });
                } else {
                    try writer.print("dbgloc {}", .{i.line});
                }
            },
            .jnz => |i| try writer.print("jnz {}, @{s}, @{s}", .{ i.cond, i.non_zero, i.zero }),
            .jmp => |i| try writer.print("jmp @{s}", .{i}),
            .call => |i| {
                try writer.print("call ${s}(", .{i.name});
                for (i.args, 0..) |arg, idx| {
                    if (idx > 0) try writer.writeAll(", ");
                    if (i.variadic_i) |vi| {
                        if (idx == vi) {
                            try writer.writeAll("..., ");
                        }
                    }
                    try writer.print("{} {}", .{ arg.ty, arg.val });
                }
                try writer.writeAll(")");
            },
            .alloc4 => |i| try writer.print("alloc4 {}", .{i}),
            .alloc8 => |i| try writer.print("alloc8 {}", .{i}),
            .alloc16 => |i| try writer.print("alloc16 {}", .{i}),
            .store => |i| {
                if (i.ty == .aggregate) {
                    @panic("Store to an aggregate type not implemented");
                }
                try writer.print("store{} {}, {}", .{ i.ty, i.src, i.dest });
            },
            .load => |i| {
                if (i.ty == .aggregate) {
                    @panic("Load aggregate type not implemented");
                }
                try writer.print("load{} {}", .{ i.ty, i.src });
            },
            .blit => |i| try writer.print("blit {}, {}, {}", .{ i.src, i.dest, i.n }),
            .udiv => |i| try writer.print("udiv {}, {}", .{ i.lhs, i.rhs }),
            .urem => |i| try writer.print("urem {}, {}", .{ i.lhs, i.rhs }),
            .sar => |i| try writer.print("sar {}, {}", .{ i.lhs, i.rhs }),
            .shr => |i| try writer.print("shr {}, {}", .{ i.lhs, i.rhs }),
            .shl => |i| try writer.print("shl {}, {}", .{ i.lhs, i.rhs }),
            .cast => |i| try writer.print("cast {}", .{i}),
            .extsw => |i| try writer.print("extsw {}", .{i}),
            .extuw => |i| try writer.print("extuw {}", .{i}),
            .extsh => |i| try writer.print("extsh {}", .{i}),
            .extuh => |i| try writer.print("extuh {}", .{i}),
            .extsb => |i| try writer.print("extsb {}", .{i}),
            .extub => |i| try writer.print("extub {}", .{i}),
            .exts => |i| try writer.print("exts {}", .{i}),
            .truncd => |i| try writer.print("truncd {}", .{i}),
            .stosi => |i| try writer.print("stosi {}", .{i}),
            .stoui => |i| try writer.print("stoui {}", .{i}),
            .dtosi => |i| try writer.print("dtosi {}", .{i}),
            .dtoui => |i| try writer.print("dtoui {}", .{i}),
            .swtof => |i| try writer.print("swtof {}", .{i}),
            .uwtof => |i| try writer.print("uwtof {}", .{i}),
            .sltof => |i| try writer.print("sltof {}", .{i}),
            .ultof => |i| try writer.print("ultof {}", .{i}),
            .vastart => |i| try writer.print("vastart {}", .{i}),
            .vaarg => |i| try writer.print("vaarg{} {}", .{ i.ty, i.val }),
            .phi => |i| try writer.print("phi @{s} {}, @{s} {}", .{ i.label1, i.val1, i.label2, i.val2 }),
            .hlt => try writer.writeAll("hlt"),
        }
    }
};

/// QBE types used to specify the size and representation of values.
pub const Type = union(enum) {
    word,
    long,
    single,
    double,

    zero,

    byte,
    signed_byte,
    unsigned_byte,
    halfword,
    signed_halfword,
    unsigned_halfword,

    /// Aggregate type with a specified name
    aggregate: *const TypeDef,

    /// Returns a C ABI type. Extended types are converted to closest base
    /// types
    pub fn intoAbi(self: Type) Type {
        return switch (self) {
            .byte, .signed_byte, .unsigned_byte, .halfword, .signed_halfword, .unsigned_halfword => .word,
            else => self,
        };
    }

    /// Returns the closest base type
    pub fn intoBase(self: Type) Type {
        return switch (self) {
            .byte, .signed_byte, .unsigned_byte, .halfword, .signed_halfword, .unsigned_halfword => .word,
            .aggregate => .long,
            else => self,
        };
    }

    /// Returns byte size for values of the type
    pub fn size(self: Type) u64 {
        return switch (self) {
            .byte, .signed_byte, .unsigned_byte, .zero => 1,
            .halfword, .signed_halfword, .unsigned_halfword => 2,
            .word, .single => 4,
            .long, .double => 8,
            .aggregate => |td| blk: {
                var offset: u64 = 0;

                for (td.items) |item| {
                    const item_type = item.ty;
                    const repeat = item.repeat;
                    const alignment = item_type.@"align"();
                    const item_size = repeat * item_type.size();
                    const padding = (alignment - (offset % alignment)) % alignment;
                    offset += padding + item_size;
                }

                const alignment = self.@"align"();
                const padding = (alignment - (offset % alignment)) % alignment;

                break :blk offset + padding;
            },
        };
    }

    /// Returns byte alignment for values of the type
    pub fn @"align"(self: Type) u64 {
        return switch (self) {
            .aggregate => |td| blk: {
                if (td.@"align") |a| {
                    break :blk a;
                }

                var max_align: u64 = 1;
                for (td.items) |item| {
                    const item_align = item.ty.@"align"();
                    if (item_align > max_align) {
                        max_align = item_align;
                    }
                }
                break :blk max_align;
            },
            else => self.size(),
        };
    }

    pub fn format(
        self: Type,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .byte => try writer.writeAll("b"),
            .signed_byte => try writer.writeAll("sb"),
            .unsigned_byte => try writer.writeAll("ub"),
            .halfword => try writer.writeAll("h"),
            .signed_halfword => try writer.writeAll("sh"),
            .unsigned_halfword => try writer.writeAll("uh"),
            .word => try writer.writeAll("w"),
            .long => try writer.writeAll("l"),
            .single => try writer.writeAll("s"),
            .double => try writer.writeAll("d"),
            .zero => try writer.writeAll("z"),
            .aggregate => |td| try writer.print(":{s}", .{td.name}),
        }
    }
};

/// QBE value that is accepted by instructions
pub const Value = union(enum) {
    /// `%`-temporary
    temporary: []const u8,
    /// `$`-global
    global: []const u8,
    /// Constant
    @"const": u64,

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .temporary => |name| try writer.print("%{s}", .{name}),
            .global => |name| try writer.print("${s}", .{name}),
            .@"const" => |val| try writer.print("{}", .{val}),
        }
    }
};

/// QBE data definition
pub const DataDef = struct {
    linkage: Linkage,
    name: []const u8,
    @"align": ?u64,
    items: []const Item,

    pub const Item = struct {
        ty: Type,
        item: DataItem,
    };

    pub fn init(
        linkage: Linkage,
        name: []const u8,
        @"align": ?u64,
        items: []const Item,
    ) DataDef {
        return DataDef{
            .linkage = linkage,
            .name = name,
            .@"align" = @"align",
            .items = items,
        };
    }

    pub fn format(
        self: DataDef,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{}data ${s} = ", .{ self.linkage, self.name });

        if (self.@"align") |@"align"| {
            try writer.print("align {} ", .{@"align"});
        }

        try writer.writeAll("{ ");
        for (self.items, 0..) |item, i| {
            if (i > 0) try writer.writeAll(", ");
            try writer.print("{} {}", .{ item.ty, item.item });
        }
        try writer.writeAll(" }");
    }
};

/// Data definition item
pub const DataItem = union(enum) {
    /// Symbol and offset
    symbol: Symbol,
    /// String
    str: []const u8,
    /// Constant
    @"const": u64,
    /// Zero-initialized data of specified size
    zero: u64,

    pub const Symbol = struct {
        name: []const u8,
        offset: ?u64,
    };

    pub fn format(
        self: DataItem,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .symbol => |s| {
                if (s.offset) |off| {
                    try writer.print("${s} +{}", .{ s.name, off });
                } else {
                    try writer.print("${s}", .{s.name});
                }
            },
            .str => |s| try writer.print("\"{s}\"", .{s}),
            .@"const" => |val| try writer.print("{}", .{val}),
            .zero => |size| try writer.print("z {}", .{size}),
        }
    }
};

/// QBE aggregate type definition
pub const TypeDef = struct {
    name: []const u8,
    @"align": ?u64,
    items: []const Item,

    pub const Item = struct {
        ty: Type,
        repeat: usize,
    };

    pub fn init(name: []const u8, @"align": ?u64, items: []const Item) TypeDef {
        return TypeDef{
            .name = name,
            .@"align" = @"align",
            .items = items,
        };
    }

    pub fn format(
        self: TypeDef,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("type :{s} = ", .{self.name});
        if (self.@"align") |@"align"| {
            try writer.print("align {} ", .{@"align"});
        }

        try writer.writeAll("{ ");
        for (self.items, 0..) |item, i| {
            if (i > 0) try writer.writeAll(", ");
            if (item.repeat > 1) {
                try writer.print("{} {}", .{ item.ty, item.repeat });
            } else {
                try writer.print("{}", .{item.ty});
            }
        }
        try writer.writeAll(" }");
    }
};

/// An IR statement
pub const Statement = union(enum) {
    assign: Assign,
    @"volatile": Instr,

    pub const Assign = struct {
        temp: Value,
        ty: Type,
        instr: Instr,
    };

    pub fn format(
        self: Statement,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .assign => |s| {
                if (s.temp != .temporary) {
                    @panic("Assignment target must be a temporary");
                }
                try writer.print("{} ={} {}", .{ s.temp, s.ty, s.instr });
            },
            .@"volatile" => |s| try writer.print("{}", .{s}),
        }
    }
};

/// A block of QBE instructions with a label
pub const Block = struct {
    /// Label before the block
    label: []const u8,
    /// A list of statements in the block
    items: ArrayList(BlockItem),

    /// Initialize a new block with a label and allocator
    pub fn init(label: []const u8, allocator: Allocator) Block {
        return .{
            .label = label,
            .items = ArrayList(BlockItem).init(allocator),
        };
    }

    pub fn deinit(self: *Block) void {
        self.items.deinit();
    }

    /// Adds a comment to the block
    pub fn addComment(self: *Block, contents: []const u8) !void {
        try self.items.append(.{ .comment = contents });
    }

    /// Adds a new instruction to the block
    pub fn addInstr(self: *Block, instr: Instr) !void {
        try self.items.append(.{ .statement = .{ .@"volatile" = instr } });
    }

    /// Adds a new instruction assigned to a temporary
    pub fn assignInstr(self: *Block, temp: Value, ty: Type, instr: Instr) !void {
        try self.items.append(.{ .statement = .{ .assign = .{
            .temp = temp,
            .ty = ty.intoBase(),
            .instr = instr,
        } } });
    }

    /// Returns true if the block's last instruction is a jump
    pub fn jumps(self: *const Block) bool {
        if (self.items.items.len == 0) return false;

        const last = self.items.items[self.items.items.len - 1];
        if (last == .statement and last.statement == .@"volatile") {
            const instr = last.statement.@"volatile".instr;
            return switch (instr) {
                .ret, .jmp, .jnz => true,
                else => false,
            };
        }
        return false;
    }

    pub fn format(
        self: Block,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("@{s}\n", .{self.label});
        for (self.items.items) |item| {
            try writer.print("\t{}\n", .{item});
        }
    }
};

/// See `Block.items`
pub const BlockItem = union(enum) {
    statement: Statement,
    comment: []const u8,

    pub fn format(
        self: BlockItem,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .statement => |stmt| try writer.print("{}", .{stmt}),
            .comment => |comment| try writer.print("# {s}", .{comment}),
        }
    }
};

/// A QBE function definition
pub const Function = struct {
    /// Function's linkage
    linkage: Linkage,
    /// Function name
    name: []const u8,
    /// Function arguments
    arguments: []const Argument,
    /// Return type
    return_ty: ?Type,
    /// labeled blocks
    blocks: ArrayList(Block),

    allocator: Allocator,

    /// Initialize an empty function and returns it
    pub fn init(
        linkage: Linkage,
        name: []const u8,
        arguments: []const Argument,
        return_ty: ?Type,
        allocator: Allocator,
    ) Function {
        return .{
            .linkage = linkage,
            .name = name,
            .arguments = arguments,
            .return_ty = return_ty,
            .blocks = ArrayList(Block).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Function) void {
        for (self.blocks.items) |*block| {
            block.deinit();
        }
        self.blocks.deinit();
    }

    /// Adds a new empty block with a specified label and returns a reference to it
    pub fn addBlock(self: *Function, label: []const u8) !*Block {
        try self.blocks.append(Block.init(label, self.allocator));
        return &self.blocks.items[self.blocks.items.len - 1];
    }

    /// Returns a reference to the last block
    pub fn lastBlock(self: *Function) !*Block {
        if (self.blocks.items.len == 0) @panic("Function must have at least one block");
        return &self.blocks.items[self.blocks.items.len - 1];
    }

    /// Adds a new instruction to the last block
    pub fn addInstr(self: *Function, instr: Instr) !void {
        if (self.blocks.items.len == 0) {
            @panic("Last block must be present");
        }
        try self.blocks.items[self.blocks.items.len - 1].addInstr(instr);
    }

    /// Adds a new instruction assigned to a temporary
    pub fn assignInstr(self: *Function, temp: Value, ty: Type, instr: Instr) !void {
        if (self.blocks.items.len == 0) {
            @panic("Last block must be present");
        }
        try self.blocks.items[self.blocks.items.len - 1].assignInstr(temp, ty, instr);
    }

    pub fn format(
        self: Function,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{}function", .{self.linkage});
        if (self.return_ty) |ty| {
            try writer.print(" {}", .{ty});
        }

        var args = std.ArrayList([]const u8).init(self.allocator);
        defer {
            for (args.items) |arg| {
                self.allocator.free(arg);
            }
            args.deinit();
        }

        for (self.arguments) |arg| {
            const arg_string = try std.fmt.allocPrint(self.allocator, "{} {}", .{ arg.ty, arg.val });
            try args.append(arg_string);
        }

        const args_string = try std.mem.join(self.allocator, ", ", args.items);
        defer self.allocator.free(args_string);

        try writer.print(" ${s}({s}) {{\n", .{ self.name, args_string });
        for (self.blocks.items) |blk| {
            try writer.print("{}", .{blk});
        }
        try writer.print("}}", .{});
    }
};

/// Linkage of a function or data defintion (e.g. section and
/// private/public status)
pub const Linkage = struct {
    /// Specifies whether the target is going to be accessible publicly
    exported: bool = false,
    /// Specifies target's section
    section: ?[]const u8 = null,
    /// Specifies target's section flags
    secflags: ?[]const u8 = null,
    /// Specifies whether the target is stored in thread-local storage
    thread_local: bool = false,

    /// Returns the default configuration for private linkage
    pub fn private() Linkage {
        return .{};
    }

    /// Returns the configuration for private linkage with a provided section
    pub fn privateWithSection(section: []const u8) Linkage {
        return .{ .section = section };
    }

    /// Returns the default configuration for public linkage
    pub fn public() Linkage {
        return .{ .exported = true };
    }

    /// Returns the configuration for public linkage with a provided section
    pub fn publicWithSection(section: []const u8) Linkage {
        return .{ .exported = true, .section = section };
    }

    /// Returns the configuration for thread-local private linkage
    pub fn threadLocal() Linkage {
        return .{ .thread_local = true };
    }

    /// Returns the configuration for thread-local public linkage
    pub fn exportedThreadLocal() Linkage {
        return .{ .exported = true, .thread_local = true };
    }

    /// Returns the configuration for thread-local private linkage with a section
    pub fn threadLocalWithSection(section: []const u8) Linkage {
        return .{ .thread_local = true, .section = section };
    }

    pub fn format(
        self: Linkage,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (self.exported) {
            try writer.writeAll("export ");
        }
        if (self.thread_local) {
            try writer.writeAll("thread ");
        }
        if (self.section) |section| {
            try writer.print("section \"{s}\"", .{section});
            if (self.secflags) |secflags| {
                try writer.print(" \"{s}\"", .{secflags});
            }
            try writer.writeAll(" ");
        }
    }
};

/// A complete QBE IL module
pub const Module = struct {
    functions: ArrayList(Function),
    types: ArrayList(TypeDef),
    data: ArrayList(DataDef),

    /// Creates a new module
    pub fn init(allocator: Allocator) Module {
        return Module{
            .functions = ArrayList(Function).init(allocator),
            .types = ArrayList(TypeDef).init(allocator),
            .data = ArrayList(DataDef).init(allocator),
        };
    }

    pub fn deinit(self: *Module) void {
        for (self.functions.items) |*func| {
            func.deinit();
        }
        self.functions.deinit();
        self.types.deinit();
        self.data.deinit();
    }

    /// Adds a function to the module, returning a reference to it for later
    /// modification
    pub fn addFunction(self: *Module, func: Function) !*Function {
        try self.functions.append(func);
        return &self.functions.items[self.functions.items.len - 1];
    }

    /// Adds a type definition to the module, returning a reference to it for
    /// later modification
    pub fn addType(self: *Module, def: TypeDef) !*TypeDef {
        try self.types.append(def);
        return &self.types.items[self.types.items.len - 1];
    }

    /// Adds a data definition to the module
    pub fn addData(self: *Module, data: DataDef) !*DataDef {
        try self.data.append(data);
        return &self.data.items[self.data.items.len - 1];
    }

    pub fn format(
        self: Module,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        for (self.types.items) |ty| {
            try writer.print("{s}\n", .{ty});
        }

        for (self.functions.items) |func| {
            try writer.print("{s}\n", .{func});
        }

        for (self.data.items) |data| {
            try writer.print("{s}\n", .{data});
        }
    }
};

test {
    _ = @import("tests.zig");
}
