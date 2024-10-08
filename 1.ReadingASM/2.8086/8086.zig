const std = @import("std");
const ENDIAN: std.builtin.Endian = .little;

const OpCode = enum(u6) { mov = 0b100010, unknown };
const Mode = enum(u2) { mem = 0b00, mem_disp8 = 0b01, mem_disp16 = 0b10, register = 0b11 };

const Word = enum(u1) { byte = 0b0, word = 0b1 };
const Instruction = packed struct(u16) {
    W: Word,
    D: enum(u1) { to = 0b0, from = 0b1 },
    OpCode: OpCode,
    RM: u3,
    Reg: u3,
    Mode: Mode,
};

const ImmediateInstruction = packed struct(u8) {
    Reg: u3,
    W: Word,
    OpCode: u4,
};

fn isImmediate(byte: u8) bool {
    return byte & 0b11110000 == 0b10110000;
}

pub fn main() !void {
    var args = std.process.args();
    _ = args.next();
    const filename = args.next() orelse {
        std.debug.print("usage: 8086.zig <filename>\n", .{});
        return;
    };
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const reader = file.reader();

    const stdout_file = std.io.getStdOut().writer();
    var buf_writer = std.io.bufferedWriter(stdout_file);
    const writer = buf_writer.writer();
    _ = try writer.print("; {s}\n\n", .{filename}); // comment w/ filename
    _ = try writer.print("bits 16\n\n", .{});

    while (reader.readInt(u8, ENDIAN)) |byte| {
        if (isImmediate(byte)) { // move data to reg immediately
            const inst: ImmediateInstruction = @bitCast(byte);
            const data = try reader.readInt(if (inst.W == .byte) u8 else u16, ENDIAN);
            try writer.print("mov {s}, {d}\n", .{ regAddr(inst.W, inst.Reg), data });
        } else {
            const inst: Instruction = @bitCast([_]u8{ byte, try reader.readInt(u8, ENDIAN) });
            const inst_st = switch (inst.OpCode) {
                .mov => |variant| @tagName(variant),
                else => unreachable,
            };
            switch (inst.mode) {
                .register => { // reg to reg
                    const reg1 = regAddr(inst.W, inst.Reg);
                    const reg2 = regAddr(inst.W, inst.RM);
                    try writer.print("{s} {s}, {s}\n", .{ inst_st, reg1, reg2 });
                },
                inline else => |variant| {
                    const disp = reader.readInt(if (variant == .mem_disp8) u8 else u16, ENDIAN);
                    const reg = regAddr(inst.W, inst.Reg);
                    if (inst.D == .from) { // reg to mem
                        const mem = memAddr(inst.RM);
                        try writer.print("{s} {s}, [{s} + {d}]\n", .{ inst_st, reg, mem, disp });
                    } else { // mem to reg
                        const mem = memAddr(inst.Mode, inst.RM, disp);
                        try writer.print("{s} [{s} + {d}], {s}\n", .{ inst_st, mem, disp, reg });
                    }
                },
            }
        }
    } else |err| switch (err) {
        error.EndOfStream => {},
        else => std.debug.print("Error reading instruction: {}\n", .{err}),
    }
    try buf_writer.flush();
}

const RegMem = enum(u3) { bx_si = 0b000, bx_di = 0b001, bp_si = 0b010, bp_di = 0b011, si = 0b100, di = 0b101, bp = 0b110, bx = 0b111 };

fn memAddr(rm: u3) []const u8 {
    const reg_mem: RegMem = @enumFromInt(rm);
    return switch (reg_mem) {
        .bx_si => "bx + si",
        .bx_di => "bx + di",
        .bp_si => "bp + si",
        .bp_di => "bp + di",
        inline else => |variant| @tagName(variant),
    };
}

const REG_byte = enum(u3) { al = 0b000, cl = 0b001, dl = 0b010, bl = 0b011, ah = 0b100, ch = 0b101, dh = 0b110, bh = 0b111 };
const REG_word = enum(u3) { ax = 0b000, cx = 0b001, dx = 0b010, bx = 0b011, sp = 0b100, bp = 0b101, si = 0b110, di = 0b111 };

fn regAddr(w: Word, register: u3) []const u8 {
    return switch (w) {
        .byte => switch (@as(REG_byte, @enumFromInt(register))) {
            inline else => |variant| @tagName(variant),
        },
        .word => switch (@as(REG_word, @enumFromInt(register))) {
            inline else => |variant| @tagName(variant),
        },
    };
}

test {
    std.testing.refAllDecls(@This());
}
