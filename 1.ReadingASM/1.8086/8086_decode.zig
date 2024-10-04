const std = @import("std");

const OpCode = enum(u6) { move = 0b100010, unknown };
const Mode = enum(u2) {
    mem = 0b00,
    mem_disp8 = 0b01,
    mem_disp16 = 0b10,
    register = 0b11,
};

const REG_byte = enum(u3) { AL = 0b000, CL = 0b001, DL = 0b010, BL = 0b011, AH = 0b100, CH = 0b101, DH = 0b110, BH = 0b111 };
const REG_word = enum(u3) { AX = 0b000, CX = 0b001, DX = 0b010, BX = 0b011, SP = 0b100, BP = 0b101, SI = 0b110, DI = 0b111 };
const Word = enum(u1) { byte = 0b0, word = 0b1 };
const Instruction = packed struct(u16) {
    OpCode: OpCode,
    D: enum(u1) { to = 0b0, from = 0b1 },
    W: Word,
    Mode: Mode,
    Reg: u3,
    RM: u3,
};

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

    while (reader.readStruct(Instruction)) |instruction| {
        _ = try writer.write(switch (instruction.OpCode) {
            .move => "mov ",
            else => unreachable,
        });
        _ = try writer.write(" ");
        _ = try writer.write(regAddr(instruction.W, instruction.Reg));
        _ = try writer.write(", ");
        _ = try writer.write(regRM(instruction.Mode, instruction.W, instruction.RM));
    } else |err| switch (err) {
        error.EndOfStream => {},
        else => std.debug.print("Error reading instruction: {}\n", .{err}),
    }
}

fn regRM(mode: Mode, w: Word, address: u3) []const u8 {
    return switch (mode) {
        .register => regAddr(w, address),
        else => unreachable,
    };
}

fn regAddr(w: Word, address: u3) []const u8 {
    return switch (w) {
        .byte => switch (@as(REG_byte, @enumFromInt(address))) {
            .AL => "al",
            .CL => "cl",
            .DL => "dl",
            .BL => "bl",
            .AH => "ah",
            .CH => "ch",
            .DH => "dh",
            .BH => "bh",
        },
        .word => switch (@as(REG_word, @enumFromInt(address))) {
            .AX => "ax",
            .CX => "cx",
            .DX => "dx",
            .BX => "bx",
            .SP => "sp",
            .BP => "bp",
            .SI => "si",
            .DI => "di",
        },
    };
}
