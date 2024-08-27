const std = @import("std");

const Lexeme = enum(u8) {
    Ident,
    String,
    Number,
    Eof,
    Error,

    @"+" = '+',
    @"-" = '-',
    @"*" = '*',
    @"/" = '/',

    @":" = ':',
    @"," = ',',

    @"{" = '{',
    @"}" = '}',
    @"[" = '[',
    @"]" = ']',
};

const Token = struct {
    kind: Lexeme,
    start: u32,
    end: u32,
};

const Lexer = struct {
    source: []const u8,
    pos: usize = 0,

    fn next(self: *Lexer) Token {
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            const start = self.pos;
            self.pos += 1;

            const lexeme: Lexeme = switch (c) {
                0...32 => continue,
                'a'...'z', 'A'...'Z', '_', 128...255 => b: {
                    while (self.pos < self.source.len) switch (self.source[self.pos]) {
                        'a'...'z', 'A'...'Z', '_', '0'...'9', 128...255 => {},
                        else => break,
                    };
                    break :b .Ident;
                },
                '0'...'9' => b: {
                    while (self.pos < self.source.len) switch (self.source[self.pos]) {
                        '0'...'9' => self.pos += 1,
                        else => break,
                    };
                    break :b .Number;
                },
                '"' => b: {
                    while (self.pos < self.source.len and self.source[self.pos] != '"') self.pos += 1;
                    self.pos += 1;
                    break :b .String;
                },
                '#' => {
                    while (self.pos < self.source.len and self.source[self.pos] != '\n') self.pos += 1;
                    continue;
                },
                '+', '-', '*', '/', ':', ',', '{', '}', '[', ']' => @enumFromInt(c),
            };

            return .{ .lexeme = lexeme, .start = start, .end = self.pos };
        }

        return .{ .lexeme = .Eof, .start = self.pos, .end = self.pos };
    }
};
const Error = error{ Failed, OutOfMemory };

fn parse(comptime T: type, arena: std.mem.Allocator, source: []const u8) Error!T {
    var lexer = Lexer{ .source = source };
    return try parseLow(T, &lexer, arena, source);
}

fn parseLow(comptime T: type, lexer: *Lexer, arena: std.mem.Allocator, source: []const u8) Error!T {}
