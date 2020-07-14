const std = @import("std");
const adma = @import("adma");
const clap = @import("clap");
const mecha = @import("mecha");

pub fn main() anyerror!void {
    const adma_ref = adma.AdmaAllocator.init();
    defer adma_ref.deinit();

    const allocator = &adma_ref.allocator;

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    const params = comptime [_]clap.Param(clap.Help){
        clap.parseParam("-h, --help          Display this help and exit.              ") catch unreachable,
        clap.parseParam("-n, --number <NUM>  An option parameter, which takes a value.") catch unreachable,
        clap.Param(clap.Help){
            .takes_value = true,
        },
    };

    var args = try clap.parse(clap.Help, &params, allocator);
    defer args.deinit();

    if (args.flag("--help"))
        std.debug.warn("--help\n", .{});
    if (args.option("--number")) |n|
        std.debug.warn("--number = {}\n", .{n});
    for (args.positionals()) |pos|
        std.debug.warn("{}\n", .{pos});
}

const InternalError = error{ UnableToFindKing, MissingPiece };

const ParseError = error{ NonSequentialMoves, MissingMoveForBlack, InvalidDataProvided };

const IllegalMoveError = error{ CastleBlocked, NoLongerAbleToCastle };

const CastleSide = enum(u1) {
    KingSide,
    QueenSide,
};

const Color = enum(u1) {
    Black,
    White,
};

const Square = struct {
    file: u3,
    rank: u3,

    pub fn init(file: u3, rank: u3) Square {
        return Square{
            .file = file,
            .rank = rank,
        };
    }

    pub fn inFile(file: u3) []const Square {
        return [_]Square {
            Square.init(file, 0),
            Square.init(file, 1),
            Square.init(file, 2),
            Square.init(file, 3),
            Square.init(file, 4),
            Square.init(file, 5),
            Square.init(file, 6),
            Square.init(file, 7),
        };
    }

    pub fn inRank(file: u3) []const Square {
        return [_]Square {
            Square.init(file, 0),
            Square.init(file, 1),
            Square.init(file, 2),
            Square.init(file, 3),
            Square.init(file, 4),
            Square.init(file, 5),
            Square.init(file, 6),
            Square.init(file, 7),
        };
    }

    pub fn fileParser() mecha.Parser(u3) {
        const file_converter = struct {
            fn converter(file: u8) ?u3 {
                switch (file) {
                    'a'...'h' => |f| return @as(u3, f - 'a'),
                    else => return null,
                }
            }
        }.converter;

        return mecha.convert(u3, file_converter, mecha.range('a', 'h'));
    }

    pub fn rankParser() mecha.Parser(u3) {
        const rank_converter = struct {
            fn converter(rank: u8) ?u3 {
                switch (rank) {
                    '1'...'8' => |r| return @as(u3, r - '1'),
                    else => return null,
                }
            }
        }.converter;

        return mecha.convert(u3, rank_converter, mecha.range('1', '8'));
    }

    pub fn parser() mecha.Parser(Square) {
        const square_converter = struct {
            fn converter(tuple: var) ?Square {
                return Square{
                    .file = tuple.at(0).*,
                    .rank = tuple.at(1).*,
                };
            }
        }.converter;

        return mecha.convert(Square, square_converter, mecha.combine(.{
            fileParser(), rankParser(),
        }));
    }
};

const PieceType = enum(u3) {
    Pawn,
    Knight,
    Bishop,
    Rook,
    King,
    Queen,

    fn generate(comptime type_: PieceType) fn (void) ?PieceType {
        return struct {
            fn generate(parsed: void) ?PieceType {
                return type_;
            }
        }.generate;
    }

    pub fn parser() mecha.Parser(PieceType) {
        return mecha.oneOf(.{
            mecha.convert(PieceType, generate(PieceType.Pawn), mecha.string("")),
            mecha.convert(PieceType, generate(PieceType.Knight), mecha.string("N")),
            mecha.convert(PieceType, generate(PieceType.Bishop), mecha.string("B")),
            mecha.convert(PieceType, generate(PieceType.Rook), mecha.string("R")),
            mecha.convert(PieceType, generate(PieceType.King), mecha.string("K")),
            mecha.convert(PieceType, generate(PieceType.Queen), mecha.string("Q")),
        });
    }
};

const Piece = struct {
    type_: PieceType,
    location: Square,
    color: Color,

    pub fn init(type_: PieceType, color: Color, rank: u3, file: u3) Piece {
        return Piece{
            .type_ = type_,
            .color = color,
            .location = Square{
                .rank = rank,
                .file = file,
            },
        };
    }
};

const initial_board = [_]Piece{
    // white pawns
    Piece.init(PieceType.Pawn, Color.White, 1, 0),
    Piece.init(PieceType.Pawn, Color.White, 1, 1),
    Piece.init(PieceType.Pawn, Color.White, 1, 2),
    Piece.init(PieceType.Pawn, Color.White, 1, 3),
    Piece.init(PieceType.Pawn, Color.White, 1, 4),
    Piece.init(PieceType.Pawn, Color.White, 1, 5),
    Piece.init(PieceType.Pawn, Color.White, 1, 6),
    Piece.init(PieceType.Pawn, Color.White, 1, 7),
    // other white pieces
    Piece.init(PieceType.Rook, Color.White, 0, 0),
    Piece.init(PieceType.Knight, Color.White, 0, 1),
    Piece.init(PieceType.Bishop, Color.White, 0, 2),
    Piece.init(PieceType.Queen, Color.White, 0, 3),
    Piece.init(PieceType.King, Color.White, 0, 4),
    Piece.init(PieceType.Bishop, Color.White, 0, 5),
    Piece.init(PieceType.Knight, Color.White, 0, 6),
    Piece.init(PieceType.Rook, Color.White, 0, 7),
    // black pawns
    Piece.init(PieceType.Pawn, Color.Black, 6, 0),
    Piece.init(PieceType.Pawn, Color.Black, 6, 1),
    Piece.init(PieceType.Pawn, Color.Black, 6, 2),
    Piece.init(PieceType.Pawn, Color.Black, 6, 3),
    Piece.init(PieceType.Pawn, Color.Black, 6, 4),
    Piece.init(PieceType.Pawn, Color.Black, 6, 5),
    Piece.init(PieceType.Pawn, Color.Black, 6, 6),
    Piece.init(PieceType.Pawn, Color.Black, 6, 7),
    // other black pieces
    Piece.init(PieceType.Rook, Color.Black, 7, 0),
    Piece.init(PieceType.Knight, Color.Black, 7, 1),
    Piece.init(PieceType.Bishop, Color.Black, 7, 2),
    Piece.init(PieceType.Queen, Color.Black, 7, 3),
    Piece.init(PieceType.King, Color.Black, 7, 4),
    Piece.init(PieceType.Bishop, Color.Black, 7, 5),
    Piece.init(PieceType.Knight, Color.Black, 7, 6),
    Piece.init(PieceType.Rook, Color.Black, 7, 7),
};

const CurrentMove = struct {
    turn: usize,
    color: Color,

    pub fn initial() CurrentMove {
        return CurrentMove{
            .turn = 0,
            .color = Color.White,
        };
    }

    pub fn increment(move: *CurrentMove) void {
        if (move.color == Color.White) {
            move.color = Color.Black;
        } else {
            move.color = Color.White;
            move.turn += 1;
        }
    }
};

const Board = struct {
    pieces: std.ArrayList(Piece),
    move: CurrentMove,
    white_can_castle: bool,
    black_can_castle: bool,

    pub fn initial(allocator: var) !Board {
        return Board{
            .pieces = try std.ArrayList.init(&initial_board, allocator),
            .move = CurrentMove.initial(),
            .white_can_castle = true,
            .black_can_castle = true,
        };
    }

    pub fn make_move(board: *Board, move: Move) !void {
        switch (move) {
            .Standard => |m| try board.standard_move(m),
            .Castle => |c| try board.castle(c),
        }

        board.current_move.increment();
    }

    pub fn castle(board: *Board, side: CastleSide) !void {
        if (board.move.color == Color.White) {
            if (!board.white_can_castle)
                return IllegalMoveError.NoLongerAbleToCastle;

            const white_king = try board.get_king(Color.White);
            if (c == CastleSide.KingSide) {
                const blocking_squares = [_]Square{
                    Square.init(5, 0), Square.init(6, 0),
                };
                if (board.any_pieces_on_squares(&blocking_squares))
                    return IllegalMoveError.CastleBlocked;

                const rook = board.get_piece_on_square(Square.init(7, 0)) orelse
                    return InternalError.MissingPiece;

                // TODO: check for checks preventing castling

                rook.location = Square.init(5, 0);
                white_king.location = Square.init(6, 0);
            }
        } else {
            if (!board.black_can_castle)
                return IllegalMoveError.NoLongerAbleToCastle;

            const black_king = try board.get_king_position(Color.Black);
        }
    }

    pub fn standard_move(board: *Board, move: StandardMove) !void {}

    pub fn find_piece_that_made_move(board: *Board, move: *StandardMove) IllegalMoveError!*Piece {
        switch (move.piece_type) {
            .Queen => {

            },
        }
    }

    pub fn in_checkmate(board: *Board) InternalError!?*Piece {
        // TODO: finish this function
        const white_king = try board.get_king(Color.White);
        const black_king = try board.get_king(Color.Black);

        return null;
    }

    pub fn get_king(board: *Board, color: Color) InternalError!*Piece {
        for (pieces) |piece| {
            if (piece.type_ == PieceType.King and piece.color == color)
                return &piece;
        }

        return InternalError.UnableToFindKing;
    }

    pub fn get_piece_on_square(board: *Board, square: Square) ?*Piece {
        for (board.pieces) |piece| {
            if (eql(Square, piece.location, square)) {
                return &piece;
            }
        }

        return null;
    }

    pub fn any_pieces_on_squares(board: *Board, squares: []const Square) bool {
        for (board.pieces) |piece| {
            for (squares) |square| {
                if (eql(Square, piece.location, square)) {
                    return true;
                }
            }
        }

        return false;
    }
};

const Move = union(enum) {
    Standard: StandardMove,
    Castle: CastleSide,

    fn getCastleShort(parse: void) ?Move {
        return Move{ .Castle = CastleSide.KingSide };
    }

    fn getCastleLong(parse: void) ?Move {
        return Move{ .Castle = CastleSide.QueenSide };
    }

    fn getStandardMove(move: StandardMove) ?Move {
        return Move{ .Standard = move };
    }

    fn parser() mecha.Parser(Move) {
        const standard_move = mecha.convert(StandardMove, getStandardMove, StandardMove.parser());
        const castle_short = mecha.convert(mecha.oneOf(.{
            mecha.string("O-O"),
            mecha.string("0-0"),
        }), getCastleShort);
        const castle_long = mecha.convert(mecha.oneOf(.{
            mecha.string("O-O-O"),
            mecha.string("0-0-0"),
        }), getCastleLong);

        return mecha.oneOf(.{
            standard_move,
            castle_short,
            castle_long,
        });
    }

    pub fn parseMoves(moves_str: []const u8, allocator: var) ParseError!std.ArrayList(Move) {
        const whitespace = mecha.oneOf(.{
            mecha.char(' '), mecha.char('\n'), mecha.char('\t'),
        });

        const move_parser = Move.parser();
        const move_pair = mecha.combine(.{
            mecha.many(whitespace),
            mecha.int(usize, 10),
            mecha.char('.'),
            mecha.manyRange(1, math.maxInt(usize), whitespace),
            move_parser,
            mecha.manyRange(1, math.maxInt(usize), whitespace),
            mecha.opt(move_parser),
            mecha.many(whitespace),
        });

        const moves = std.ArrayList(Move).init(allocator);
        var remaining = &moves_str[0..];
        var current_move_index: usize = 1;

        while (move_pair(remaining)) |result| {
            const move_index = result.value.at(1).*;
            if (move_index != current_move_index)
                return ParseError.NonSequentialMoves;

            const white_move = result.value.at(4).*;
            try moves.append(white_move);

            if (result.value.at(7).*) |black_move| {
                try moves.append(black_move);
            } else {
                if (move_pair(result.rest)) {
                    return ParseError.MissingMoveForBlack;
                } else {
                    break;
                }
            }

            remaining = result.rest;
            current_move_index += 1;
        }

        if (mecha.many(whitespace)(remaining) == null) {
            return ParseError.InvalidDataProvided;
        }

        return moves;
    }
};

const StandardMove = struct {
    piece_type: PieceType,
    destination: Square,
    source_file: ?u3,
    source_rank: ?u3,
    en_passant: bool,
    capture: bool,
    check: bool,
    checkmate: bool,
    promote_to: ?PieceType,

    fn fromTuple(tuple: var) ?StandardMove {
        const destination: Square = undefined;
        var source_file: ?u3 = null;
        var source_rank: ?u3 = null;
        if (tuple.at(4).*) |file| {
            destination = Square{
                .file = file,
                .rank = tuple.at(5).* orelse return null,
            };
            source_file = tuple.at(1).*;
            source_rank = tuple.at(2).*;
        } else {
            destination = Square{
                .file = tuple.at(1).* orelse return null,
                .rank = tuple.at(2).* orelse return null,
            };
        }

        return StandardMove{
            .piece_type = tuple.at(0).*,
            .capture = tuple.at(3).* != null,
            .promote_to = tuple.at(6).*,
            .en_passant = tuple.at(7).* != null,
            .check = tuple.at(8).* != null,
            .checkmate = tuple.at(9).* != null,
            .destination = destination,
            .source_file = source_file,
            .source_rank = source_rank,
        };
    }

    pub fn parser() mecha.Parser(StandardMove) {
        const promote_to = struct {
            fn typeGetter(tuple: var) ?PieceType {
                return tuple.at(1).*;
            }

            fn parser() mecha.Parser(PieceType) {
                return mecha.convert(PieceType, typeGetter, mecha.combine(.{
                    mecha.char('='),
                    PieceType.parser(),
                }));
            }
        }.parser;

        return mecha.convert(StandardMove, fromTuple, mecha.combine(.{
            PieceType.parser(),
            mecha.opt(Square.fileParser()),
            mecha.opt(Square.rankParser()),
            mecha.opt(mecha.oneOf(.{ mecha.char('x'), mecha.char('X') })),
            mecha.opt(Square.fileParser()),
            mecha.opt(Square.rankParser()),
            mecha.opt(promote_to),
            mecha.opt(mecha.string("e.p.")),
            mecha.opt(mecha.char('+')),
            mecha.opt(mecha.char('#')),
        }));
    }
};
