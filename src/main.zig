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

    // var args = try clap.parse(clap.Help, &params, allocator);
    // defer args.deinit();

    if (args.flag("--help")) {
        std.debug.warn("--help\n", .{});
        std.os.exit(1);
    }
    // if (args.option("--number")) |n|
    //     std.debug.warn("--number = {}\n", .{n});
    // for (args.positionals()) |pos|
    //     std.debug.warn("{}\n", .{pos});

    var board = Board.initial();
    std.debug.print("{}", .{board});
}

const InternalError = error{ UnableToFindKing, MissingPiece };

const ParseError = error{ NonSequentialMoves, MissingMoveForBlack, InvalidDataProvided };

const IllegalMoveError = error{ CastleBlocked, NoLongerAbleToCastle, AlreadyCastled };

const CastleSide = enum(u1) {
    KingSide,
    QueenSide,
};

const Color = enum(u1) {
    Black,
    White,
};

const Diagonal = enum(u1) {
    Left,
    Right,
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

    pub fn in_direction(square: Square, vector: AttackVector) ?Square {
        const components = vector.components();

        const file = fallibleAdd(isize, square.file, components.up);
        const rank = fallibleAdd(isize, square.rank, components.right);

        if (file != null and rank != null) {
            return Square{ .file = file.?, .rank = rank.? };
        } else {
            return null;
        }
    }

    pub fn add_vector(square: Square, vector: MoveVector) ?Square {
        const file = fallibleAdd(isize, square.file, vector.file);
        const rank = fallibleAdd(isize, square.rank, vector.rank);

        if (file != null and rank != null) {
            return Square{ .file = file.?, .rank = rank.? };
        } else {
            return null;
        }
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

    pub fn moves(piece: *Piece, board: *Board) MoveIterator {
        return MoveIterator.init(piece, board);
    }

    const MoveIterator = struct {
        piece: *Piece,
        index: usize,
        board: *Board,

        pub fn init(piece: *Piece, board: *Board) MoveIterator {
            return MoveIterator{
                .piece = piece,
                .index = 0,
                .board = board,
            };
        }

        pub fn next(it: *MoveIterator) ?Square {
            while (true) {
                switch (it.piece.type_) {
                    .Pawn => switch (it.index) {
                        0 => {},
                    },
                }
            }
        }
    };
};

const AttackDirection = enum(u3) {
    Up,
    UpRight,
    Right,
    DownRight,
    Down,
    DownLeft,
    Left,
    UpLeft,

    fn next_for_piece_type(direction: AttackDirection, piece_type: PieceType) ?AttackDirection {
        return switch (piece_type) {
            Queen, King => switch (direction) {
                Up => UpRight,
                UpRight => Right,
                Right => DownRight,
                DownRight => Down,
                Down => DownLeft,
                DownLeft => Left,
                Left => UpLeft,
                UpLeft => null,
            },
            Rook => switch (direction) {
                Up => Right,
                Right => Down,
                Down => Left,
                else => null,
            },
            Bishop => switch (direction) {
                UpRight => DownRight,
                DownRight => DownLeft,
                DownLeft => UpLeft,
                else => null,
            },
            else => null,
        };
    }
};

const AttackVector = struct {
    direction: AttackDirection,
    distance: u3,

    pub fn init(comptime type_: PieceType) AttackVector {
        return AttackVector{
            .distance = 1,
            .direction = switch (type_) {
                Queen, Rook, King => AttackDirection.Up,
                Bishop => UpRight,
                else => @compileError("AttackVector is not usable for pawns or knights"),
            },
        };
    }

    pub fn components(vector: AttackVector) var {
        var up: isize;
        var right: isize;

        switch (vector.direction) {
            Up => {
                up = 1;
                right = 0;
            },
            UpRight => {
                up = 1;
                right = 1;
            },
            Right => {
                up = 0;
                right = 1;
            },
            DownRight => {
                up = -1;
                right = 1;
            },
            Down => {
                up = -1;
                right = 0;
            },
            DownLeft => {
                up = -1;
                right = -1;
            },
            Left => {
                up = 0;
                right = -1;
            },
            UpLeft => {
                up = 1;
                right = -1;
            },
        }

        return .{
            .up = @as(isize, vector.distance) * up,
            .right = @as(isize, vector.distance) * right,
        };
    }
};

const MoveVector = struct {
    file: isize,
    rank: isize,
};

const Piece = struct {
    type_: PieceType,
    color: Color,

    pub fn init(type_: PieceType, color: Color) Piece {
        return Piece{
            .type_ = type_,
            .color = color,
        };
    }

    pub fn chars(piece: Piece, unicode: bool) []const u8 {
        if (unicode) {
            return switch (piece.type_) {
                PieceType.Pawn => "♟︎",
                PieceType.Knight => "♞",
                PieceType.Bishop => "♝",
                PieceType.Rook => "♜",
                PieceType.King => "♚",
                PieceType.Queen => "♛",
            };
        } else {
            return switch (piece.type_) {
                PieceType.Pawn => "P",
                PieceType.Knight => "N",
                PieceType.Bishop => "B",
                PieceType.Rook => "R",
                PieceType.King => "K",
                PieceType.Queen => "Q",
            };
        }
    }

    pub fn possible_moves(piece: *Piece, square: Square, board: *Board) MoveIterator {
        return MoveIterator.init(piece, square, pieces);
    }

    const MoveIterator = struct {
        piece: *Piece,
        square: Square,
        board: *Board,
        type_: ?MoveContext,

        const NextMove = struct {
            move: ?Move = null,
            out_of_moves: bool = false,
        };

        const PawnMove = enum {
            OneForward,
            TwoForward,
            LeftAttack,
            RightAttack,

            pub fn initial() PawnMove {
                return PawnMove.OneForward;
            }

            pub fn next(move: PawnMove) ?PawnMove {
                return switch (move) {
                    OneForward => TwoForward,
                    TwoForward => LeftAttack,
                    LeftAttack => RightAttack,
                    RightAttack => null,
                };
            }

            pub fn vector(move: PawnMove, color: Color) MoveVector {
                if (color == Color.White) {
                    return switch (move) {
                        OneForward => MoveVector{
                            .file = 1,
                            .rank = 0,
                        },
                        TwoForward => MoveVector{
                            .file = 2,
                            .rank = 0,
                        },
                        LeftAttack => MoveVector{
                            .file = 1,
                            .rank = -1,
                        },
                        RightAttack => MoveVector{
                            .file = 1,
                            .rank = 1,
                        },
                    };
                } else {
                    return switch (move) {
                        OneForward => MoveVector{
                            .file = -1,
                            .rank = 0,
                        },
                        TwoForward => MoveVector{
                            .file = -2,
                            .rank = 0,
                        },
                        LeftAttack => MoveVector{
                            .file = -1,
                            .rank = 1,
                        },
                        RightAttack => MoveVector{
                            .file = -1,
                            .rank = -1,
                        },
                    };
                }
            }
        };

        const MoveContext = union(PieceType) {
            Pawn = PawnMove,
            Queen = AttackVector,
            Rook = AttackVector,
            Bishop = AttackVector,
            King = AttackDirection,
            Knight = struct {
                move_index: u3,
            },

            fn init(type_: PieceType) MoveContext {
                return switch (type_) {
                    Pawn => .{
                        .Pawn = PawnMove.initial(),
                    },
                    Queen => .{
                        .Queen = AttackVector.init(PieceType.Queen),
                    },
                    Rook => .{
                        .Rook = AttackVector.init(PieceType.Rook),
                    },
                    Bishop => .{
                        .Bishop = AttackVector.init(PieceType.Bishop),
                    },
                    King => .{
                        .King = AttackDirection.Up,
                    },
                    Knight => .{
                        .Knight = .{ .move_index = 0 },
                    },
                };
            }
        };

        pub fn init(piece: *Piece, square: Square, board: *Board) MoveIterator {
            return MoveIterator{
                .piece = piece,
                .square = square,
                .board = board,
                .type_ = MoveContext.init(piece.type_),
            };
        }

        pub fn next(it: *MoveIterator) ?Square {
            while (true) {
                const context = it.context orelse return null;

                const next_move = switch (context) {
                    .Queen, .Rook, .Bishop => |vector| it.next_move_in_vector(vector),
                    .King => |direction| it.next_move_for_king(direction),
                    .Knight => |data| it.next_move_for_knight(&data.move_index),
                    .Pawn => |move| it.next_move_for_pawn(move),
                };

                if (next_move.out_of_moves) it.context = null;
                if (next_move.move) |move| return move;
            }
        }

        fn next_move_in_vector(it: *MoveIterator, vector: *AttackVector) NextMove {
            var next_move = NextMove.initial();
            const go_to_next_direction = false;

            const square = it.square.in_direction(vector.direction, vector.distance);
            if (square) |sq| {
                if (board.get_piece(sq)) |piece| {
                    vector.distance = 1;
                    go_to_next_direction = true;

                    if (piece.color != it.piece.color) next_move.move = sq;
                } else {
                    if (vector.distance < 7) {
                        vector.distance += 1;
                    } else {
                        vector.distance = 1;
                        go_to_next_direction = true;
                    }

                    next_move.move = sq;
                }
            } else {
                vector.distance = 1;
                go_to_next_direction = true;
            }

            if (go_to_next_direction) {
                if (vector.direction.next_for_piece_type(it.piece.type_)) |d| {
                    vector.direction = d;
                } else {
                    out_of_moves = true;
                }
            }

            return next_move;
        }

        fn next_move_for_king(it: *MoveIterator, direction: *AttackDirection) NextMove {
            var next_move = NextMove.initial();
            const vector = AttackVector{ .direction = direction, .distance = 1 };

            if (it.square.in_direction(vector)) |sq| {
                if (it.board.get_piece(sq)) |piece| {
                    if (piece.color != it.piece.color) {
                        next_move.move = sq;
                    }
                } else {
                    next_move.move = sq;
                }
            }

            if (direction.next_for_piece_type(context.piece.type_)) |d| {
                direction.* = d;
            } else {
                next_move.out_of_moves = true;
            }

            return next_move;
        }

        fn next_move_for_knight(it: *MoveIterator, move_index: *u3) NextMove {
            const knight_moves = [8]MoveVector{
                MoveVector{ .file = 2, .rank = 1 },
                MoveVector{ .file = 1, .rank = 2 },
                MoveVector{ .file = -1, .rank = 2 },
                MoveVector{ .file = -2, .rank = 1 },
                MoveVector{ .file = -2, .rank = -1 },
                MoveVector{ .file = -1, .rank = -2 },
                MoveVector{ .file = 1, .rank = -2 },
                MoveVector{ .file = 2, .rank = -1 },
            };

            var next_move = NextMove.initial();
            var square = it.square.add_vector(knight_moves[move_index.*]);

            if (move_index.* == math.intMax(u3)) {
                next_move.out_of_moves = true;
            } else {
                move_index.* += 1;
            }

            if (square) |sq| {
                if (it.board.get_piece(sq)) |piece| {
                    if (piece.color != it.piece.color) {
                        next_move.move = sq;
                    }
                } else {
                    next_move.move = sq;
                }
            }

            return next_move;
        }

        fn next_move_for_pawn(it: *MoveIterator, move: *PawnMove) NextMove {
            var next_move = NextMove.initial();
            const vector = move.vector(it.piece.color);
            const square = it.square.add_vector(vector);

            if (move_index.* == math.intMax(u3)) {
                next_move.out_of_moves = true;
            } else {
                move_index.* += 1;
            }

            if (square) |sq| {
                if (it.board.get_piece(sq)) |piece| {
                    if (piece.color != it.piece.color) {
                        next_move.move = sq;
                    }
                } else {
                    next_move.move = sq;
                }
            }

            return next_move;
        }
    };
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
    pieces: [8][8]?Piece,
    move: CurrentMove,
    white_castling: CastleData,
    black_castling: CastleData,

    pub fn initial() !Board {
        return Board{
            .pieces = initial_board,
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
            if (board.white_castling.has_castled)
                return IllegalMoveError.AlreadyCastled;

            if (!board.white_castling.able_to_castle())
                return IllegalMoveError.NoLongerAbleToCastle;
        } else {
            if (board.black_castling.has_castled)
                return IllegalMoveError.AlreadyCastled;

            if (!board.black_castling.able_to_castle())
                return IllegalMoveError.NoLongerAbleToCastle;
        }

        const move_through_squares: []const Square;
        const king_castle_squares: []const Square;

        // TODO
        // if (board.move.color == Color.White) {
        //     if (side == CastleSide.QueenSide) {
        //         move_through_squares =
        //     }
        // }

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

                const rook = board.get_piece(Square.init(7, 0)) orelse
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

    pub fn standard_move(board: *Board, move: StandardMove) !void {
        // z
    }

    pub fn find_piece_that_made_move(board: *Board, move: *StandardMove) IllegalMoveError!*Piece {
        var piece_iter = board.pieces(move.type_, board.current_move.color);

        switch (move.piece_type) {
            .Queen => {},
        }
    }

    pub fn in_checkmate(board: *Board) InternalError!?*Piece {
        // TODO: finish this function
        const white_king = try board.get_king(Color.White);
        const black_king = try board.get_king(Color.Black);

        return null;
    }

    pub fn get_king(board: *Board, color: Color) InternalError!*Piece {
        var king_pieces = board.pieces(PieceType.King, color);
        while (king_pieces.next()) |king| return king;

        return InternalError.UnableToFindKing;
    }

    pub fn get_piece(board: *Board, square: Square) ?*Piece {
        return &(board.pieces[square.rank][square.file] orelse return null);
    }

    pub fn get_piece_const(board: *const Board, square: Square) ?*const Piece {
        return &(board.pieces[square.rank][square.file] orelse return null);
    }

    pub fn any_pieces_on_squares(board: *Board, squares: []const Square) bool {
        for (squares) |square| {
            if (board.get_piece(square) != null) {
                return true;
            }
        }

        return false;
    }

    pub fn pieces(board: *Board, type_: ?PieceType, color: ?Color) Iterator {
        return Iterator.init(board, type_, color);
    }

    pub fn format(board: *const Board, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: var) !void {
        const cyan = "\u{001b}[46m";
        const green = "\u{001b}[42m";
        const reset = "\u{001b}[0m";
        const bright_white = "\u{001b}[37;1m";
        const bright_black = "\u{001b}[30;1m";

        var rank: u3 = 7;

        while (rank >= 0) : (rank -= 1) {
            var file: u3 = 0;

            while (file <= 7) : (file += 1) {
                const square = Square.init(file, rank);

                const background = if ((@as(usize, file) + @as(usize, rank)) % 2 == 0) cyan else green;
                try writer.writeAll(background);

                if (board.get_piece_const(square)) |piece| {
                    const color = if (piece.color == Color.White) bright_white else bright_black;
                    try writer.print("{} {} ", .{ color, piece.chars(false) });
                } else {
                    try writer.writeAll("   ");
                }

                if (file == 7) break;
            }

            try writer.writeAll("\n");

            if (rank == 0) break;
        }

        try writer.writeAll(reset);

        try writer.print("\n{} to move (turn {})\n", .{
            if (board.move.color == Color.White) "white" else "black",
            board.move.turn + 1,
        });
    }

    const Iterator = struct {
        board: *Board,
        square: ?Square,
        type_: ?PieceType,
        color: ?Color,

        pub fn init(board: *Board, type_: ?PieceType, color: ?Color) Iterator {
            return Iterator{
                .board = board,
                .square = Square.init(0, 0),
                .type_ = type_,
                .color = color,
            };
        }

        fn increment_square(it: *Iterator, old_square: Square) void {
            if (old_square.rank == 7) {
                if (old_square.file == 7) {
                    it.square = null;
                } else {
                    it.square = Square{
                        .file = 0,
                        .rank = old_square + 1,
                    };
                }
            } else {
                it.square = Square{
                    .file = old_square.file + 1,
                    .rank = old_square.rank,
                };
            }
        }

        pub fn next(it: *Iterator) ?*Piece {
            while (true) {
                const square = it.square orelse return null;
                defer it.increment_square(square);

                if (board.get_piece(square)) |piece| {
                    if (it.type_) |type_| {
                        if (type_ != piece.type_) continue;
                    }

                    if (it.color) |color| {
                        if (color != piece.color) continue;
                    }

                    return piece;
                }
            }
        }

        pub fn collect(it: *Iterator, allocator: var) !std.ArrayList(*Piece) {
            var piece_list = std.ArrayList(*Piece).init(allocator);
            while (it.next()) |piece| try p.append(piece);

            return piece_list;
        }
    };
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

const CastleData = struct {
    king_has_moved: bool,
    kingside_rook_has_moved: bool,
    queenside_rook_has_moved: bool,
    has_castled: bool,

    pub fn initial() CastleData {
        return CastleData{
            .king_has_moved = false,
            .kingside_rook_has_moved = false,
            .queenside_rook_has_moved = false,
            .has_castled = false,
        };
    }

    // pub fn castle(data: *CastleData, side: CastleSide) void {
    //     data.king_has_moved = true;
    //     data.has_castled = true;

    //     switch (side) {
    //         .KingSide => data.kingside_rook_has_moved = true,
    //         .QueenSide => data.queenside_rook_has_moved = true,
    //     }
    // }

    pub fn able_to_castle(data: *CastleData, side: CastleSide) bool {
        const rook_has_moved = if (side == CastleSide.KingSide)
            data.kingside_rook_has_moved
        else
            data.queenside_rook_has_moved;

        return !data.king_has_moved and !rook_has_moved;
    }
};

const white_pawns = [1]?Piece{Piece.init(PieceType.Pawn, Color.White)} ** 8;
const black_pawns = [1]?Piece{Piece.init(PieceType.Pawn, Color.Black)} ** 8;
const empty_rank = [1]?Piece{null} ** 8;
const white_pieces = [8]?Piece{
    Piece.init(PieceType.Rook, Color.White),
    Piece.init(PieceType.Knight, Color.White),
    Piece.init(PieceType.Bishop, Color.White),
    Piece.init(PieceType.Queen, Color.White),
    Piece.init(PieceType.King, Color.White),
    Piece.init(PieceType.Bishop, Color.White),
    Piece.init(PieceType.Knight, Color.White),
    Piece.init(PieceType.Rook, Color.White),
};
const black_pieces = [8]?Piece{
    Piece.init(PieceType.Rook, Color.Black),
    Piece.init(PieceType.Knight, Color.Black),
    Piece.init(PieceType.Bishop, Color.Black),
    Piece.init(PieceType.Queen, Color.Black),
    Piece.init(PieceType.King, Color.Black),
    Piece.init(PieceType.Bishop, Color.Black),
    Piece.init(PieceType.Knight, Color.Black),
    Piece.init(PieceType.Rook, Color.Black),
};

const initial_board = [8][8]?Piece{
    white_pieces,
    white_pawns,
    empty_rank,
    empty_rank,
    empty_rank,
    empty_rank,
    black_pawns,
    black_pieces,
};

fn fallibleAdd(comptime Int: type, a: u3, b: Int) ?u3 {
    const sum = @as(Int, a) + b;

    if (sum >= 0 and sum <= math.maxInt(u3)) {
        return @as(u3, sum);
    } else {
        return null;
    }
}
