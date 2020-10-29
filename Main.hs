module Main where

import Data.List (sortBy)

import qualified Data.Map.Strict as M


main :: IO ()
main = putStrLn "Hello, Haskell!"

data Piece = Piece Color PieceType
    deriving (Show, Ord, Eq)

data Color = White | Black
    deriving (Show, Ord, Eq)

data PieceType
    = Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King
    deriving (Show, Ord, Eq)

newtype Square = Square (Char, Int)
    deriving (Show, Ord, Eq)

getRow :: Square -> Int
getRow (Square (_, r)) = r

getCol :: Square -> Char
getCol (Square (c, _)) = c

newtype Board = Board (M.Map Square (Maybe Piece))
    deriving (Show, Ord, Eq)

emptyBoard :: Board
emptyBoard = Board $ M.fromList $ zip emptySquares (replicate 64 Nothing)
  where
    emptySquares :: [Square]
    emptySquares = [Square (c, r) | c <- columns, r <- rows]

startingBoard :: Board
startingBoard =
    let Board emptySquares = emptyBoard
    in Board
        $ pawns
            `M.union` rooks
            `M.union` knights
            `M.union` bishops
            `M.union` kings
            `M.union` queens
            `M.union` emptySquares
  where
    pawns :: M.Map Square (Maybe Piece)
    pawns = M.fromList
        $  [(Square (c, 2), Just $ Piece White Pawn) | c <- columns]
        ++ [(Square (c, 7), Just $ Piece Black Pawn) | c <- columns]

    rooks :: M.Map Square (Maybe Piece)
    rooks = M.fromList
        [ (Square ('a', 1), Just $ Piece White Rook)
        , (Square ('h', 1), Just $ Piece White Rook)
        , (Square ('a', 8), Just $ Piece Black Rook)
        , (Square ('h', 8), Just $ Piece Black Rook)
        ]

    knights :: M.Map Square (Maybe Piece)
    knights = M.fromList
        [ (Square ('b', 1), Just $ Piece White Knight)
        , (Square ('g', 1), Just $ Piece White Knight)
        , (Square ('b', 8), Just $ Piece Black Knight)
        , (Square ('g', 8), Just $ Piece Black Knight)
        ]

    bishops :: M.Map Square (Maybe Piece)
    bishops = M.fromList
        [ (Square ('c', 1), Just $ Piece White Bishop)
        , (Square ('f', 1), Just $ Piece White Bishop)
        , (Square ('c', 8), Just $ Piece Black Bishop)
        , (Square ('f', 8), Just $ Piece Black Bishop)
        ]

    kings :: M.Map Square (Maybe Piece)
    kings = M.fromList
        [ (Square ('e', 1), Just $ Piece White King)
        , (Square ('e', 8), Just $ Piece Black King)
        ]

    queens :: M.Map Square (Maybe Piece)
    queens = M.fromList
        [ (Square ('d', 1), Just $ Piece White Queen)
        , (Square ('d', 8), Just $ Piece Black Queen)
        ]

columns :: String
columns = ['a' .. 'h']

rows :: [Int]
rows = [1 .. 8]

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard

showBoard :: Board -> String
showBoard board = concatMap (\ x -> showRow x board ++ "\n") (reverse rows)

showRow :: Int -> Board -> String
showRow row (Board m) = concatMap showSquarePiece sortedRow
  where
    sortedRow :: [(Square, Maybe Piece)]
    sortedRow = sortBy (\ (s1, _) (s2, _) -> getRow s1 `compare` getRow s2) contentsOfRow

    contentsOfRow :: [(Square, Maybe Piece)]
    contentsOfRow = M.toList $ M.filterWithKey (\ (Square (_, r)) _ -> r == row) m

showSquarePiece :: (Square, Maybe Piece) -> String
showSquarePiece (square, mPiece) = case mPiece of
    Nothing    -> replicate 3 $ charSquareColor square
    Just piece -> [charSquareColor square, charPiece piece, charSquareColor square]
  where
    charSquareColor :: Square -> Char
    charSquareColor s = case squareColor s of
        White -> '█'
        Black -> '▒'

    charPiece :: Piece -> Char
    charPiece (Piece clr typ) = case (clr, typ) of
        (Black, Pawn)   -> '♙'
        (Black, Knight) -> '♘'
        (Black, Bishop) -> '♗'
        (Black, Rook)   -> '♖'
        (Black, Queen)  -> '♕'
        (Black, King)   -> '♔'
        (White, Pawn)   -> '♟'
        (White, Knight) -> '♞'
        (White, Bishop) -> '♝'
        (White, Rook)   -> '♜'
        (White, Queen)  -> '♛'
        (White, King)   -> '♚'

squareColor :: Square -> Color
squareColor (Square (c, r))
    | odd  (enumCols M.! c) && even r = White
    | odd  (enumCols M.! c) && odd r  = Black
    | even (enumCols M.! c) && even r = Black
    | even (enumCols M.! c) && odd r  = White
  where
    enumCols :: M.Map Char Int
    enumCols = M.fromList $ zip columns rows
