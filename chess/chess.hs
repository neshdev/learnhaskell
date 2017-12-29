module Chess where

    data Player = White | Black deriving (Show, Eq, Ord)
    
    type Board = [(Maybe Player, Maybe Location, Maybe Material, Maybe [Constraint])]

    --You need to check this basically before any turn.
    isCheckMate :: Board -> Bool
    isCheckMate = undefined
    isCheck :: Board -> Bool
    isCheck = undefined
    isDraw :: Board -> Bool
    isDraw = undefined

    --Successor Functions
    type Rank = Integer
    type File = Integer
    type Location = (Rank,File) --Row,Column

    --file :: Location -> Integer
    --file (f,_) = f

    --rank :: Location -> Integer
    --rank (_,r) = r

    chessNotation :: Location -> String
    chessNotation = undefined

    data Direction =  Diagonal Rank File 
                    | Vertical File 
                    | Horizontal Rank 
                    | Lshape Rank File deriving (Eq, Show, Ord)

    data Constraint =     StartingClassic 
                        | Blocking Direction
                        | PawnPromotion 
                        | EnPassant Direction
                        | Opposition Direction
                        | ProtectedOpposition Direction
                        | OutOfBound
                        | None deriving (Eq, Show, Ord)

    data Material =   Pawn
                    | Bishop
                    | Knight
                    | Rook
                    | Queen
                    | King deriving (Eq, Show, Ord)

-- Oposition constrains are relative from the current material
    -- ex:
    -- if piece is a Pawn (a,4), and an opponents material is at (a,5)
    -- Then the relative position for the current pawn is (0,0) and the opposition constraint is on (0,1)

    {-
    Each piece can move in several direction, but there can be constraints on each piece
    This is board independent. 

    Imagine a board with only the pawn and maybe one other piece(friend or foe)
    Pawns can only move vertical. 
    In the beggining, they can move 1 or 2 moves up. 
    They can take on the location of the opponent that is Diagonal to them
    if its a EnPassant, They can take on the (location of opponent + Vertical 1) that is Adjacent(Horizontal) to them 
    In general they can only move one vertically by one
    If they blocked by a piece that is Opposite to them, they cant move
    It cant go out of bounds
    -}
    successor :: Material -> Constraint -> [Direction]
    successor Pawn StartingClassic                   = [Vertical 1, Vertical 2]
    successor Pawn PawnPromotion                     = [Vertical 1, Vertical 2]
    successor Pawn (Opposition (Vertical _))         = []
    successor Pawn (Opposition (Horizontal _))       = [Vertical 1]
    successor Pawn (Opposition direction)            = [Vertical 1, direction]
    successor Pawn OutOfBound                        = []
    successor Pawn (Blocking _)                      = [] 
    successor Pawn (EnPassant (Horizontal rank))     = [Diagonal rank 1, Vertical 1]
    successor Pawn _                                 = [Vertical 1]
    {-
    #  Imagine a board with only the King and maybe one other pieces(friend or foe)
    #  King can move 1 step in any direction
    #  King can take an opponent that is not protected
    #  King cannot take an opponent that is protected
    #  King cannot take a position where a friendly piece is placed
    #  King cannot go out of bounds
    -}
    successor King StartingClassic                                    = []
    successor King (ProtectedOpposition direction)                    = filter f list where
                                                                        list = [Vertical (-1), Vertical 1, Horizontal 1, Horizontal (-1)] ++ [ Diagonal 1 1, Diagonal (-1) 1, Diagonal (-1) (-1), Diagonal 1 (-1)]
                                                                        f x= x /= direction
    successor King (Blocking direction)                               = filter f list where
                                                                        list = [Vertical (-1), Vertical 1, Horizontal 1, Horizontal (-1)] ++ [ Diagonal 1 1, Diagonal (-1) 1, Diagonal (-1) (-1), Diagonal 1 (-1)]
                                                                        f x= x /= direction
    successor King _                                                  = [Vertical (-1), Vertical 1, Horizontal 1, Horizontal (-1)] ++ [ Diagonal 1 1, Diagonal (-1) 1, Diagonal (-1) (-1), Diagonal 1 (-1)]
    -- some otheer successor conditions...
    -- Basically the rules for each piece
    successor _ _ = []
    

    initalizeBoard :: Board
    initalizeBoard =  whitePawns ++ blackPawns where 
                      whitePawns = [ (
                                    Just White, 
                                    Just (file,2), 
                                    Just Pawn, 
                                    Just [StartingClassic,PawnPromotion]
                                ) 
                                | file <- [1..8]
                            ]
                      blackPawns = [ (
                                    Just Black, 
                                    Just (file,2), 
                                    Just Pawn, 
                                    Just [StartingClassic,PawnPromotion]
                                ) 
                                | file <- [1..8]
                            ]
    
    initalizeGame :: Board -> (Board, Player)
    initalizeGame b  = (b, White)

    --give this during each turn
    materialForPlayer :: Player -> Board -> [Maybe Material]
    materialForPlayer player board = [ m | (p,_,m,_) <- board, p == Just player]

    --validate they choose an apporpriate material
    isPlayersMaterialInChosenMaterial :: Player -> Material -> Maybe Material
    isPlayersMaterialInChosenMaterial = undefined

    playersTransition :: Player -> Board -> Material -> (Board, Player) 
    playersTransition = undefined