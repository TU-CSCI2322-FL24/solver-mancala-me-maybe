import Debug.Trace

-- story one
type Pos = Int
type Player = Int
type Stones = Int

data Divet = Store Player Stones | Pit Pos Stones deriving (Show, Eq)
type Row = [Divet]
type Board  = (Row, Row)

makeBoard :: Int -> Board
makeBoard k = 
    let aux (x, y) 0 = (x, y) 
        aux (x, y) n = aux (((Pit pos 4):x), ((Pit pos 4):y)) (n - 1)
            where pos = (k - n) + 1
    in aux ([Store 1 0], [Store 2 0]) k 

move :: Player -> Board -> Int -> Board
move player (one, two) pit = 
    let aux [(Store pl st)] = error "Pit does not exist"
        aux ((Pit ps st):xs) = if ps == pit then st else aux xs
        side = case player of
            1 -> one
            2 -> two
    in makeMove player (one, two) pit (aux side)

makeMove :: Player -> Board -> Int -> Int -> Board
makeMove 1 (one, two) pit st
    | pit - st < 0  = makeMove 2 ((makeMove' one pit st), two) size (st - pit)
    | otherwise     = (makeMove' one pit st, two)
    where size = length one
makeMove 2 (one, two) pit st
    | pit - st < 0  = makeMove 1 (one, (makeMove' two pit st)) size (st - pit)
    | otherwise     = (one, makeMove' two pit st)
    where size = length one

makeMove' :: Row -> Int -> Int -> Row
makeMove' [Store pl st] pit n = [Store pl (st + 1)]
makeMove' ((Pit pos st):xs) pit 0 = ((Pit pos st):xs)
makeMove' ((Pit pos st):xs) pit n
    | pos == pit    = (Pit pos 0):(makeMove' xs pit n)
    | pos < pit     = (Pit pos (st + 1)):(makeMove' xs pit (n - 1))
    | otherwise     = (Pit pos st):(makeMove' xs pit n)

-- story two
type Game = Board

hasGameEnded :: Game -> Bool
hasGameEnded (one, two)
    | all (\(Pit _ s) -> s == 0) (init one)    = True
    | all (\(Pit _ s) -> s == 0) (init two)    = True
    | otherwise                                 = False

win :: Game -> Maybe Player
win (one, two)
    | s1 > s2       = Just 1
    | s2 > s1       = Just 2
    | otherwise     = Nothing
    where (Store p1 s1) = last one
          (Store p2 s2) = last two
