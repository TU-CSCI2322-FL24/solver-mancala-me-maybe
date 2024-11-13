import Debug.Trace

-- story one
type Position = Int
data Divet = Store Int | Pit Position Int deriving (Show, Eq)
data Player = PlayerOne | PlayerTwo deriving (Show, Eq)

type Row = [Divet] -- should be changed to "type Row = ([Pit], Store)" where Divet has been removed
type Board  = (Row, Row)

type Game = (Player, Board)

makeBoard :: Int -> Board
makeBoard k = 
    let aux (x, y) 0 = (x, y) 
        aux (x, y) n = aux (((Pit pos 4):x), ((Pit pos 4):y)) (n - 1)
            where pos = (k - n) + 1
    in aux ([Store 0], [Store 0]) k

move :: Player -> Board -> Int -> Board
move player board pit = 
    let aux [(Store st)] = error "Pit does not exist"
        aux ((Pit ps st):xs) = if ps == pit then st else aux xs
    in makeMove player board pit (aux (sideOf player board))

sideOf :: Player -> Board -> Row
sideOf PlayerOne board = fst board
sideOf PlayerTwo board = snd board

makeMove :: Player -> Board -> Int -> Int -> Board
makeMove PlayerOne (one, two) pit st
    | pit - st < 0  = makeMove PlayerTwo ((sowRow one pit st), two) size (st - pit)
    | otherwise     = ((sowRow one pit st), two)
    where size = length one
makeMove PlayerTwo (one, two) pit st
    | pit - st < 0  = makeMove PlayerOne (one, (sowRow two pit st)) size (st - pit)
    | otherwise     = (one, (sowRow two pit st))
    where size = length one

sowRow :: Row -> Int -> Int -> Row
sowRow [Store st] pit n = [Store (st + 1)]
sowRow ((Pit pos st):xs) pit 0 = ((Pit pos st):xs)
sowRow ((Pit pos st):xs) pit n
    | pos == pit    = (Pit pos 0):(sowRow xs pit n)
    | pos < pit     = (Pit pos (st + 1)):(sowRow xs pit (n - 1))
    | otherwise     = (Pit pos st):(sowRow xs pit n)

-- story two
data GameState = Ongoing | Win Player | Tie

hasGameEnded :: Game -> Bool
hasGameEnded (_, (one, two))
    | all (\(Pit _ s) -> s == 0) (init one)     = True
    | all (\(Pit _ s) -> s == 0) (init two)     = True
    | otherwise                                 = False

currGameState :: Game -> GameState
currGameState state@(_, (one, two))
    | not (hasGameEnded state)      = Ongoing
    | s1 > s2                       = Win PlayerOne
    | s2 > s1                       = Win PlayerTwo
    | otherwise                     = Tie
    where (Store s1) = last one
          (Store s2) = last two
