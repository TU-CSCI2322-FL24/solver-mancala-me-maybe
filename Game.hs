import Debug.Trace
import Data.Maybe

-- story one
type Position = Int
data Player = PlayerOne | PlayerTwo deriving (Show, Eq)
type Row = ([Int], Int) -- should be changed to "type Row = ([Pit], Store)" where Divet has been removed
type Board  = (Row, Row)

type Game = (Player, Board)

makeBoard :: Int -> Board
makeBoard k =
    let pits = [0 | _ <- 0..k]
    in ((pits, 0),(pits, 0))

move :: Game -> Position -> Board
move (player, board) pit =
    let stones = getStones (player,board) pit
    in if isJust stones then  makeMove player board pit stones
    else error = "Invalid pit selected"

getStones :: Game -> Position -> Maybe Int
getStones (player, (one,two)) pit =
    let side = sideOf player (one,two)
        val  = safeBangBang (zip side [1..]) pit
    in if val == 0 then nothing else Just val

safeBangBang :: [(Int,Int)] -> Int -> Int
safeBangBang [] _ = 0
safeBangBang ((val,pos):xs) pit = if pos == pit then val else safeBangBang xs pit

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

--Useful when we know that all pits will get a piece
sowAll :: Row -> Row
sowAll (_,s) = map (\a -> a + 1) s

--returns new row and remaining pieces
moveCorrectSide :: Row -> Position -> Int -> (Row,Int)
moveCorrectSide (_ , pits) pos stones =
    let (before,after) = splitAt pos pits
        remaining      = stones - after

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
