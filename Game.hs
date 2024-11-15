import Debug.Trace
import Data.Maybe

-- story one
type Position = Int
type Stones = Int
data Player = PlayerOne | PlayerTwo deriving (Show, Eq)
type Row = (Int, [Int]) -- should be changed to "type Row = ([Pit], Store)" where Divet has been removed
type Board  = (Row, Row)

type Game = (Player, Board)

makeGame :: Int -> Game
makeGame k =
    let pits = [4 | _ <- [1..k]]
    in (PlayerOne,((0, pits),(0, pits)))

move :: Game -> Position -> Game
move game@(p,board) pit =
    let stones = getStones game pit
    in if isJust stones then movePieces ((p, pickUp game pit), fromJust stones) p (pit + 1)
    else error "Invalid pit selected"

pickUp :: Game -> Position -> Board
pickUp (PlayerOne, ((s,one), two)) pit =
    let (begin,(_:end)) = splitAt pit one
        newRow = begin ++ [0] ++ end
    in ((s,newRow),two)
pickUp (PlayerTwo, (one, (s,two))) pit =
    let (begin,(_:end)) = splitAt pit two
        newRow = begin ++ [0] ++ end
    in (one,(s,newRow))

movePieces :: (Game,Stones) -> Player -> Position -> Game
movePieces (game@(playersTurn,(one,two)), stones) playersSide pos
    | stones <= 0 = (otherPlayer playersTurn, (one, two))
    | playersTurn == playersSide = movePieces (getCorrect game stones pos) (otherPlayer playersSide) 0
    | otherwise = movePieces (getIncorrect game stones pos) (otherPlayer playersSide) 0

getCorrect :: Game -> Stones -> Position -> (Game,Int)
getCorrect (PlayerOne,((store,pits),two)) stones pos =
    let (newPits,remain)     = sowStones pits pos stones
        (left, newStore)     = if remain > 0 then (remain - 1, store + 1) else (remain,store)
    in ((PlayerOne,((newStore,newPits),two)),left)
getCorrect (PlayerTwo,(one,(store,pits))) stones pos =
    let (newPits,remain)     = sowStones pits pos stones
        (left, newStore)     = if remain > 0 then (remain - 1, store + 1) else (remain,store)
    in ((PlayerTwo,(one,(newStore, newPits))), left)

getIncorrect :: Game -> Stones -> Position -> (Game, Int)
getIncorrect (PlayerOne,(one,(store,pits))) stones pos =
    let (newPits,remain) = sowStones pits pos stones
        newBoard = (one,(store,newPits))
    in ((PlayerOne,newBoard),remain)
getIncorrect (PlayerTwo,((store,pits),two)) stones pos =
    let (newPits,remain) = sowStones pits pos stones
        newBoard = ((store,newPits),two)
    in ((PlayerTwo,newBoard), remain)


getStones :: Game -> Position -> Maybe Int
getStones (player, (one,two)) pit =
    let (_,side) = sideOf player (one,two)
        val  = safeBangBang (zip side [0..]) pit
    in if val == 0 then Nothing else Just val

safeBangBang :: [(Int,Int)] -> Int -> Int
safeBangBang [] _ = 0
safeBangBang ((val,pos):xs) pit = if pos == pit then val else safeBangBang xs pit

otherSide :: Player -> Board -> Row
otherSide PlayerOne (_,board) = board
otherSide PlayerTwo (board,_) = board

sideOf :: Player -> Board -> Row
sideOf PlayerOne (board,_) = board
sideOf PlayerTwo (_,board) = board

--Useful when we know that all pits will get a piece
sowAll :: [Stones] -> [Stones]
sowAll s = map (\a -> a + 1) s

sowSome :: [Stones] -> Int -> [Stones]
sowSome pits num = [if y <= num then x + 1 else x | (x,y) <- (zip pits [1..])]

--returns new row and remaining pieces
sowStones :: [Stones] -> Position -> Stones -> ([Stones],Int)
sowStones pits 0 stones =
    let remaining      = stones - (length pits)
        val            = if remaining >= 0 then sowAll pits else sowSome pits stones
    in (val, remaining)
sowStones pits pos stones =
    let (before,after) = splitAt pos pits
        remaining      = stones - (length after)
        val            = if remaining >= 0 then sowAll after else sowSome after stones
        in (before ++ val, remaining)

otherPlayer :: Player -> Player
otherPlayer PlayerOne = PlayerTwo
otherPlayer PlayerTwo = PlayerOne

-- story two
data GameState = Ongoing | Win Player | Tie deriving Show

hasGameEnded :: Game -> Bool
hasGameEnded (_, ((_,one), (_,two)))
    | all (\s -> s == 0) one     = True
    | all (\s -> s == 0) two     = True
    | otherwise                  = False

currGameState :: Game -> GameState
currGameState state@(_, ((s1,_), (s2,_)))
    | not (hasGameEnded state)      = Ongoing
    | s1 > s2                       = Win PlayerOne
    | s2 > s1                       = Win PlayerTwo
    | otherwise                     = Tie
