import Debug.Trace
import Data.Maybe

-- story one
type Position = Int
type Stones = Int
data Player = PlayerOne | PlayerTwo deriving (Show, Eq)
type Row = (Int, [Int])
type Board  = (Row, Row)
type Game = (Player, Board)

--setup
-----------------------------------------------------

makeGame :: Int -> Game
makeGame k =
    let pits = [4 | _ <- [1..k]]
    in (PlayerOne,((0, pits),(0, pits)))

------------------------------------------------------
--move

move :: Game -> Position -> Game
move game@(p,board) pit =
    let stones = getStones game pit
    in if isJust stones then movePieces ((p, pickUp game pit), fromJust stones) p (pit + 1)
    else error "Invalid pit selected"

pickUp :: Game -> Position -> Board
pickUp (p, ((s1,one), (s2,two))) pit =
    let (begin,(_:end)) = if p == PlayerOne then splitAt pit one else splitAt pit two
        newRow = begin ++ [0] ++ end
    in if p == PlayerOne then ((s1,newRow),(s2,two)) else ((s1,one),(s2,newRow))

getStones :: Game -> Position -> Maybe Int
getStones (player, (one,two)) pit =
    let (_,side) = sideOf player (one,two)
        val  = safeBangBang (zip side [0..]) pit
    in if val == 0 then Nothing else Just val

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

--returns new row and remaining pieces
sowStones :: [Stones] -> Position -> Stones -> ([Stones],Int)
sowStones pits pos stones =
    let (before,after) = splitAt pos pits
        remaining      = stones - (length after)
        val            = if remaining >= 0 then map (\a -> a + 1) after else [if y <= stones then x + 1 else x | (x,y) <- (zip pits [1..])]
        in (before ++ val, remaining)


-----------------------------------------------------------------------------------------
-- story two Game Status
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



-------------------------------------------------------------------------------------------
-- universal helper Functions

safeBangBang :: [(Int,Int)] -> Int -> Int
safeBangBang [] _ = 0
safeBangBang ((val,pos):xs) pit = if pos == pit then val else safeBangBang xs pit

sideOf :: Player -> Board -> Row
sideOf PlayerOne (board,_) = board
sideOf PlayerTwo (_,board) = board

otherPlayer :: Player -> Player
otherPlayer PlayerOne = PlayerTwo
otherPlayer PlayerTwo = PlayerOne
