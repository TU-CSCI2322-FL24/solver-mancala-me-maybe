module Game where

import Debug.Trace
import Data.Maybe
import Text.Read

-- story one
type Position = Int
type Stones = Int

data Player = PlayerOne | PlayerTwo deriving (Show, Eq)
type Row = (Int, [Int])
type Board  = (Row, Row)
type Game = (Player, Board)

--setup
-----------------------------------------------------
-- Story 1 & 3: Game and Move

makeGame :: Int -> Game
makeGame k =
    let pits = [4 | _ <- [1..k]]
    in (PlayerOne,((0, pits),(0, pits)))

move :: Game -> Position -> Game
move game@(p,board) pit =
    let stones = getStones game pit
    in case stones of
        Nothing -> error "Invalid Pit Selected"
        Just stones -> movePieces ((p, pickUp game pit), stones) p (pit + 1)

------------------------------------------------------
--move functions

pickUp :: Game -> Position -> Board
pickUp (p, ((s1,one), (s2,two))) pit =
    let (begin,(_:end)) = if p == PlayerOne then splitAt pit one else splitAt pit two
        newRow = begin ++ [0] ++ end
    in if p == PlayerOne then ((s1,newRow),(s2,two)) else ((s1,one),(s2,newRow))


movePieces :: (Game,Stones) -> Player -> Position -> Game
movePieces (game@(playersTurn,(one,two)), stones) playersSide pos
    | stones <= 0                = checkLanding (game,stones) playersSide (length one) --(otherPlayer playersTurn, (one, two))
    | playersTurn == playersSide = movePieces (getCorrect game stones pos) (otherPlayer playersSide) 0
    | otherwise                  = movePieces (getIncorrect game stones pos) (otherPlayer playersSide) 0
    -- | playersTurn == playersSide = movePieces (getCorrect game stones pos) (otherPlayer playersSide) 0
    -- | otherwise = movePieces (getIncorrect game stones pos) (otherPlayer playersSide) 0
    -- | stones <= 0 = traceShow pos $ if emptyPit (playersTurn, (one,two)) pos then takeStoneFromOther (playersTurn, (one,two)) pos else (otherPlayer playersTurn, (one, two))

getCorrect :: Game -> Stones -> Position -> (Game,Int)
getCorrect (PlayerOne,((store,pits),two)) stones pos =
    let (newPits,remain)     = sowStones pits pos stones
        (left, newStore)     = if remain > 0 then (remain - 1, store + 1) else (remain - 1,store)
    in ((PlayerOne,((newStore,newPits),two)),left)
getCorrect (PlayerTwo,(one,(store,pits))) stones pos =
    let (newPits,remain)     = sowStones pits pos stones
        (left, newStore)     = if remain > 0 then (remain - 1, store + 1) else (remain - 1,store)
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
        val            = if remaining >= 0 then map (\a -> a + 1) after else [if y <= stones then x + 1 else x | (x,y) <- (zip after [1..])]
        in (before ++ val, remaining)

checkLanding :: (Game,Stones) -> Player -> Int -> Game
checkLanding ((playerTurn,b),0) playerSide len --if ending in your own store
    | playerSide /= playerTurn = (playerTurn,b)
    | otherwise = (playerSide,b)
checkLanding ((playerTurn,b),stones) playerSide len
    | playerSide /= playerTurn = (playerSide,b)
    | otherwise = --check where piece landed
        let position = len + stones
            check    = getStones (playerSide,b) position
            newTurn  = otherPlayer playerTurn
            other    = getStones (newTurn,b) (len + 1 - position)
        in case check of
            Nothing -> (newTurn, b)
            Just 1  -> case other of
                Nothing -> (newTurn, b)
                Just y -> (newTurn, addToStore playerTurn b y position)
            Just x  -> (newTurn, b)

addToStore :: Player -> Board -> Stones -> Position -> Board
addToStore PlayerOne ((s1,one),two) val pos =
    let (before,_:after) = splitAt pos one
    in ((s1 + val + 1,before ++ [0] ++ after),two)
addToStore PlayerTwo (one,(s2,two)) val pos =
    let (before,_:after) = splitAt pos two
    in (one,(s2 + val + 1,before ++ [0] ++ after))

-- emptyPit :: Game -> Position -> Bool
-- emptyPit game@(pl, (one, two)) pit = 
                                   -- let Just stone = getStones (pl, (one,two)) pit
                                   -- in traceShow stone $ stone == 1
-- 
-- takeStoneFromOther :: Game -> Position -> Game 
-- takeStoneFromOther game@(PlayerOne, (one,two)) pos = 
                                             -- let curStore = (fst one) 
                                                 -- aux oldX oldY ((st1, []), (st2, [])) count = (PlayerTwo, ((st1, oldX), (st2, oldY))) 
                                                 -- aux oldX oldY ((st1, (x:xs)), (st2, (y:ys))) count = if count == pos then (PlayerTwo, (((st1 + x + y), oldX ++ (0:xs)), (st2, oldY ++ (0:ys)))) else aux (x:oldX) (0:oldY) ((st1, xs), (st2, ys)) (count + 1) 
                                            -- in aux [] [] (one, two) 0   
-- 
-- takeStoneFromOther game@(PlayerTwo, (one,two)) pos = 
                                             -- let curStore = (fst one) 
                                                 -- aux oldX oldY ((st1, []), (st2, [])) count = (PlayerOne, ((st1, oldX), (st2, oldY))) 
                                                 -- aux oldX oldY ((st1, (x:xs)), (st2, (y:ys))) count = if count == pos then (PlayerTwo, ((st1, oldX ++ (0:xs)), ((st2 + x + y), oldY ++ (y:ys)))) else aux (x:oldX) (0:oldY) ((st1, xs), (st2, ys)) (count + 1) 
                                            -- in aux [] [] (one, two) 0   
------------------------------------------------------------------------------------------
--Story 4: possibleMoves

possibleMoves :: Game -> [Game]
possibleMoves game@(_,((_,one),_)) =
    let len = length one - 1
    in [move game pos | pos <- [0..len]]

------------------------------------------------------------------------------------------

-- Story 5: Pretty-print a game into a string as a Mancala board
prettyPrintGame :: Game -> String
prettyPrintGame (player, ((storeOne, pitsOne), (storeTwo, pitsTwo))) =
    let boardWidth = length pitsOne
        topRow = "      " ++ unwords (map show (reverse pitsTwo)) ++ "   "
        middleRow = "P2: " ++ show storeTwo ++ replicate (1 + boardWidth * 2) ' ' ++ "P1: " ++ show storeOne
        bottomRow = "      " ++ unwords (map show pitsOne) ++ "   "
        currentPlayer = "Player: " ++ show player
    in unlines [topRow, middleRow, bottomRow, currentPlayer]

------------------------------------------------------------------------------------------
-- story 8: Game State and Winner

type Winner = Player
data GameState = Ongoing | Win Winner | Tie deriving Show

hasGameEnded :: Game -> Bool
hasGameEnded (_, ((_,one), (_,two)))
    | all (== 0) one    = True
    | all (== 0) two    = True
    | otherwise         = False

whoWon :: Game -> Maybe Winner
whoWon game@(_, ((s1,_), (s2,_)))
    | s1 > s2       = Just PlayerOne
    | s1 < s2       = Just PlayerTwo
    | otherwise     = Nothing

currGameState :: Game -> GameState
currGameState game =
    if not (hasGameEnded game) then Ongoing
    else case (whoWon game) of
        Just w  -> Win w
        Nothing -> Tie

------------------------------------------------------------------------------------------
-- Story 9 & 10: Guess Moves

whoWillWin :: Game -> GameState
whoWillWin game@(player, _) =
    let moves = possibleMoves game
        outcomes = [currGameState g | g <- moves]
    in getBest outcomes (Win $ otherPlayer player) player

getBest :: [GameState] -> GameState -> Player -> GameState
getBest [] gameState _ = gameState
getBest (Tie:xs) gameState player = getBest xs Tie player
getBest ((Win winner):xs) gameState player
    | winner == player = Win winner
    | otherwise        = getBest xs gameState player

-------------------------------------------------------------------------------------------

-- Story 11 & 12: Text Format & readGame (read text format)

readGame :: String -> Game
readGame input =
    let linesInput = lines input
    in if length linesInput /= 4
        then error "Invalid input: Expected exactly 4 lines."
        else
            let [line1, line2, line3, line4] = linesInput
                -- Parse pitsTwo and pitsOne
                pitsTwo = case traverse readMaybe (words line1) of
                    Just pits -> reverse pits
                    Nothing -> error "Invalid input: Pits (line 1) must be integers."
                pitsOne = case traverse readMaybe (words line3) of
                    Just pits -> pits
                    Nothing -> error "Invalid input: Pits (line 3) must be integers."
                -- Parse line2 for P1 and P2 store values
                wordsLine2 = words line2
                _ = if length wordsLine2 /= 4 || head wordsLine2 /= "P1:" || wordsLine2 !! 2 /= "P2:"
                        then error "Invalid input: Line 2 must follow the format 'P1: <int> P2: <int>'."
                        else ()
                p1Store = case readMaybe (wordsLine2 !! 1) of
                    Just s -> s
                    Nothing -> error "Invalid input: P1 store value must be an integer."
                p2Store = case readMaybe (wordsLine2 !! 3) of
                    Just s -> s
                    Nothing -> error "Invalid input: P2 store value must be an integer."
                -- Parse line4 for player
                wordsLine4 = words line4
                _ = if length wordsLine4 /= 2 || head wordsLine4 /= "Player:"
                        then error "Invalid input: Line 4 must follow the format 'Player: PlayerOne' or 'Player: PlayerTwo'."
                        else ()
                player = case last wordsLine4 of
                    "PlayerOne" -> PlayerOne
                    "PlayerTwo" -> PlayerTwo
                    _ -> error "Invalid input: Player must be 'PlayerOne' or 'PlayerTwo'."
            in (player, ((p1Store, pitsOne), (p2Store, pitsTwo)))

-------------------------------------------------------------------------------------------
-- Story 13: showGame (takes a game and puts it into string format)

showGame :: Game -> String
showGame (player, ((storeOne, pitsOne), (storeTwo, pitsTwo))) =
    let -- Line 1: P2 pits (in reverse)
        line1 = unwords (map show $ reverse pitsTwo)
        -- Line 2: stores
        line2 = "P1: " ++ show storeOne ++ " P2: " ++ show storeTwo
        -- Line 3: P1 pits
        line3 = unwords (map show pitsOne)
        -- Line 4: current Player
        line4 = "Player: " ++ show player
    in unlines [line1, line2, line3, line4]

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

getStones :: Game -> Position -> Maybe Int
getStones (player, (one,two)) pit =
    let (_,side) = sideOf player (one,two)
        val  = safeBangBang (zip side [0..]) pit
    in if val == 0 then Nothing else Just val

