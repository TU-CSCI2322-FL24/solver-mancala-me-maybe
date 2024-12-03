module Game where

import Debug.Trace
import Data.Maybe
import Text.Read
import System.Environment

-- story one
type Position = Int
type Stones = Int

data Player = PlayerOne | PlayerTwo deriving (Show, Eq)
type Row = (Int, [Int])
type Board  = (Row, Row)
type Game = (Player, Board)
type Move = (Player, Position)

data Winner = Win Player | Tie deriving (Show,Eq)
data GameState = Ongoing | Winner Winner deriving Show

--setup
-----------------------------------------------------
-- Story 1 & 3: Game and Move

makeGame :: Int -> Game
makeGame k =
    let pits = [4 | _ <- [1..k]]
    in (PlayerOne,((0, pits),(0, pits)))

move :: Game -> Position -> Maybe Game
move game@(p,board) pit = do
                          stones <- getStones game pit
                          Just (movePieces ((p, pickUp game pit), stones) p (pit + 1))
{-
    let stones = getStones game pit
    in case stones of
        Nothing -> Nothing 
        Just stones -> Just (movePieces ((p, pickUp game pit), stones) p (pit + 1))
-}
------------------------------------------------------
--move functions

pickUp :: Game -> Position -> Board
pickUp (p, ((s1,one), (s2,two))) pit =
    let (begin,(_:end)) = if p == PlayerOne then splitAt pit one else splitAt pit two
        newRow = begin ++ [0] ++ end
    in if p == PlayerOne then ((s1,newRow),(s2,two)) else ((s1,one),(s2,newRow))


movePieces :: (Game,Stones) -> Player -> Position -> Game
movePieces (game@(playersTurn,((_,one),_)), stones) playersSide pos
    | stones <= 0                = checkLanding (game,stones) playersSide (length one) --(otherPlayer playersTurn, (one, two))
    | playersTurn == playersSide = movePieces (getCorrect game stones pos) (otherPlayer playersSide) 0
    | otherwise                  = movePieces (getIncorrect game stones pos) (otherPlayer playersSide) 0

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
    | playerSide /= playerTurn = (playerTurn,b) --you ended in your own store
    | otherwise = (otherPlayer playerSide,b)
checkLanding ((playerTurn,b),stones) playerSide len
    | playerSide == playerTurn = (otherPlayer playerTurn,b) --you ended on the other side
    | otherwise = --you ended on your side
        let position = len + stones
            check    = getStones (playerTurn,b) position
            otherPosition = len + 1 - position
            other    = getStones (playerSide,b) otherPosition
        in case check of
            Nothing -> (playerSide, b)
            Just 1  -> case other of
                Nothing -> (playerSide, b)
                Just y -> (playerSide, addToStore playerTurn b y position otherPosition)
            Just x  -> (playerSide, b)

addToStore :: Player -> Board -> Stones -> Position -> Position -> Board
addToStore PlayerOne ((s1,one),(s2,two)) val pos1 pos2 =
    let (before,_:after) = splitAt pos1 one
        (oB,_:oA)        = splitAt pos2 two
    in ((s1 + val + 1,before ++ [0] ++ after),(s2,(oB ++ [0] ++ oA)))
addToStore PlayerTwo ((s1,one),(s2,two)) val pos1 pos2 =
    let (before,_:after) = splitAt pos1 two
        (oB,_:oA)        = splitAt pos2 one
    in ((s1, oB ++ [0] ++ oA),(s2 + val + 1,before ++ [0] ++ after))

------------------------------------------------------------------------------------------
--Story 4: possibleMoves

possibleMoves :: Game -> [Game]
possibleMoves game@(_,((_,one),_)) =
    let len = length one - 1
    in catMaybes [move game pos | pos <- [0..len]]

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
-- Story 8: Game State and Winner


hasGameEnded :: Game -> Bool
hasGameEnded (_, ((storeOne,one), (storeTwo,two)))
    | all (== 0) one    = True
    | all (== 0) two    = True
    | storeOne > ((length one) * 8) = True
    | storeTwo > ((length two) * 8) = True 
    | otherwise         = False

whoWon :: Game -> Maybe Winner
whoWon game@(_, ((s1,_), (s2,_)))
    | not (hasGameEnded game)   = Nothing
    | s1 > s2                   = Just (Win PlayerOne)
    | s1 < s2                   = Just (Win PlayerTwo)
    | s1 == s2                  = Just Tie

currGameState :: Game -> GameState
currGameState game =
    maybe Ongoing Winner (whoWon game)

------------------------------------------------------------------------------------------
-- Story 9: Guess Moves
whoWillWin :: Game -> Player -> Winner

-- depth first search 
whoWillWin game pl = 
   let aux game = traceShow ((show (hasGameEnded game)) ++ " : " ++ (show game)) $ if (hasGameEnded game) then fromJust (whoWon game) else compareListOutcome pl [aux g | g <- (possibleMoves game)]   
   in aux game 

-- breadth first search
{-- whoWillWin game pl = findOutcome (possibleMoves game) pl (fromMaybe (Win (otherPlayer pl)) (whoWon game)) 
 
findOutcome :: [Game] -> Player -> Winner -> Winner
findOutcome [] pl curOutcome = curOutcome 
findOutcome games pl curOutcome = 
   let newOutcome = bestOutcome [fromJust (whoWon g) | g <- games, hasGameEnded g] pl
       aux [] = [] 
       aux (g:gs) = if not (hasGameEnded g) then (possibleMoves g) ++ (aux gs) else aux gs 
       outcome = compareOutcome pl (fromMaybe (Win (otherPlayer pl)) newOutcome) curOutcome 
   in traceShow (length games) $ if outcome == (Win pl) then outcome else findOutcome (aux games) pl outcome  

bestOutcome :: [Winner] -> Player -> Maybe Winner
bestOutcome [] pl = Nothing    
bestOutcome (w:ws) pl = 
    let aux [] best = Just best
        aux (w:ws) best = traceShow (("Game Result: ") ++ (show w)) $ if best == (Win pl) then Just best else if ((w == (Win pl)) || ((w == Tie) && (best == (Win (otherPlayer pl))))) then aux ws w else aux ws best
    in aux ws w 
--}

-- original Stragety
{-- whoWillWin :: Game -> Winner
whoWillWin game@(player, _) = ongoingToWinner (possibleMoves game) player


ongoingToWinner :: [Game] -> Player -> Winner
ongoingToWinner games player =
    let moves         = allMoves games
        main
        outcomes      = [(g,currGameState g) | g <- moves]
        (ongoing,bestState) = foldr sortGameState ([],Win (otherPlayer player)) outcomes
    in if bestState == Win player then Win player
       else if length ongoing == 0 then bestState
            else ongoingToWinner ongoing (otherPlayer player)

sortGameState :: (Game,GameState) -> ([Game],Winner) -> ([Game], Winner)
sortGameState (g,Ongoing) (a,b) = ((g:a), b)
sortGameState ((player,_), Winner Tie) (a,b) = if b == Win player then (a,b) else (a, Tie)
sortGameState ((player,_), Winner (Win p)) (a,b) = if player == p then (a, Win p) else (a,b)

allMoves :: [Game] -> [Game]
allMoves [] = []
allMove (g:gs) = traceShow ((show g) ++ " and " ++ (show gs)) $ (possibleMoves g) ++ (allMoves gs)
--}
-------------------------------------------------------------------------------------------
-- Story 10: Best Move 

bestMove :: Game -> Game 
bestMove game@(pl, (one,two)) = 
    let (x:xs) = possibleMoves game 
        aux [] pl (gameState, result) = gameState
        aux (y:ys) pl (gameState, result) = 
            let newResult = whoWillWin y pl   
            in if result == (Win pl) then gameState else if newResult == (Win pl) then y else if (newResult == Tie) && (result == (Win (otherPlayer pl))) then aux ys pl (y, newResult) else aux ys pl (gameState, result) 
    in aux xs pl (x, (whoWillWin x pl)) 

-- Keep for Sprint 3 
{-- bestMove :: Game -> Game
bestMove game@(pt, (one,two)) =
    let moves = possibleMoves (pt, (one,two))
        repeats = stopAtStore moves pt
    in if (null repeats) then findBestMove moves (pt, (one,two)) else findBestMove repeats (pt, (one,two))

stopAtStore :: [Game] -> Player -> [Game]
stopAtStore moves pt = [(mPt, state) | (mPt, state) <- moves, mPt == pt]

findBestMove :: [Game] -> Game -> Game
findBestMove (move:moves) orgState =
    let aux [] (bestM, bestR) = bestM
        aux (x:xs) (bestM, bestR) =
            let rank = getRank x orgState
            in if rank > bestR then aux xs (x, rank) else aux xs (bestM, bestR)
    in aux moves (move, (getRank move orgState))

getRank :: Game -> Game -> Int
getRank (nPt, (nOne,nTwo)) (PlayerOne, (one,two)) =
    let captureScore = (fst nOne) - (fst one)
        otherSideStone = (- (sum [new - cur | (new, cur) <- (zip (snd nTwo) (snd two))]))
        opCapPotent = opCap (nPt, (nOne, nTwo))
        allowOpRepeat = if (nPt == PlayerOne) || (not (canOpRepeat (nPt, (nOne,nTwo)))) then 0 else (-3)
    in captureScore + otherSideStone + opCapPotent + allowOpRepeat

getRank (nPt, (nOne, nTwo)) (PlayerTwo, (one,two)) =
    let captureScore = (fst nTwo) - (fst two)
        otherSideStone = (-(sum [new - cur | (new, cur) <- (zip (snd nOne) (snd one))]))
        opCapPotent = opCap (nPt, (nOne, nTwo))
        allowOpRepeat = if (nPt == PlayerTwo) || (not (canOpRepeat (nPt, (nOne, nTwo)))) then 0 else (-3)
    in captureScore + otherSideStone + opCapPotent + allowOpRepeat

opCap :: Game -> Int
opCap (PlayerOne, (one,two)) =
    let moves = possibleMoves (PlayerOne, (one,two))
    in foldr (\ (p, (o,t)) recVal -> max ((fst o) - (fst one)) recVal) 0 moves

opCap (PlayerTwo, (one, two)) =
    let moves = possibleMoves (PlayerTwo, (one,two))
    in foldr (\ (p, (o,t)) recVal -> max (((fst t) - (fst two)) - 1) recVal) 0 moves


canOpRepeat :: Game -> Bool
canOpRepeat (pt, (one,two)) =
    let moves = possibleMoves (pt, (one, two))
    in foldr (\(p,state) recVal -> (p == pt) || recVal) False moves
--} 
-------------------------------------------------------------------------------------------
-- Story 11 & 12: Text Format & readGame (read text format)

readGame :: String -> Game
readGame input =
    let linesInput = lines input
    in case linesInput of
        [] -> error "Invalid input: 0 lines provided, expected 4."
        [_] -> error "Invalid input: 1 line provided, expected 4."
        [_, _] -> error "Invalid input: 2 lines provided, expected 4."
        [_, _, _] -> error "Invalid input: 3 lines provided, expected 4."
        [line1, line2, line3, line4] -> 
            -- parse pitsTwo and pitsOne
            let pitsTwo = case traverse readMaybe (words line1) of
                    Just pits -> reverse pits
                    Nothing -> error "Invalid input: P2 Pits (line 1) must be integers."
                pitsOne = case traverse readMaybe (words line3) of
                    Just pits -> pits
                    Nothing -> error "Invalid input: P1 Pits (line 3) must be integers."
                -- parse line2 for P1 and P2 store values
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
                -- parse line4 for player
                wordsLine4 = words line4
                _ = if length wordsLine4 /= 2 || head wordsLine4 /= "Player:"
                        then error "Invalid input: Line 4 must follow the format 'Player: PlayerOne' or 'Player: PlayerTwo'."
                        else ()
                player = case last wordsLine4 of
                    "PlayerOne" -> PlayerOne
                    "PlayerTwo" -> PlayerTwo
                    _ -> error "Invalid input: Player must be 'PlayerOne' or 'PlayerTwo'."
            in (player, ((p1Store, pitsOne), (p2Store, pitsTwo)))
        _ -> error "Invalid input: More than 4 lines provided, expected exactly 4."

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
-- Story 14: IO Actions

writeGame :: Game -> FilePath -> IO ()
writeGame game filePath = do
    writeFile filePath (showGame game)

loadGame :: FilePath -> IO Game
loadGame filePath = do
    contents <- readFile filePath
    return (readGame contents)

putBestMove :: Game -> IO ()
putBestMove game = do
    putStrLn "Old Game:"
    putStrLn $ showGame game
    putStrLn "Best Move:"
    putStrLn $ showGame (bestMove game)

main :: IO ()
main = do
    args <- getArgs
    let filePath = if length args == 1 then head args else error "Incorrect number of arguments."
    game <- loadGame filePath
    putBestMove game

-------------------------------------------------------------------------------------------
-- Universal Helper Functions

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

compareListOutcome :: Player -> [Winner] -> Winner
compareListOutcome pl (w:ws) = 
   let aux [] outcome = outcome
       aux (x:xs) outcome = aux xs (compareOutcome pl x outcome) 
   in aux ws w 
   
compareOutcome :: Player -> Winner -> Winner -> Winner
compareOutcome pl new current = if new == (Win (otherPlayer pl)) then current else if new == (Win pl) then new else if (new == (Win pl)) && (current == (Win (otherPlayer pl))) then new else current 

