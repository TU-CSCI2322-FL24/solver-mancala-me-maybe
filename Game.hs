module Game where
import Debug.Trace
import Data.Maybe
import Text.Read
import System.IO
import System.Environment
import System.Console.GetOpt

-----------------------------------------------------
-- Story 1: Types

type Position = Int
type Stones = Int

data Player = PlayerOne | PlayerTwo deriving (Show, Eq, Ord)
type Row = (Int, [Int])
type Board  = (Row, Row)
type Game = (Player, Board)
type Move = (Player, Position)

data Winner = Win Player | Tie deriving (Show,Eq)
data GameState = Ongoing | Winner Winner deriving Show
-----------------------------------------------------
-- Story 1 & 3: Game and Move

makeGame :: Int -> Game
makeGame k =
    let pits = [4 | _ <- [1..k]]
    in (PlayerOne,((0, pits),(0, pits)))

move :: Game -> Move -> Maybe Game
move game@(p,board) (pl, pit)
    | pl /= p       = Nothing
    | otherwise     = do
        stones <- getStones game pit
        Just (movePieces ((p, pickUp game pit), stones) p (pit + 1))

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

possibleMoves :: Game -> [Move]
possibleMoves game@(pl, ((_,one),_)) =
    let len = length one - 1
    in [(pl, pos) | pos <- [0..len]]

possibleGames :: Game -> [Game]
possibleGames game = mapMaybe (move game) (possibleMoves game)

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
    | all (== 0) one                    = True
    | all (== 0) two                    = True
    | storeOne > ((length one) * 8)     = True
    | storeTwo > ((length two) * 8)     = True
    | otherwise                         = False

whoWon :: Game -> Maybe Winner
whoWon game@(_, ((s1,one), (s2,two)))
    | not (hasGameEnded game)       = Nothing
    | sc1 > sc2                     = Just (Win PlayerOne)
    | sc1 < sc2                     = Just (Win PlayerTwo)
    | sc1 == sc2                    = Just Tie
    | otherwise                     = Nothing
    where sc1 = sum one + s1
          sc2 = sum two + s2

currGameState :: Game -> GameState
currGameState game =
    maybe Ongoing Winner (whoWon game)

------------------------------------------------------------------------------------------
-- Story 9: Guess Moves
whoWillWin :: Game -> Player -> Winner
whoWillWin game pl =
    let status = whoWon game
    in case status of
        Just winner -> winner
        Nothing -> compareListOutcome pl [whoWillWin g pl | g <- (possibleGames game)]

---------------------------------------------------------------------
-- Story 10: Best Move 

bestMove :: Game -> Maybe Move 
bestMove game@(pl, (one,two)) =
    let ((x, pos):xs) = zip (possibleGames game) [0 ..]
        aux [] pl (pos, result) = (pl,pos)
        aux ((y, p):ys) pl (pos, result)
            | result == (Win pl)                                            = (pl, pos)
            | newResult == (Win pl)                                         = (pl, p)
            | (newResult == Tie) && (result == (Win (otherPlayer pl)))      = aux ys pl (p, newResult)
            | otherwise                                                     = aux ys pl (pos, result)
            where newResult = whoWillWin y pl
    in if hasGameEnded game then Nothing else Just (aux xs pl (pos, (whoWillWin x pl)))
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

verbosePrint :: Player -> Winner -> String 
verbosePrint pl outcome = "Best Outcome for " ++ (show pl) ++ " is " ++ (show outcome) 

putBestMove :: Game -> Bool -> IO ()
putBestMove game@(pl, _) isVerbose = do 
    putStrLn $ show (bestMove game)
    if isVerbose 
    then putStrLn $ verbosePrint pl (whoWillWin game pl)
    else putStr $ ""  

putGoodMove :: Game -> Int -> Bool -> IO ()
putGoodMove game@(pl, _) depth isVerbose = do 
   putStrLn $ show (goodMove game depth)
   if isVerbose
   then putStrLn $ verbosePrint pl (whoWillWin game pl) 
   else putStr $ ""

putMove :: Game -> Int -> Bool -> IO ()
putMove game@(pl, _) pos isVerbose = do
   let gameState = fromJust (move game (pl, pos)) 
   if isVerbose 
   then do 
       putStrLn $ prettyPrintGame gameState
       putStrLn $ verbosePrint pl (whoWillWin gameState pl)
   else  putStrLn $ showGame gameState 

getNumber :: Flag -> Int 
getNumber (OutMove x) = read x   
getNumber (Depth x) = read x 

sortFlag :: [Flag] -> [Flag] 
sortFlag flags = 
    let aux [] sorted = sorted 
        aux (f:fs) sorted = 
           let rec [] = [f]
               rec (s:ss) = if (f < s) then (f:s:ss) else s:(rec ss)  
           in aux fs (rec sorted)   
    in aux flags []  

printCurrentGame game@(pl,_) =
   do putStrLn $ prettyPrintGame game 
      putStrLn $ ((show pl) ++ " Turn") 

computerMove :: Game -> Int -> Maybe Move 
computerMove game depth = if depth == 0 then bestMove game else goodMove game depth

playGame :: Game -> Int -> IO () 
playGame game@(PlayerOne, _) depth = 
    do printCurrentGame game 
       putStr $ "Please input your move: "
       hFlush stdout
       pit <- getLine 
       if getStones game ((read pit :: Int) - 1) == Nothing 
       then do 
            putStrLn $ "Invalid move try again" 
            playGame game depth 
       else 
            let nextGame = fromJust (move game (PlayerOne, ((read pit :: Int) - 1)))
            in do 
               if hasGameEnded nextGame 
               then do 
                    putStrLn $ showGame nextGame
                    putStrLn $ show (fromJust (whoWon nextGame))
               else playGame nextGame depth 


playGame game@(PlayerTwo, _) depth = 
    do printCurrentGame game 
       let movement = (computerMove game depth)
       case movement of 
            Nothing -> do 
                       putStrLn $ showGame game 
                       putStrLn $ show (fromJust (whoWon game))
            Just (pl, pos) -> case (move game (pl, pos)) of 
                                  Nothing -> do 
                                             putStrLn $ showGame game
                                             putStrLn $ show (fromJust (whoWon game))
                                  Just nGame -> case (whoWon nGame) of 
                                                    Nothing -> playGame nGame depth
                                                    Just result -> do 
                                                                   putStrLn $ showGame nGame 
                                                                   putStrLn $ show result 
main = 
     do args <- getArgs 
        let (flags, inputs, errors) = getOpt Permute options args 
        putStrLn $ show (flags, inputs, errors) 
        if Help `elem` flags
        then putStrLn $ usageInfo "Game [options] [filename] Interactive Mancala" options
        else
           do let fName = if null inputs then "games/baseGame.txt" else head inputs
                  sortedFlags = sortFlag flags
              game <- loadGame fName 
              if null sortedFlags then putBestMove game False else flagGame game sortedFlags (Verbose `elem` flags)                                 

-------------------------------------------------------------------------------------------
-- Story 17: Evaluate Rating

type Rating = Int

rateGame :: Game -> Int
rateGame game
    | Just (Win PlayerOne) <- whoWon game = 1000 -- If PlayerOne has won
    | Just (Win PlayerTwo) <- whoWon game = -1000 -- If PlayerTwo has won
    | otherwise = evaluateGame game -- Fallback to your evaluation logic

evaluateGame :: Game -> Rating
evaluateGame (currentPlayer, ((x, pitsOne), (y, pitsTwo))) =
    let result = (emptyPits pitsOne 0 `div` 2) - (emptyPits pitsTwo 0 `div` 2) + if currentPlayer == PlayerOne then 2 else -2
    in result + x - y

emptyPits :: [Int] -> Int -> Int
emptyPits [] count = count
emptyPits (x:xs) count =
    if x == 0 then emptyPits xs (count + 1)
    else emptyPits xs count

-------------------------------------------------------------------------------------------
-- Story 18: Cut-Off Depth

maxOrMin :: (Foldable t, Ord a) => Player -> t a -> a
maxOrMin pl = case pl of
    PlayerOne -> maximum
    PlayerTwo -> minimum

whoMightWin :: Game -> Int -> Rating
whoMightWin game@(pl, _) depth
    | hasGameEnded game || depth == 0   = rateGame game
    | otherwise                         = maxOrMin pl [whoMightWin g (depth - 1) | g <- possibleGames game]

goodMove :: Game -> Int -> Maybe Move
goodMove game@(pl, _) depth
    | hasGameEnded game || depth == 0   = Nothing
    | otherwise                         = Just $ snd (maxOrMin pl [(whoMightWin (fromJust g) (depth - 1), m) | m@(pl, pos) <- possibleMoves game, let g = move game m, isJust g])

-------------------------------------------------------------------------------------------
-- Story 19: Lazy Story 18

maxLazy :: Player -> [(Rating, Move)] -> (Rating, Move) -> (Rating, Move)
maxLazy pl [] compX = compX
maxLazy pl ((rx, rm):xs) (rComp, rMove)
    | rx == 1000        = (rx, rm)
    | rx > rComp        = maxLazy pl xs (rx, rm)
    | otherwise         = maxLazy pl xs (rComp, rMove)

minLazy :: Player -> [(Rating, Move)] -> (Rating, Move) -> (Rating, Move)
minLazy pl [] compX = compX
minLazy pl ((rx, rm):xs) (rComp, rMove)
    | rx == -1000       = (rx, rm)
    | rx < rComp        = minLazy pl xs (rx, rm)
    | otherwise         = minLazy pl xs (rComp, rMove)

maxOrMinLazy :: Player -> [(Rating, Move)] -> Maybe (Rating, Move)
maxOrMinLazy pl [] = Nothing
maxOrMinLazy pl (first:list) = case pl of
    PlayerOne -> Just $ maxLazy pl list first
    PlayerTwo -> Just $ minLazy pl list first

whoMightWinEarly :: Game -> Int -> Maybe (Rating, Move)
whoMightWinEarly game@(pl, _) depth
    | hasGameEnded game || depth == 0   = Nothing
    | otherwise                         = maxOrMinLazy pl (catMaybes [whoMightWinEarly g (depth - 1) | m@(pl, pos) <- possibleMoves game, let Just g = move game m])

------------------------------------------------------------------------------------
-- Story 21 - 25

flagGame :: Game -> [Flag] -> Bool -> IO ()
flagGame game [] isVerbose = do putStr " "  
flagGame game (f:fs) isVerbose 
   | f == NoDepth = do 
                      if Interactive `elem` fs 
                      then do 
                           playGame game 0
                      else do
                             putBestMove game isVerbose 
                             flagGame game fs isVerbose 
   | (show f) == "Depth" = do 
                       if Interactive `elem` fs 
                       then playGame game (getNumber f)
                       else do 
                            putGoodMove game (getNumber f) isVerbose
                            flagGame game fs isVerbose
    
   | (show f) == "OutMove" = do 
                               putMove game ((getNumber f) - 1) isVerbose
                               flagGame game fs isVerbose
   | f == Verbose = flagGame game fs isVerbose
   | f == Interactive = do
                          playGame game 4    
   | otherwise = error "incorrect flag inputed"
   

data Flag = Help | NoDepth | Depth String | OutMove String | Verbose | Interactive deriving Eq 

instance Show Flag where
   show Help = "Help" 
   show NoDepth = "No Depth" 
   show (Depth a) = "Depth" 
   show (OutMove a) = "OutMove" 
   show Verbose = "Best Outcome for "
   show Interactive = "Interactive"

instance Ord Flag where
   compare x y = compare (rnk x) (rnk y) 
       where 
           rnk Help = 1 
           rnk NoDepth = 2
           rnk (Depth a) = 3
           rnk (OutMove a) = 4
           rnk Verbose = 5
           rnk Interactive = 6      

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg NoDepth) "Print out the best move with no cut-off depth"
          , Option ['d'] ["depth"] (ReqArg Depth "<depth>") "Print out the best move from <depth>" 
          , Option ['m'] ["move"] (ReqArg OutMove "<move>") "Print out the resulting board from <move> to stdout" 
          , Option ['v'] ["verbose"] (NoArg Verbose) "Print both the move and a description of how good it is: win, lose, tie, or a rating"
          , Option ['i'] ["interactive"] (NoArg Interactive) "Start a new game and plays against the computer. Compatible with -d flag."
          ]
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
compareOutcome pl new current
    | new == Win (otherPlayer pl)                               = current
    | new == Win pl                                             = new
    | (new == Win pl) && (current == Win (otherPlayer pl))      = new
    | otherwise                                                 = current
