-- story one
type Stones = Int
type Player = Int
data Divets = Store Player Stones | Pit Stones deriving Show

type Row  = [Divets]
type Board = (Row, Row)
type Game = Board -- will proboably need to include more than just the board

makeRow :: Player -> Int -> Row
makeRow player k = Store player 0:[Pit 4 | _ <- [1..k]]

defaultBoard :: Int -> Board
defaultBoard k = (makeRow 1 k, makeRow 2 k)

move :: Player -> Board -> Int -> Board
move player (one,two) pit
    | location > length one = error "Put "
    where location = pit + 1

-- story two
type Winner = Player

win :: Game -> Winner
win game = undefined
