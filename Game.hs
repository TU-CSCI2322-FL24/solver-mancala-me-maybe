data Divets = Store Player Stones | Pit Stones deriving Show

type Stones = Int
type Player = Int
type Board  = [Divets]

makeBoard :: Player -> Int -> Board
makeBoard player k = (Store player 0):[Pit 4 | _ <- [1..k]]

defaultBoard :: Int -> (Board,Board)
defaultBoard k = (makeBoard 1 k, makeBoard 2 k)

move :: Player -> (Board, Board) -> Int -> (Board,Board)
move player (one,two) pit =
    | location > (length one) = error "Put "
    where loaction = pit + 1
