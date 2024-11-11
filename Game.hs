import Debug.Trace

data Divets = Store Player Stones | Pit Pos Stones deriving (Show, Eq)

type Stones = Int
type Player = Int
type Pos = Int
type Board  = ([Divets],[Divets])

makeBoard :: Int -> Board
makeBoard k = 
            let aux (x,y) 0 = (x,y) 
                aux (x,y) n = 
                            let pos = (k - n) + 1
                            in aux (((Pit pos 4):x), ((Pit pos 4):y)) (n - 1) 
            in aux ([Store 1 0], [Store 2 0]) k 

                                                   
move player (one, two) pit = 
                           let side = case player of 
                                      1 -> one 
                                      2 -> two 
                               aux [(Store pl st)] = error "Pit does not exist"
                               aux ((Pit ps st):xs) = if ps == pit then st else aux xs
                           in makeMove side (one, two) pit (aux side) 

makeMove player (one, two) pit st = 
                                  let size = length one
                                      aux [(Store pl st)] n = (Store pl (st + 1)):[] 
                                      aux ((Pit pos st):xs) n = if n == 0 then ((Pit pos st):xs) else if pos == pit then ((Pit pos 0):(aux xs n)) else if pos < pit then ((Pit pos (st + 1)):(aux xs (n - 1))) else ((Pit pos st):(aux xs n)) 
                                 in if (pit - st) < 0 then if player == one then makeMove two ((aux one st), two) size ((-(pit - st))) else makeMove one (one, (aux two st)) size (-(pit - st)) else if player == one then ((aux one st), two) else (one, (aux two st))                                                                 
                              
