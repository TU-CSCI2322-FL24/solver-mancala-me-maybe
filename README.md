# solver-mancala-me-maybe
solver-mancala-me-maybe created by GitHub Classroom  
Jordan Carter  
Paul Borrego  
Cody Estes  
Fess Myhre  

RULES
Each player can only put stones in there own stores  
Each player can only move stones out of their own pits  
Placing your last stone in a players own store gives that player an extra turn  
Placing your last stone in an empty pit you own and the opposing pit has stones in it then you take the all stones from each side and place them in your own store  

2 store's, 1 for each player
6 pits

Type Stones = Int  
Data Player = Int  
Data Divets = Store Player Stones | Pits Player Stones  

Idea 1  
PlayerOneBoard [Divets]  
PlayerTwoBoard [Divets]  

Idea 2  
Board [(Divets,Divets)]  
--Problems  
Going seperate ways can be tough
