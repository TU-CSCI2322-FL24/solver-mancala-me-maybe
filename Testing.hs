module Testing where
import Game 
import Data.Semigroup
import Dictionaries
import Control.Monad.Extra
import Test.Grader.Core
import Test.Grader.Rubric
import Test.Grader.Eval
import Test.Grader.Tests
import Test.Grader.Parsing.Util
import Test.Grader.Parsing
import Data.List
import Control.Monad.Trans.RWS

testScore :: Grader String
testScore = assess "score" 3 $ do
    check "that score is defined" $ shouldBeDefined "score"

runTests :: Int -> Bool -> IO ()
runTests verb force = do
    let a = runGrader tree
    format <- makeFormat verb force "projectDesc.yaml"
    runRWST a () format 
    return ()
