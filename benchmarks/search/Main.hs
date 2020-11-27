import System.Environment
import Search
import BenchUtils

main :: IO ()
main = do
    [cmdsFile] <- getArgs
    cmds <- readCommands cmdsFile
    let doCmd cmd = timeIt (commandLabel cmd) $ runCommand cmd
    runSearchM (mapM_ doCmd cmds) mempty

