{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import RIO
import RIO.Process
import System.Environment
import System.Exit

-- Here comes the boilerplate!
data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  }
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

main :: IO ()
main = do
  -- more boilerplate, could use runSimpleApp instead
  lo <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          }
     in runRIO app run

run :: RIO App ()
run = do
  args <- liftIO getArgs
  case args of
    [] -> do
      logError "You need to provide a command to run"
      liftIO System.Exit.exitFailure
    x:xs -> proc x xs runProcess_