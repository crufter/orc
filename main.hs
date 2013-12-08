{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}

import Hakit
import qualified Hakit.Http as H
import qualified System.Environment as SE  
import qualified Data.List as L
import qualified Data.Map as M
import qualified Control.Concurrent as CC
import qualified Data.Text as T
import qualified Control.Concurrent.MVar as MV
import qualified Data.IORef as Ref

data Instance = Instance {
    lastHeartbeat   :: Integer,
    name            :: T.Text,
    endpoints       :: T.Text
}

data ConnectedInstances = ConnInst {
    nameToNodes :: M.Map T.Text Instance
} 

data Test = Test {
    x :: Integer
}

main = do
    putStrLn "Starting ORC"
    t <- Ref.newIORef $ Test 0
    let inc :: Test -> (Test, ())
        inc tes = (Test $ x tes + 1, ())
    args <- SE.getArgs
    let handler req = case H.path req of
                ["inc"]     -> do
                        ret <- Ref.readIORef t
                        _ <- Ref.atomicModifyIORef' t inc
                        return $ H.setBody (show $ x ret) H.resp
                ["ping"]    -> return $ H.setBody "pong" H.resp
                otherwise   -> return $ H.setBody "Unrecognized path" H.resp
    H.startServer 8081 handler
