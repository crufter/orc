import Orc
import qualified Control.Concurrent.STM.TVar as TV

main = do
    putStrLn "Starting ORC"
    instances <- TV.newTVarIO newInstances
    orc instances 8081
    
