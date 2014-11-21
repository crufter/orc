{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE DeriveGeneric          #-}

module Orc(
    startOrc,
    Instances,
    newOrc,
    router
) where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString as B
import qualified Userv.Http as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Control.Concurrent as CC
import qualified Data.Text as T
import qualified Control.Concurrent.MVar as MV
import qualified Control.Exception as E
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TV
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as C
import qualified System.Console.CmdArgs.Implicit as Con

{--------------------------------------------------------------------
  Types and helpers.  
--------------------------------------------------------------------}

data Endpoint = Endpoint {
    alias :: T.Text,        -- Ie. "register"
    path :: T.Text          -- Ie. "whatever/you/wish/reg"
} deriving (Eq, Show, Generic)

instance ToJSON Endpoint
instance FromJSON Endpoint

data Instance = Instance {
    address         :: T.Text,                  -- "http://myservice.com" or "https://myservice.com:8081"
    serviceName     :: T.Text,                  -- Servicename, eg. "image-resize", "comment"
    instanceName    :: T.Text,                  -- A unique identifier of the service instance
    endpoints       :: M.Map T.Text Endpoint,   -- Map from endpoint alias to Endpoint
    weight          :: Int
} deriving (Eq, Show, Generic)

instance ToJSON Instance
instance FromJSON Instance

data Instances = Instances {
    byServiceName   :: M.Map T.Text [T.Text],   -- Maps serviceNames to instanceName
    services        :: M.Map T.Text Instance    -- Maps instanceNames to instances
} deriving (Eq, Show, Generic)

newInstances :: Instances
newInstances = Instances (M.fromList []) (M.fromList []) 

instance ToJSON Instances
instance FromJSON Instances

data EndpointStat = EndpointStat {
    calls           :: Int, 
    failures        :: Int,
    responseTimes   :: [(Int,Int)]                  -- last X response time, format: (call timestamp, response time in milliseconds)
}

data InstanceStat = InstanceStat {
    firstPing       :: Int,                         -- timeStamp
    lastPing        :: Int,                         -- timeStamp
    pingFailedNum   :: Int,
    pingAllNum      :: Int,
    endpointStats   :: M.Map T.Text EndpointStat    -- timeStamp  
}

{--------------------------------------------------------------------
  Helpers.  
--------------------------------------------------------------------}

insert :: Instance -> Instances -> Instances
insert i is =
    if M.member iName $ (services is)
        then is
        else if M.member sName $ byServiceName is
            then Instances  (M.adjust (\xs -> iName:xs) sName $ byServiceName is)
                            updated
            else Instances  (M.insert sName [iName] $ byServiceName is)
                            updated
    where
        sName = serviceName i
        iName = instanceName i
        updated = M.insert iName i (services is)

-- So inefficient, temporal
remove :: T.Text -> Instances -> Instances
remove iName is =
    Instances   (M.adjust (\xs -> filter (\x -> x /= iName) xs) iName $ byServiceName is)
                (M.delete iName $ services is)

--update :: T.Text -> (Instance -> Instance) -> Instances -> Instances
--update instanceName updFunc ins = 

serviceNames :: Instances -> [T.Text]
serviceNames ins = map fst . M.toList $ byServiceName ins

{--------------------------------------------------------------------
  Handlers.  
--------------------------------------------------------------------}

pingHandler = object ["pong" .= True]

unrecHandler = return $ object ["error" .= "Unrecognized path"]

connectedHandler :: TV.TVar Instances -> IO Value
connectedHandler instances = do
    inst <- TV.readTVarIO instances
    return $ toJSON inst

ok = encode $ object ["ok" .= True]

disconnectHandler :: TV.TVar Instances -> Value -> IO Value
disconnectHandler instances doc = do
    STM.atomically $ TV.modifyTVar' instances del
    return $ object []
    where
        iName = case doc of
            Object hashmap  -> case HM.lookup "istanceName" hashmap of
                Just x      -> case x of
                    String x    -> x
                    otherwise   -> "instanceName is not a string"
                Nothing     -> error "instanceName not found"
            otherwise       -> error "Input not a map"
        del = remove iName

proxyHandler :: TV.TVar Instances -> Caller -> Value -> T.Text -> T.Text -> IO Value
proxyHandler instances caller v sName endpoint = do
    inst <- TV.readTVarIO instances
    case M.lookup sName $ byServiceName inst of
        Nothing     -> serviceNotFound
        Just names  -> case M.lookup (head names) $ services inst of
            Nothing -> error . T.unpack $ T.concat ["inconsistent state for ", head names]
            Just i  -> case M.lookup endpoint $ endpoints i of
                Nothing -> endpointNotFound
                Just e  -> proxy i e
    where
        proxy :: Instance -> Endpoint -> IO Value
        proxy i e = do
            resp <- H.req (B.concat $ map TE.encodeUtf8 [address i, "/", path e]) v
            case resp of
                Just rsp    -> return rsp
                Nothing     -> return $ object ["error" .= "Something went wrong"] -- obv. fix this later
        -- 400ish errors
        serviceNotFound =
            let msg = T.concat["service ", sName, " not found"]
            in return $ object ["error" .= msg]
        endpointNotFound =
            let msg = T.concat ["endpoint ", endpoint, " for service ", sName, " not found"]
            in return $ object ["error" .= msg]

connectHandler :: TV.TVar Instances -> Value -> IO Value
connectHandler instances dat = do
    -- Strangely the nonstrict version gives the same
    -- effect as when an IORef stores the error?!
    STM.atomically $ TV.modifyTVar' instances upd
    return $ object []
    where
        inst :: Instance
        -- @todo remove this cruft
        inst = case decode $ encode dat of
            Just i  -> i 
            Nothing -> error "Could not decode JSON"
        upd :: Instances -> Instances
        upd = insert inst

{--------------------------------------------------------------------
  Main loop.  
--------------------------------------------------------------------}

router :: Orc -> B.ByteString -> Value -> IO Value 
router (Orc instances caller) path' v =
    let correct xs = if length xs > 0
            then if xs!!0 == ""
                then tail xs
                else xs
            else xs
        path = C.split '/' path'
    in case correct path of
        ["connect"]             -> connectHandler instances v
        ["connected"]           -> connectedHandler instances
        ["disconnect"]          -> disconnectHandler instances v
        ["ping"]                -> return pingHandler
        [a, b]                  -> proxyHandler instances caller v (TE.decodeUtf8 a) (TE.decodeUtf8 b)
        otherwise               -> unrecHandler
        --
        -- ["connectExample"]      ->
        -- ["disconnectExample"]   ->
        -- ["endpointList"]        -> 

type Caller = B.ByteString -> Value -> IO (Maybe Value)
data Orc = Orc (TV.TVar Instances) Caller

newOrc :: IO Orc
newOrc = do
    instances <- TV.newTVarIO newInstances
    return $ Orc instances H.req

startOrc :: Orc -> Int -> IO ()
startOrc instances portNum = do 
    let err :: Show e => e -> IO Value
        err e = return $ object ["error" .= show e]
        exRouter :: E.SomeException -> IO Value
        exRouter e = err e
        routerEx = \path v -> E.catch (router instances path v) exRouter
    H.serve portNum routerEx