{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE BangPatterns           #-}

import Hakit
import qualified Hakit.Http as H
-- import qualified System.Environment as SE  
import qualified Data.List as L
import qualified Data.Map as M
import qualified Control.Concurrent as CC
import qualified Data.Text as T
import qualified Control.Concurrent.MVar as MV
import qualified Control.Exception as E
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TV

{--------------------------------------------------------------------
  Types and helpers.  
--------------------------------------------------------------------}

data Endpoint = E {
    alias :: T.Text,        -- Ie. "register"
    path :: T.Text,         -- Ie. "whatever/you/wish/reg"
    method :: T.Text
} deriving (Eq, Show)

-- Horrible boilerplate, extend hakit to deal with it.
instance DocValLike Endpoint where
    toDocVal e = d [
            "alias"     .- alias e,
            "path"      .- path e,
            "method"    .- method e
        ]
    fromDocVal d =
        let doc = toMap d
        in E    (toString $ get "alias" doc)
                (toString $ get "path" doc)
                (toString $ get "method" doc)

data Instance = I {
    connectTime     :: Integer,                 -- Timestamp when the connection was established
    address         :: T.Text,                  -- "http://myservice.com" or "https://myservice.com:8081"
    serviceName     :: T.Text,                  -- Servicename, eg. "image-resize", "comment"
    instanceName    :: T.Text,                  -- A unique identifier of the service instance
    endpoints       :: M.Map T.Text Endpoint    -- Map from endpoint alias to Endpoint
} deriving (Eq, Show)

instance DocValLike Instance where
    toDocVal i = d [
            "connectTime"   .- connectTime i,
            "address"       .- address i,
            "serviceName"   .- serviceName i,
            "instanceName"  .- instanceName i,
            "endpoints"     .- (d . map (\(k, v) -> (k, d v)) . M.toList $ endpoints i)
        ]
    fromDocVal d =
        let doc = toMap d
        in I    (toInt      $ get "connectTime" doc)
                (toString   $ get "address" doc)
                (toString   $ get "serviceName" doc)
                (toString   $ get "instanceName" doc)
                (M.fromList . map (\(k, v) -> (k, (fromDocVal v)::Endpoint)) . M.toList . toMap $ get "endpoints" doc)

-- Maps serviceNames to instances
data Instances = Is {
    byServiceName   :: M.Map T.Text [T.Text],
    services        :: M.Map T.Text Instance
} deriving (Eq, Show)

insert :: Instance -> Instances -> Instances
insert i is =
    if M.member iName $ (services is)
        then is
        else if M.member sName $ byServiceName is
            then Is     (M.adjust (\xs -> iName:xs) sName $ byServiceName is)
                        updated
            else Is     (M.insert sName [iName] $ byServiceName is)
                        updated
    where
        sName = serviceName i
        iName = instanceName i
        updated = M.insert iName i (services is)

-- So inefficient, temporal
remove :: T.Text -> Instances -> Instances
remove iName is =
    Is  (M.adjust (\xs -> filter (\x -> x /= iName) xs) iName $ byServiceName is)
        (M.delete iName $ services is)

--update :: T.Text -> (Instance -> Instance) -> Instances -> Instances
--update instanceName updFunc ins = 

serviceNames :: Instances -> [T.Text]
serviceNames ins = map fst . M.toList $ byServiceName ins

instance DocValLike Instances where
    toDocVal i = d [
            "byServiceName" .- (dm . map (\(k,v) -> (k, d $ map d v)) . M.toList $ byServiceName i)
        ]

{--------------------------------------------------------------------
  Handlers.  
--------------------------------------------------------------------}

pingHandler = return $ H.setBody pong H.resp
    where
        pong = toJSON $ dm ["pong" .- True]

unrecHandler = return $ H.setBody unrecognizedPath H.resp
    where
        unrecognizedPath = toJSON $ dm ["error" .- "Unrecognized path"]

connectedHandler :: TV.TVar Instances -> IO H.Resp
connectedHandler instances = do
    inst <- TV.readTVarIO instances
    return $ H.setBody (toJSON . toMap $ toDocVal inst) H.resp

ok = toJSON $ dm ["ok" .- True]

err :: Show e => e -> IO H.Resp
err e =
    let body = toJSON $ dm ["error" .- show e]
    in return $ H.setBody body H.resp

{--------------------------------------------------------------------
  Connect handler.  
--------------------------------------------------------------------}

--connectValidator = [
--        "serverName"    .- True,
--        "instanceName"  .- True
--    ]

connectHandler :: Document -> TV.TVar Instances -> IO H.Resp
connectHandler dat instances = do
    -- Strangely the nonstrict version gives the same
    -- effect as when an IORef stores the error?!
    STM.atomically $ TV.modifyTVar' instances upd
    return $ H.setBody ok H.resp
    where
        inst :: Instance
        inst = fromDocVal . d . set "connectTime" 1 $ dat
        upd :: Instances -> Instances
        upd = insert inst

{--------------------------------------------------------------------
  Main loop.  
--------------------------------------------------------------------}

main = do
    putStrLn "Starting ORC"
    instances <- TV.newTVarIO $ Is (M.fromList []) (M.fromList [])
    let handler req = case H.path req of
            -- ["disconnect"] -> disconnectHandler instances
            ["connected"]   -> connectedHandler instances
            ["connect"]     -> connectHandler (H.params req) instances
            ["ping"]        -> pingHandler
            otherwise       -> unrecHandler
        exHandler :: E.SomeException -> IO H.Resp
        exHandler e = err e
        handlerEx = \req -> E.catch (handler req) exHandler
    H.startServer 8081 handlerEx
