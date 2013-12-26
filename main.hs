{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}

import Hakit
import qualified Hakit.Http as H
-- import qualified System.Environment as SE  
import qualified Data.List as L
import qualified Data.Map as M
import qualified Control.Concurrent as CC
import qualified Data.Text as T
import qualified Control.Concurrent.MVar as MV
import qualified Control.Exception as E
import qualified Data.IORef as Ref

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

serviceNames :: Instances -> [T.Text]
serviceNames ins =
    let nameList = map fst . M.toList $ nameToNodes ins
        unique = M.fromList $ map (\x -> (x, ())) nameList 
    in map fst $ M.toList unique

-- Maps serviceNames to instances
data Instances = Is {
    nameToNodes :: M.Map T.Text [Instance]
} deriving (Eq, Show)

instance DocValLike Instances where
    toDocVal i = d [
            "nameToNodes" .- (dm . map (\(k,v) -> (k, d $ map d v)) . M.toList $ nameToNodes i)
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

connectedHandler :: Ref.IORef Instances -> IO H.Resp
connectedHandler instances = do
    inst <- Ref.readIORef instances
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

connectHandler :: Document -> Ref.IORef Instances -> IO H.Resp
connectHandler dat instances = do
    Ref.atomicModifyIORef' instances upd
    return $ H.setBody ok H.resp
    where
        inst :: Instance
        inst = fromDocVal . d . set "connectTime" 1 $ dat
        upd :: Instances -> (Instances, ())
        upd i =
            let servName = serviceName inst
                mapp = nameToNodes i
                exists = M.member servName mapp
            in if exists
                then (Is (M.adjust (\xs -> inst:xs) servName mapp), ())
                else (Is (M.insert servName [inst] mapp ), ())

{--------------------------------------------------------------------
  Main loop.  
--------------------------------------------------------------------}

main = do
    putStrLn "Starting ORC"
    instances <- Ref.newIORef . Is $ M.fromList []
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
