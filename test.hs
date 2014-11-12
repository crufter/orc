{-# LANGUAGE OverloadedStrings      #-}

module Test where

import Test.HUnit
import Orc
import Data.Aeson
import qualified Control.Concurrent.STM.TVar as TV

testConnect = TestCase $ do
	instances <- TV.newTVarIO newInstances
	let inst = object $ [
			"address" .= "127.0.0.1:6060",
			"serviceName" .= "wrapper",
			"instanceName" .= "wrapper 1234",
			"endpoints" .= [
				"wrap" .= [
					"alias": "wrap",
					"path": "wrap"
				]
			]
		]
	handler instances "/connect" inst
	x <- handler instances  "/connected" $ object []
	assertBool "response" $ x == (object $ [
			"byServiceName" .= [
				"wrapper" .= ["wrapper 1234"]
			],
			"services" .= [
				"wrapper 1234" .= inst
			]
		])

tests = TestList [
    TestLabel "testConnect"         testConnect
    ]