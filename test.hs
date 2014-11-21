{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}

module Test where

import Test.HUnit
import Orc
import Data.Aeson

eq a b = assertBool ((show a) ++ " vs " ++ (show b)) $ a == b

testConnect = TestCase $ do
	orc <- newOrc
	let inst = object [
			"address" .= "127.0.0.1:6060",
			"serviceName" .= "wrapper",
			"instanceName" .= "wrapper 1234",
			"endpoints" .= object [
				"wrap" .= object [
					"alias" .= "wrap",
					"path" .= "wrap"
				]
			],
			"weight" .= 1
		]
	router orc "/connect" inst
	x <- router orc  "/connected" $ object []
	let exp = object [
			"byServiceName" .= object [
				"wrapper" .= ["wrapper 1234"]
			],
			"services" .= object [
				"wrapper 1234" .= inst
			]
		]
	eq x exp

tests = TestList [
    TestLabel "testConnect"         testConnect
    ]