{-# LANGUAGE OverloadedStrings #-}

-- Module      : Main
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Applicative
import           Control.Error
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import           Data.List                  (sort)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Monoid                ((<>))
import qualified System.APT.Package         as Pkg
import qualified System.APT.Package.Control as Ctl
import           System.APT.Types
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "apteryx"
    [ testGroup "parse control fields"
        [ testCase "package name" $
            parse "Package" "libvarnishapi1"

        , testCase "version number" $
            parse "Version" "4.0.0~~tp1-1~precise"

        , testCase "architecture" $
            parse "Architecture" "amd64"

        , testCase "unknown field" $
            parse "Unknown" "unknown value."

        , testCase "singleline description" $
            parse "Description" "Expanded synopsis\n"

        , testCase "multiline description" $
            parse "Description"
                "single-line synopsis\n\
                \ A more expand description covering\n\
                \ Two extra lines.\n"
        ]

    , testGroup "deserialise package description"
        [ testCase "package name" $
            control entName "libvarnishapi1"

        , testCase "version number" $
            control entVers (Vers "0.1.0+123~1" [0, 1, 0, 123, 1])

        , testCase "amd64 architecture" $
            control entArch Amd64
        ]

    , testCase "descending version ordering" $
        let mk = Vers ""
            a  = mk [0]
            b  = mk [0, 1, 0, 123, 1]
            c  = mk [1, 2, 3]
            d  = mk [23, 0, 12, 0]
         in [a, b, c, d] @=? sort [b, d, c, a]
    ]

parse :: ByteString -> ByteString -> Assertion
parse k v = Just v @=?
    ((hush . Ctl.parse $ k <> ": " <> v) >>= Map.lookup (CI.mk k))

control :: (Eq a, Show a) => (Entry () -> a) -> a -> Assertion
control f x = Right x @=? (f <$> g)
  where
    g = Ctl.parse m >>= Pkg.fromControl

    m = "Package: libvarnishapi1\n\
        \Version: 0.1.0+123~1\n\
        \Maintainer: Bob Jones <bob.jones@gmail.com>\n\
        \Architecture: amd64\n\
        \Depends: runit\n\
        \Description: A brief synopsis about the package\n\
        \ A continued and expand description about the package.\n"
