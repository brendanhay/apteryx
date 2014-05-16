{-# LANGUAGE OverloadedStrings #-}

-- Module      : System.APT.Package.Control
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.APT.Package.Control where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import           Data.CaseInsensitive             (CI)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Monoid
import           Data.String
import           System.APT.Types

parse :: ByteString -> Either Error (Map (CI ByteString) ByteString)
parse = fmapL invalidPackage . parseOnly (Map.fromList <$> many1 field)

-- | Each field consists of the field name followed by a colon and then the
-- data/value associated with that field.
field :: Parser (CI ByteString, ByteString)
field = do
    skipComments
    k <- key <* skipWhile whitespace
    v <- value k
    return (k, v)
    <?> "field"

-- | Field names are not case-sensitive, but it is usual to capitalize the field
-- names using mixed case.
key :: Parser (CI ByteString)
key = fromString <$> ((:) <$> satisfy start <*> name) <?> "key"
  where
    name = manyTill (satisfy (`elem` valid)) (char ':')

    -- Field names must not begin with the comment character, #, nor
    -- with the hyphen character, -.
    start c = c /= '#' || c /= '-'

    -- The field name is composed of US-ASCII characters excluding control
    -- characters, space, and colon.
    -- i.e., characters in the ranges 33-57 and 59-126, inclusive.
    valid = map toEnum ([33..57] ++ [59..126])

-- | Determine the field value parser to use based on the parsed key.
value :: CI ByteString -> Parser ByteString
value k =
    case k of
        "description" -> multiline False
        _             -> simple

-- | The field, including its value, must be a single line.
-- Folding of the field is not permitted.
--
-- This is the default field type if the definition of the field does not
-- specify a different type.
simple :: Parser ByteString
simple = takeLine <?> "simple field"

-- | The value of a multiline field may comprise multiple continuation lines.
--
-- The first line of the value, the part on the same line as the field
-- name, often has special significance or may have to be empty. Other lines are
-- added following the same syntax as the continuation lines of the folded
-- fields. Whitespace, including newlines, is significant in the values of
-- multiline fields.
multiline :: Bool -- ^ Signifies the initial line follow ':' should be empty.
          -> Parser ByteString
multiline e = mconcat <$> ((:) <$> first <*> many cont) <?> "multiline field"
  where
    first = if e
        then endOfLine >> return ""
        else line <?> "initial line"

    cont = mappend "\n " <$> (satisfy whitespace *> line)
        <?> "multiline continuation"

line :: Parser ByteString
line = skipComments *> takeLine <* skipComments <?> "line"

-- | Lines starting with # without any preceding whitespace are comments lines
-- that are only permitted in source package control files
-- (debian/control). These comment lines are ignored, even between two
-- continuation lines. They do not end logical lines.
skipComments :: Parser ()
skipComments = option () . void $ char '#' >> takeLine

takeLine :: Parser ByteString
takeLine = takeTill eol <* (endOfLine <|> endOfInput)

-- | Whitespace must not appear inside names (of packages, architectures, files
-- or anything else) or version numbers, or between the characters of
-- multi-character version relationships.
whitespace :: Char -> Bool
whitespace c = c == ' ' || c == '\t'

-- | Paragraph separators (empty lines) and lines consisting only of spaces and
-- tabs are not allowed within field values or between fields. Empty lines in field
-- values are usually escaped by representing them by a space followed by a dot.
eol :: Char -> Bool
eol c = c == '\n' || c == '\r'
