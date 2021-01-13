{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.ByteCode.Name
  ( Name
  , fromBuilder
  , toBuilder
  )
where

import qualified Data.ByteString.Builder as B
-- import Data.Monoid ((<>))
-- import qualified Data.Name as Name
-- import qualified Data.Utf8 as Utf8
-- import Data.Word (Word8)
-- import qualified Data.List as List
-- import qualified Data.Char as Char
-- import qualified Data.Index as Index

-- import qualified Elm.ModuleName as ModuleName
-- import qualified Elm.Package as Pkg
-- import qualified Elm.String as ES
-- import qualified Elm.Float as EF

  
-- NAME


newtype Name =
  Name { toBuilder :: B.Builder }


-- CONSTRUCTORS

fromBuilder :: B.Builder -> Name
fromBuilder = Name
