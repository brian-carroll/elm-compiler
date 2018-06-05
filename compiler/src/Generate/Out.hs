module Generate.Out
  ( Output(..)
  )
  where


import qualified Elm.Name as N
import qualified Data.ByteString.Builder as B

data Output
  = None
  | Some N.Name [N.Name] B.Builder

