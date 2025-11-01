module Util.Aeson where 

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Aeson.Encode.Pretty as JSONPretty
import qualified Data.ByteString.Lazy.Char8 as BS

writeFileJSON :: ToJSON a => FilePath -> a -> IO ()
writeFileJSON fp v = TIO.writeFile fp (encodePretty v)

encodePretty :: ToJSON a => a -> T.Text
encodePretty = decodeUtf8 . BS.toStrict . JSONPretty.encodePretty
