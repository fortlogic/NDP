module PPM (readPPM) where

import qualified Data.ByteString as BS
import Graphics.Netpbm

readPPM :: FilePath -> IO (Maybe PPM)
readPPM file = do
  parseResult <- parsePPM <$> BS.readFile file
  return $ case parseResult of
             Right ([ppm], Nothing) -> Just ppm
             _ -> Nothing
