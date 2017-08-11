import Control.Concurrent (forkIO)
import Control.Exception (handle)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L

-- Provided by the 'zlib' package on http://hackage.haskell.org/
import Codec.Compression.GZip (compress)

-- NOTE readline needs GUN readline to be installed at OS, so replace it with mine.

readline :: String -> IO (Maybe [Char])
readline note = do
  putStrLn note
  input <- getLine
  return (Just input)

-- NOTE folk thread sample: read file then spawn thread to compress it.

folkThead = do
    maybeLine <- readline "Enter a file to compress> "
    case maybeLine of
      Nothing -> return ()      -- user entered EOF
      Just "" -> return ()      -- treat no name as "want to quit"
      Just name -> do
        -- NOTE should modify like this
           handle ((\e -> print e) :: IOError -> IO ()) $ do
             content <- L.readFile name
             forkIO (compressFile name content)
             return ()
           main
  where compressFile path = L.writeFile (path ++ ".gz") . compress
