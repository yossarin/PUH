import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative ((<$>))
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.List (isInfixOf, intercalate)
import System.Environment (getArgs)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LT

-- (1) a simple utility for measuring the time an action spent executing.
time :: IO () -> IO Double
time f = do
  before <- (realToFrac <$> getPOSIXTime) :: IO Double
  f
  after <- (realToFrac <$> getPOSIXTime) :: IO Double
  return $ after - before

-- (2) 
-- (a) a simple grep-like utility
grep :: String -> FilePath -> IO ()
grep pattern file = putStrLn =<< intercalate "\n" <$> filter (isInfixOf pattern) <$> lines <$> readFile file

-- (b) grep with args
grepWithArgs :: IO ()
grepWithArgs = do
  [p,f] <- getArgs
  grep p f

-- (c) lazy grep
grepText :: L.Text -> FilePath -> IO ()
grepText pattern file = LT.putStrLn =<< L.intercalate (L.pack "\n") <$> filter (L.isInfixOf pattern) <$> L.lines <$> LT.readFile file 

