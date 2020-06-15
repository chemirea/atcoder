{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Data.ByteString.Char8 (ByteString, putStrLn)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as Map
import           Prelude               hiding (getLine, print, putStrLn, readLn,
                                        unwords, words)

class (Read a, Show a) => CPString a where
  cpRead :: String -> a
  cpShow :: a -> ByteString
  cpRead = read
  cpShow = BS.pack . show

instance CPString ByteString where
  cpRead = BS.pack
  cpShow = id

instance CPString String where
  cpRead = id
  cpShow = BS.pack

instance CPString Char where
  cpRead [a] = a
  cpShow = BS.pack . pure

instance CPString Int

instance CPString Integer

instance CPString Float

instance CPString Double

instance CPString Rational

instance CPString a => CPString [a]

readLine :: CPString a => IO [a]
readLine = fmap (cpRead . BS.unpack) . BS.words <$> BS.getLine

printLn :: CPString a => a -> IO ()
printLn = putStrLn . cpShow

main :: IO ()
main = do
  putStrLn "Hello AtCoder!"
