module System.PIO (
  binToNum,
  octToNum,
  hexToNum
  ) where

import Numeric(readInt,readOct,readHex)

readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 f1 f2
  where
    f1 n = elem n ['0','1']
    f2 '0' = 0
    f2 '1' = 1


-- | an unsigned number in binary notation.
binToNum :: (Eq a, Num a) => String -> a
binToNum s = case readBin s of
  (x,""):_  -> x
  _         -> error "binToNum: parse error" 


-- | an unsigned number in octal notation.
octToNum :: (Eq a, Num a) => String -> a
octToNum s = case readOct s of
  (x,""):_  -> x
  _         -> error "octToNum: parse error" 


-- | an unsigned number in hexadecimal notation.
-- Both upper or lower case letters are allowed.
hexToNum :: (Eq a, Num a) => String -> a
hexToNum s = case readHex s of
  (x,""):_  -> x
  _         -> error "hexToNum: parse error" 
  

