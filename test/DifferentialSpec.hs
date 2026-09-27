{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The token stream of the current lexer must equal the token stream of the lexer as of
-- 537a43a7 ("StockCLexer") on every input and every way of splitting the input into pieces,
-- including where both fail. Set JSON_STREAM_DIFF_CORPUS to a directory to also compare
-- every @.json@ file under it.
module DifferentialSpec (spec) where

import           Control.Monad               (filterM, forM_)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BSC
import           Data.List                   (isSuffixOf, sort)
import           Data.Word                   (Word8)
import           System.Directory            (doesDirectoryExist, listDirectory)
import           System.Environment          (lookupEnv)
import           Test.Hspec
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Gen         (unGen)
import           Test.QuickCheck.Random      (mkQCGen)

import qualified Data.JsonStream.CLexer      as Patched
import           Data.JsonStream.TokenParser (TokenResult (..))
import qualified StockCLexer                 as Stock

-- | Feed both lexers the same pieces in lockstep. Right is the number of equal elements.
-- Left describes the first difference in an element or in where a stream ends.
compareStreams :: [BS.ByteString] -> Either String Int
compareStreams pieces = go 0 (Stock.tokenParser first) (Patched.tokenParser first) rest
  where
    (first, rest) = case pieces of
      []       -> (BS.empty, [])
      (p : ps) -> (p, ps)
    go :: Int -> TokenResult -> TokenResult -> [BS.ByteString] -> Either String Int
    go !n (PartialResult a na) (PartialResult b nb) ps
      | a == b = go (n + 1) na nb ps
      | otherwise = Left ("element " ++ show n ++ ": stock " ++ show a ++ ", patched " ++ show b)
    go !n (TokMoreData ka) (TokMoreData kb) (p : ps) = go n (ka p) (kb p) ps
    go !n (TokMoreData _) (TokMoreData _) [] = Right n
    go !n TokFailed TokFailed _ = Right n
    go !n a b ps = Left ("after " ++ show n ++ " elements, " ++ show (length ps)
                         ++ " pieces left: stock " ++ end a ++ ", patched " ++ end b)
    end (PartialResult el _) = "element " ++ show el
    end (TokMoreData _)      = "needs more data"
    end TokFailed            = "failed"

agree :: [BS.ByteString] -> Property
agree pieces = case compareStreams pieces of
  Right _  -> property True
  Left msg -> counterexample msg False

-- | Every two-piece split, then one byte per piece.
agreeAtEverySplit :: BS.ByteString -> Property
agreeAtEverySplit bs = conjoin
  ([ counterexample ("split at " ++ show i) (agree [a, b])
   | i <- [0 .. BS.length bs], let (a, b) = BS.splitAt i bs ]
   ++ [counterexample "one byte per piece" (agree (singleBytes bs))])

singleBytes :: BS.ByteString -> [BS.ByteString]
singleBytes = map BS.singleton . BS.unpack

chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf k bs
  | BS.null bs = []
  | otherwise = let (a, b) = BS.splitAt k bs in a : chunksOf k b

-- | Random piece sizes, zero included, sometimes led by an empty piece as runParser does.
randomPieces :: Int -> BS.ByteString -> Gen [BS.ByteString]
randomPieces maxPiece bs = do
  leadEmpty <- arbitrary
  sizes <- infiniteListOf (frequency [(1, pure 0), (6, choose (1, 8)), (3, choose (1, maxPiece))])
  let cut _ rest | BS.null rest = []
      cut (s : ss) rest = let (a, b) = BS.splitAt s rest in a : cut ss b
      cut [] rest = [rest]
  pure ((if leadEmpty then (BS.empty :) else id) (cut sizes bs))

forAllPieces :: BS.ByteString -> Property
forAllPieces bs = forAll (randomPieces (max 1 (BS.length bs)) bs) agree

-- Generators ---------------------------------------------------------------------------

-- | Bytes that drive the lexer through its states, with malformed and non-ASCII bytes.
lexerByte :: Gen Word8
lexerByte = frequency
  [ (6, elements (BS.unpack "{}[]\",:\\"))
  , (4, elements (BS.unpack "truefalsn"))
  , (4, elements (BS.unpack "0123456789-+.eE"))
  , (3, elements whitespaceBytes)
  , (1, choose (0, 31))
  , (1, choose (127, 255))
  , (1, elements [0x85, 0xa0])
  , (1, arbitrary)
  ]

whitespaceBytes :: [Word8]
whitespaceBytes = [32, 9, 10, 13, 11, 12]

fragment :: Gen BS.ByteString
fragment = frequency
  [ (8, BS.singleton <$> lexerByte)
  , (2, elements ["true", "false", "null", "tru", "fals", "nul", "truex", "null]"])
  , (2, elements ["\\\"", "\\\\", "\\/", "\\n", "\\u00e9", "\\ud83d\\ude00", "\\u", "\\x"])
  , (2, elements ["\xc3\xa9", "\xe2\x82\xac", "\xf0\x9f\x98\x80", "\xc3", "\xff\xfe"])
  , (2, number)
  , (1, whitespace)
  ]

jsonish :: Gen BS.ByteString
jsonish = BS.concat <$> listOf fragment

anyBytes :: Gen BS.ByteString
anyBytes = BS.pack <$> listOf arbitrary

whitespace :: Gen BS.ByteString
whitespace = BS.pack <$> frequency
  [ (6, pure [])
  , (4, resize 4 (listOf (elements whitespaceBytes)))
  , (1, resize 3 (listOf (elements [0x85, 0xa0, 0x1c, 0x00])))
  ]

number :: Gen BS.ByteString
number = frequency
  [ (6, wellFormed)
  , (2, elements [ "-", "1.", ".5", "1e", "1e+", "--1", "1.2.3", "01", "+1", "-0", "0.0", "1E400", "1e-400" ])
  , (2, elements [ "999999999999999999", "1000000000000000000", "9223372036854775807"
                 , "9223372036854775808", "-9223372036854775808", "-9223372036854775809"
                 , "123456789.123456789", "0.000000000000000001" ])
  , (1, BSC.pack <$> listOf1 (elements "0123456789-+.eE"))
  ]
  where
    wellFormed = do
      sign <- elements ["", "-"]
      int <- oneof [pure "0", digits1]
      frac <- oneof [pure "", ("." <>) <$> digits1]
      ex <- oneof [pure "", do e <- elements ["e", "E"]; s <- elements ["", "+", "-"]; d <- digits1; pure (e <> s <> d)]
      pure (BS.concat [sign, int, frac, ex])
    digits1 = BSC.pack <$> sized (\n -> choose (1, max 1 (min 40 n)) >>= flip vectorOf (elements ['0' .. '9']))

stringContent :: Gen BS.ByteString
stringContent = BS.concat <$> listOf (frequency
  [ (10, BSC.pack <$> listOf1 (elements (filter (`notElem` ("\"\\" :: String)) [' ' .. '~'])))
  , (3, elements ["\\\"", "\\\\", "\\/", "\\b", "\\f", "\\n", "\\r", "\\t", "\\u0041", "\\u00e9", "\\ud83d\\ude00", "\\uDC00"])
  , (2, elements ["\xc5\xbe", "\xe2\x82\xac", "\xf0\x9f\x98\x80"])
  , (1, BS.singleton <$> choose (0, 31))
  , (1, BS.singleton <$> choose (128, 255))
  ])

jsonString :: Gen BS.ByteString
jsonString = do
  body <- stringContent
  close <- frequency [(12, pure "\""), (1, pure ""), (1, pure "\\")]
  pure ("\"" <> body <> close)

-- | Well-formed JSON with every whitespace byte isspace accepts, and sometimes dropped,
-- doubled or misplaced separators.
json :: Gen BS.ByteString
json = sized (\n -> value (min 6 (n `div` 10)))
  where
    value depth = do
      v <- if depth <= 0 then scalar else frequency [(3, scalar), (1, array (depth - 1)), (1, object (depth - 1))]
      pad v
    pad v = do
      a <- whitespace
      b <- whitespace
      pure (a <> v <> b)
    scalar = oneof [number, jsonString, elements ["true", "false", "null"]]
    separator s = frequency [(10, pad s), (1, pure ""), (1, pure (s <> s)), (1, elements [",", ":"])]
    array depth = do
      items <- scale (`div` 2) (listOf (value depth))
      sep <- separator ","
      pure ("[" <> BS.intercalate sep items <> "]")
    object depth = do
      fields <- scale (`div` 2) (listOf (do k <- jsonString; c <- separator ":"; v <- value depth; pure (k <> c <> v)))
      sep <- separator ","
      pure ("{" <> BS.intercalate sep fields <> "}")

-- | Well-formed JSON with random byte insertions, deletions, replacements and truncation.
mutated :: Gen BS.ByteString
mutated = do
  base <- json
  k <- choose (1, 4)
  go k base
  where
    go :: Int -> BS.ByteString -> Gen BS.ByteString
    go 0 bs = pure bs
    go k bs = do
      i <- choose (0, BS.length bs)
      b <- lexerByte
      let (pre, post) = BS.splitAt i bs
      bs' <- elements
        [ pre <> BS.singleton b <> post
        , pre <> BS.drop 1 post
        , pre <> BS.singleton b <> BS.drop 1 post
        , pre
        ]
      go (k - 1) bs'

small :: Gen BS.ByteString -> Gen BS.ByteString
small g = (\bs -> BS.take 400 bs) <$> resize 30 g

-- Fixed cases ---------------------------------------------------------------------------

-- | Piece sizes that put every structure of a long document across a boundary.
pieceSizes :: [Int]
pieceSizes = [1, 2, 3, 5, 7, 16, 61, 509, 4093, 32767, 32768, 32769]

agreeForSizes :: BS.ByteString -> Expectation
agreeForSizes = agreeForSizesIn (maxBound : pieceSizes)

agreeForSizesIn :: [Int] -> BS.ByteString -> Expectation
agreeForSizesIn sizes bs = forM_ sizes $ \k ->
  either (\msg -> expectationFailure ("piece size " ++ show k ++ ": " ++ msg)) (const (pure ()))
    (compareStreams (chunksOf k bs))

longString :: BS.ByteString
longString = BS.concat
  ["[\"", BSC.replicate 32766 'a', "\\\"", BSC.replicate 32767 'b', "\\\\", BSC.replicate 40000 'c'
  , "\\u00e9\xc5\xbe\", \"", BSC.replicate 70000 'd', "\"]"]

deepNesting :: Int -> BS.ByteString
deepNesting n = BS.concat [BSC.replicate n '[', "{\"a\":", BSC.replicate n '{', BSC.replicate n '}', "}", BSC.replicate n ']']

deepObjects :: Int -> BS.ByteString
deepObjects n = BS.concat (replicate n "{\"k\":[") <> "1" <> BS.concat (replicate n "]}")

-- | Longer than the Haskell side's digit limit for a number split across pieces, so both
-- fail. Small pieces and one whole piece are left out: the first sums the held parts on every
-- piece and the second parses all the digits into an Integer, both quadratic.
longNumber :: BS.ByteString
longNumber = "[" <> BSC.replicate 200010 '7' <> "]"

-- Corpus --------------------------------------------------------------------------------

jsonFilesUnder :: FilePath -> IO [FilePath]
jsonFilesUnder dir = do
  names <- sort <$> listDirectory dir
  let paths = map ((dir ++ "/") ++) names
  dirs <- filterM doesDirectoryExist paths
  nested <- concat <$> mapM jsonFilesUnder dirs
  pure (filter (".json" `isSuffixOf`) paths ++ nested)

corpusSpec :: Maybe FilePath -> Spec
corpusSpec Nothing = it "compares a corpus when JSON_STREAM_DIFF_CORPUS is set" $ pendingWith "unset"
corpusSpec (Just dir) = do
  files <- runIO (jsonFilesUnder dir)
  forM_ files $ \path -> it path $ do
    bs <- BS.readFile path
    agreeForSizes bs
    forM_ [1 .. 8 :: Int] $ \seed ->
      either (\msg -> expectationFailure ("random pieces, seed " ++ show seed ++ ": " ++ msg)) (const (pure ()))
        (compareStreams (unGen (randomPieces 65536 bs) (mkQCGen seed) 30))

spec :: Spec
spec = describe "Lexer parity with 537a43a7" $ do
  prop "random bytes, random pieces" $ forAll anyBytes forAllPieces
  prop "lexer-directed bytes, random pieces" $ forAll jsonish forAllPieces
  prop "JSON with whitespace and separator variants, random pieces" $ forAll json forAllPieces
  prop "mutated JSON, random pieces" $ forAll mutated forAllPieces
  prop "every split point and single bytes" $
    forAll (oneof [small anyBytes, small jsonish, small json, small mutated]) agreeAtEverySplit
  prop "strings with escapes at every split point" $
    forAll (small jsonString) agreeAtEverySplit
  it "long strings with escapes across piece boundaries" $ agreeForSizes longString
  it "deep array and object nesting" $ agreeForSizes (deepNesting 100000)
  it "deep mixed nesting" $ agreeForSizes (deepObjects 20000)
  it "a long number across piece boundaries" $ agreeForSizes ("[-" <> BSC.replicate 5000 '7' <> ".5e+" <> BSC.replicate 300 '1' <> "]")
  it "a number past the digit limit" $ agreeForSizesIn [4093, 32768] longNumber
  it "whitespace bytes 0 to 255 between tokens" $
    forM_ [0 .. 255 :: Word8] $ \b ->
      agreeForSizes (BS.concat ["[1", BS.singleton b, "2", BS.singleton b, "true", BS.singleton b, "]"])
  describe "captures" $ corpusSpec =<< runIO (lookupEnv "JSON_STREAM_DIFF_CORPUS")
