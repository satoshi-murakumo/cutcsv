{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

import qualified Codec.Archive.Tar            as Tar
import qualified Codec.Compression.GZip       as GZip
import           Control.Applicative          (many, (*>), (<$>), (<*), (<*>),
                                               (<|>))
import           Control.Monad                (forM_, void)
import           Data.Attoparsec.Char8        (Parser, char, sepBy1, string,
                                               takeWhile, (<?>))
import           Data.Attoparsec.Lazy         (Result (..), parse)
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy.Char8   as BSL
import           Data.Monoid
import           Prelude                      hiding (notElem, takeWhile)
import           System.Console.CmdArgs
import           System.FilePath              (takeExtensions, takeFileName)
import           System.IO                    (BufferMode (..), hSetBinaryMode,
                                               hSetBuffering, stdin, stdout)

------------------------------------------------------------------------------
-- main

main :: IO ()
main = do
    hSetBinaryMode stdin  True
    hSetBinaryMode stdout True

    option <- cmdArgs config

    process (fields' option) (files option)

------------------------------------------------------------------------------
-- commandline option

data Option = Option {
              fields :: String
            , files  :: [FilePath]
            } deriving (Show, Data, Typeable)

config :: Option
config = Option {
         fields = def         &= typ "[INT]" &= help "ex; 1,2,9"
       , files  = def &= args &= typFile
       } &= program "cutcsv"
         &= summary "Pull out a specific column from a CSV file."

fields' :: Option -> [Int]
fields' Option {..} = read $ "[" ++ fields ++ "]" :: [Int]

------------------------------------------------------------------------------
-- process input

process cols [] = do
    contents <- BSL.getContents
    BB.hPutBuilder stdout
      $ renderCsv $ cutCsv cols $ parseCsv contents

process cols fs = do
    fcontents <- concat <$> mapM readContents fs
    forM_ fcontents $ \(fname, contents) -> do
        putStrLn fname
        BB.hPutBuilder stdout
          $ renderCsv $ cutCsv cols $ parseCsv contents

readContents f
    | takeExtensions f == ".tar.gz" = readTarGz f
    | takeExtensions f == ".gz"     = readGz f
    | otherwise                     = readFile' f

readTarGz, readGz, readFile' :: FilePath -> IO [(FilePath, BSL.ByteString)]
readTarGz f = do
    entries <- Tar.read . GZip.decompress <$> BSL.readFile f
    return . concatMap toContents . toList $ entries
  where
    toList = Tar.foldEntries (:) [] (error . show)
    toContents e = case Tar.entryContent e of
      Tar.NormalFile contents _ ->
          [(takeFileName . Tar.entryPath $ e , contents)]
      _ -> []

readGz f = do
    contents <- GZip.decompress <$> BSL.readFile f
    return $ [(takeFileName f, contents)]

readFile' f = do
    contents <- BSL.readFile f
    return $ [(takeFileName f, contents)]

------------------------------------------------------------------------------
-- parse Csv

type CsvRecord = [CsvField]

data CsvField = QuotedField ByteString
              | UnquotedField ByteString
                deriving Show

parseCsv :: BSL.ByteString -> [CsvRecord]
parseCsv b = loop $ parse csvRecord b
  where
    loop (Fail _ messages additional) =
      error $ concatMap (++ "\n") $ additional : messages
    loop (Done rest r)
      | rest == BSL.empty = [r]
      | otherwise        = r : (loop $ parse csvRecord rest)

csvRecord :: Parser CsvRecord
csvRecord = (csvField `sepBy1` char ',') <* lineEnd
            <?> "record"

csvField :: Parser CsvField
csvField = quotedField <|> unquotedField
           <?> "field"

quotedField :: Parser CsvField
quotedField = char '"' *> (QuotedField <$> insideQuotes) <* char '"'
              <?> "quotedField"

insideQuotes :: Parser BS.ByteString
insideQuotes =
    mappend <$> takeWhile (/= '"')
            <*> (mconcat <$> many (mappend <$> dquotes <*> insideQuotes))
               <?> "inside of double quotes"
  where
    dquotes = string "\"\"" >> return "\""
              <?> "paired double quotes"

unquotedField :: Parser CsvField
unquotedField = UnquotedField <$> takeWhile (`BS.notElem` ",\n\r\"")
                <?> "unquoted field"

lineEnd :: Parser ()
lineEnd = void (char '\n') <|> void (string "\r\n") <|> void (char '\r')
          <?> "end of line"

cr = 13
lf = 10
dquote = 34
comma = 44

------------------------------------------------------------------------------
-- select column

cutCsv :: [Int] -> [CsvRecord] -> [CsvRecord]
cutCsv ns = map (selectCols ns)

selectCols :: [Int] -> [a] -> [a]
selectCols [] cols = cols
selectCols ns cols = concatMap (selectCol cols) ns

selectCol :: [a] -> Int -> [a]
selectCol cols n = take 1 $ drop (n - 1) cols

------------------------------------------------------------------------------
-- render Csv

renderCsv :: [CsvRecord] -> Builder
renderCsv = mconcat . map renderRecord

renderRecord :: CsvRecord -> Builder
renderRecord []     = mempty
renderRecord (c:cs) =
    renderField c <> mconcat [ BB.char8 ',' <> renderField i | i <- cs ]
                  <> BB.char8 '\n'

renderField :: CsvField -> Builder
renderField (UnquotedField s) = BB.byteString s
renderField (QuotedField s)   = BB.char8 '"' <> BS.foldr escape mempty s <> BB.char8 '"'
  where
    escape '\r' ac = BB.char8 '\\' <> BB.char8 'r' <> ac
    escape '\n' ac = BB.char8 '\\' <> BB.char8 'n' <> ac
    escape '"'  ac = BB.char8 '"'  <> BB.char8 '"' <> ac
    escape c    ac = BB.char8 c    <> ac
