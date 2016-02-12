{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

import qualified Codec.Archive.Tar                as Tar
import qualified Codec.Archive.Zip                as Zip
import qualified Codec.Compression.GZip           as GZip
import           Control.Applicative              (many, (<|>))
import           Control.Monad                    (forM_, void, when)
import           Data.Attoparsec.ByteString.Char8 (Parser, char, sepBy1, string,
                                                   takeWhile, (<?>))
import           Data.Attoparsec.ByteString.Lazy  (Result (..), parse)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as BS
import           Data.ByteString.Lazy.Builder     (Builder)
import qualified Data.ByteString.Lazy.Builder     as BB
import qualified Data.ByteString.Lazy.Char8       as BSL
import           Data.Monoid
import           Prelude                          hiding (notElem, takeWhile)
import           System.Console.CmdArgs
import           System.FilePath                  (takeExtensions, takeFileName)
import           System.IO                        (hSetBinaryMode, stdin,
                                                   stdout)

------------------------------------------------------------------------------
-- main

main :: IO ()
main = do
    hSetBinaryMode stdin  True
    hSetBinaryMode stdout True

    option <- cmdArgs config

    process option (files option)

------------------------------------------------------------------------------
-- commandline option

data Option = Option {
              nofilename :: Bool
            , noescape   :: Bool
            , fields     :: String
            , files      :: [FilePath]
            } deriving (Show, Data, Typeable)

config :: Option
config = Option {
         nofilename   = False
       , noescape = False
       , fields   = def         &= typ "[INT]" &= help "ex; 1,2,9"
       , files    = def &= args &= typFile
       } &= program "cutcsv"
         &= summary "Pull out a specific column from a CSV file."

fields' :: String -> [Int]
fields' fields = read $ "[" ++ fields ++ "]" :: [Int]

------------------------------------------------------------------------------
-- process input

process :: Option -> [FilePath] -> IO ()
process Option{..} [] = do
    contents <- BSL.getContents
    BB.hPutBuilder stdout
      $ processCore noescape (fields' fields) contents

process Option{..} fs =
    forM_ fs $ \filepath -> do
        fcontents <- readContents filepath

        forM_ fcontents $ \(fname, contents) -> do
            when (not nofilename) $ putStrLn fname
            BB.hPutBuilder stdout
              $ processCore noescape (fields' fields) contents

processCore :: Bool -> [Int] -> BSL.ByteString -> Builder
processCore noescape cols = renderCsv noescape . cutCsv cols . parseCsv

readContents :: FilePath -> IO [(FilePath, BSL.ByteString)]
readContents f
    | takeExtensions f == ".tar.gz" = readTarGz f
    | takeExtensions f == ".gz"     = readGz f
    | takeExtensions f == ".zip"    = readZip f
    | otherwise                     = readFile' f

readTarGz, readGz, readZip, readFile' :: FilePath -> IO [(FilePath, BSL.ByteString)]
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
    return [(takeFileName f, contents)]

readZip f = do
    archive <- Zip.toArchive <$> BSL.readFile f
    return $ map toContents $ Zip.zEntries archive
  where
    toContents e = (takeFileName . Zip.eRelativePath $ e, Zip.fromEntry e)

readFile' f = do
    contents <- BSL.readFile f
    return [(takeFileName f, contents)]

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
      error $ unlines $ additional : messages
    loop (Done rest r)
      | rest == BSL.empty = [r]
      | otherwise        = r : loop (parse csvRecord rest)

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

renderCsv :: Bool -> [CsvRecord] -> Builder
renderCsv noescape = mconcat . map (renderRecord noescape)

renderRecord :: Bool -> CsvRecord -> Builder
renderRecord _ []     = mempty
renderRecord e (c:cs) =
    renderField e c <> mconcat [ BB.char8 ',' <> renderField e i | i <- cs ]
                    <> BB.char8 '\n'

renderField :: Bool -> CsvField -> Builder
renderField _ (UnquotedField s) = BB.byteString s
renderField e (QuotedField s)   = BB.char8 '"'
                                    <> (if e then noescape else escape) s
                                    <> BB.char8 '"'
  where
    noescape = BS.foldr noescape' mempty
    noescape' '"'  ac = BB.char8 '"'  <> BB.char8 '"' <> ac
    noescape' c    ac = BB.char8 c    <> ac

    escape = BS.foldr escape' mempty
    escape' '\r' ac = BB.char8 '\\' <> BB.char8 'r' <> ac
    escape' '\n' ac = BB.char8 '\\' <> BB.char8 'n' <> ac
    escape' '"'  ac = BB.char8 '"'  <> BB.char8 '"' <> ac
    escape' c    ac = BB.char8 c    <> ac
