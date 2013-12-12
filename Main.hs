module Main where

import Text.ParserCombinators.Parsec as P
import System.FilePath.Find (find, always, fileName, (~~?))
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Either (rights, lefts)
import Text.Printf (printf)


data Subsystem = Subsystem {
    subsystemName :: String
   ,members :: [Member]
}


data Member = Member {
    memberName :: String
   ,netid :: String
   ,year :: Year
   ,credits :: Int
   ,leadEvals :: [Float]
   ,memberEvals :: [Float]
}


data Year = F | So | J | Sr | MEng deriving (Show, Read)


instance Show Member where
    show m = memberData ++ "\n" ++ rawData
        where
            memberData = intercalate "," . map ($ m) $ [
                    memberName
                   ,netid
                   ,show . year
                   ,show . credits
                   ,printf "%.2f" . average . leadEvals
                   ,printf "%.2f" . average . memberEvals
                ]
            leadEvalsStr = intercalate "," . map show . leadEvals $ m
            memberEvalsStr = intercalate "," . map show . memberEvals $ m
            rawData = intercalate "\n" [
                    ",,,,,,Lead evaluations:," ++ leadEvalsStr
                   ,",,,,,,Member evaluations:," ++ memberEvalsStr
                ]


instance Show Subsystem where
    show s =
        subsystemName s ++ "\n\n" ++ intercalate "\n\n" (map show $ members s) ++ "\n\n"


average :: Fractional a => [a] -> a
average l = sum l / fromIntegral (length l)


inSpaces :: P.Parser a -> P.Parser a
inSpaces = P.between P.spaces P.spaces


inBraces :: P.Parser a -> P.Parser a
inBraces = P.between (P.char '{') (P.char '}')


inBrackets :: P.Parser a -> P.Parser a
inBrackets = P.between (P.char '[') (P.char ']')


pValLine :: P.Parser a -> P.Parser a
pValLine p = do
    P.manyTill P.anyChar (P.char ':')
    P.spaces
    val <- p
    P.newline
    return val


pSubsystemName :: P.Parser String
pSubsystemName = pValLine $ P.many1 P.letter


pYear :: P.Parser Year
pYear = read <$> pValLine (P.string "F"
                       <|> P.try (P.string "So")
                       <|> P.string "J"
                       <|> P.string "Sr"
                       <|> P.string "MEng")


pScoreLine :: P.Parser Float
pScoreLine = read <$> inSpaces (P.many1 (P.digit <|> P.char '.'))


pScores :: P.Parser [Float]
pScores = do
    P.manyTill P.anyChar (P.char ':')
    inSpaces $ inBrackets $ P.many pScoreLine


pMember :: P.Parser Member
pMember = inBraces $ do
    _memberName <- pValLine $ P.many1 (P.letter <|> P.oneOf " -") --[' ', '-'])
    _netid <- pValLine $ P.many1 P.alphaNum
    _year <- pYear
    _credits <- read <$> pValLine (many1 P.digit) :: P.Parser Int
    _leadEvals <- pScores
    _memberEvals <- pScores
    return $ Member _memberName _netid _year _credits _leadEvals _memberEvals


pMembers :: P.Parser [Member]
pMembers = P.sepEndBy1 pMember P.spaces


pSubsystem :: P.Parser Subsystem
pSubsystem = do
    _subsystemName <- pSubsystemName
    _members <- pMembers
    return $ Subsystem _subsystemName _members


parseAll :: [String] -> [Subsystem]
parseAll evals =
    let results = map (parse pSubsystem "") evals in
    case lefts results of
        [] -> rights results
        xs -> error . show $ lefts results


writeOutput :: [String] -> IO ()
writeOutput = writeFile "example.csv" . intercalate "\n" . map show . parseAll


main :: IO ()
main = do
    evalFiles <- find always (fileName ~~? "*.txt") "."
    evals <- mapM readFile evalFiles
    writeOutput $ map (filter (/='\r')) evals