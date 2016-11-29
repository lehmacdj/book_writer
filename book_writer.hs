import System.Random
import System.IO
import Control.Monad.State
import Data.Char
import Data.List

main = do
    contents <- readFile "/usr/share/dict/words"
    let dict = lines contents
    gen <- getStdGen
    let sentences = fst $ runState (makeText dict) $ gen
        strSentences = map stringOfSentence sentences
        text = concat $ intersperse " " strSentences
        novel = unwords $ take 50000 $ words text
        wrapped = unlines $ concatMap (map trim . wrapLine 80) $ lines novel
    writeFile "novel.txt" $ novel
    return ()


trim :: String -> String
trim = trimAndReverse . trimAndReverse
  where trimAndReverse = reverse . dropWhile isSpace

reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
reverseBreak f xs = (reverse before, reverse after)
  where (after, before) = break f $ reverse xs

wrapLine :: Int -> String -> [String]
wrapLine maxLen line
  | length line <= maxLen  = [line]
  | any isSpace beforeMax  = beforeSpace : (wrapLine maxLen $ afterSpace ++ afterMax)
  | otherwise              = beforeMax : wrapLine maxLen afterMax
    where (beforeMax, afterMax) = splitAt maxLen line
          (beforeSpace, afterSpace) = reverseBreak isSpace beforeMax

stringOfSentence :: Sentence -> String
stringOfSentence (iphrasePairs, mphrase, endMark) =
    let strOfPhrasePair (p, s) = stringOfPhrase p ++ stringOfSeparator s
        phrases = map strOfPhrasePair iphrasePairs ++ [stringOfPhrase mphrase]
        spaced = concat $ intersperse " " phrases
    in  upperCaseFirst spaced ++ stringOfEndMark endMark

upperCaseFirst :: String -> String
upperCaseFirst string = toUpper (head string) : tail string

stringOfPhrase :: Phrase -> String
stringOfPhrase = concat . intersperse " "

stringOfSeparator :: Separator -> String
stringOfSeparator Comma = ","
stringOfSeparator Semicolon = ";"
stringOfSeparator Colon = ":"

stringOfEndMark :: EndMark -> String
stringOfEndMark Period = "."
stringOfEndMark Question = "?"
stringOfEndMark Exclaimation = "!"

type BookWord = String

data Separator = Comma | Semicolon | Colon
    deriving (Bounded, Enum, Show)
data EndMark = Exclaimation | Question | Period
    deriving (Bounded, Enum, Show)

type Phrase = [BookWord] -- A phrase should be at least 3 words
type Sentence = ([(Phrase, Separator)], Phrase, EndMark)

randomStR :: (RandomGen g, Random a) => (a, a) -> State g a
randomStR = state . randomR

repeatM :: (Monad m) => m a -> m [a]
repeatM = sequence . repeat

makeText :: (RandomGen g) => [String] -> State g [Sentence]
makeText dict = repeatM $ makeSentence dict

makeSentence :: (RandomGen g) => [String] -> State g Sentence
makeSentence dict = do
    extraPhraseCount <- randomStR (0, 2)
    phrases <- replicateM extraPhraseCount $ makePhrase dict
    separators <- replicateM extraPhraseCount $ makeSeparator
    let initialPhrases = zip phrases separators
    endMark <- makeEndMark
    mainPhrase <- makePhrase dict
    return (initialPhrases, mainPhrase, endMark)

makePhrase :: (RandomGen g) => [String] -> State g Phrase
makePhrase dict = do
    wordCount <- randomStR (3, 10)
    replicateM wordCount $ makeWord dict

makeWord :: (RandomGen g) => [String] -> State g BookWord
makeWord dict = do
    index <- randomStR (0, length dict - 1)
    return $ dict !! index

makeSeparator :: (RandomGen g) => State g Separator
makeSeparator = do
    num <- randomStR (0, 4)
    return $ separatorOfNum num

separatorOfNum :: Int -> Separator
separatorOfNum 0 = Comma
separatorOfNum 1 = Comma
separatorOfNum 2 = Comma
separatorOfNum 3 = Semicolon
separatorOfNum 4 = Colon
separatorOfNum _ = error "invalid num to convert into separator"

makeEndMark :: (RandomGen g) => State g EndMark
makeEndMark = do
    num <- randomStR (0, 4)
    return $ endMarkOfNum num

endMarkOfNum :: Int -> EndMark
endMarkOfNum 0 = Period
endMarkOfNum 1 = Period
endMarkOfNum 2 = Period
endMarkOfNum 3 = Question
endMarkOfNum 4 = Exclaimation
endMarkOfNum _ = error "invalid num to convert into end mark"

-- Functions to help in GHC
evalRandom r = newStdGen >>= return . \g -> fst $ runState r g
