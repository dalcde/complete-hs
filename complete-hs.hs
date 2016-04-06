import Control.Monad.List
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) = flip (.)
infixl 2 >>>

if' :: a -> a -> Bool -> a
if' x y bool  = if bool then x else y

description :: String
description = "\
    \Usage: complete-hs [prefix] [command]\n\
    \Completes the command [command]. If [prefix] is empty, it searches in $PATH as\n\
    \well"

parse :: [String] -> IO (String, String)
parse ("-h":_) = putStrLn description >> exitWith ExitSuccess
parse (p:c:xs) = return (p, c)
parse _        = putStrLn description >> exitWith (ExitFailure 1)

prettyPath :: FilePath -> IO FilePath
prettyPath path = fmap (if' (takeFileName path ++ "/") (takeFileName path)) (doesDirectoryExist path)

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' dir = fmap (map (dir </>)) (getDirectoryContents dir) >>= sequence . map prettyPath

listDirectory' :: [FilePath] -> IO [FilePath]
listDirectory' = filterM doesDirectoryExist >=>
    fmap join . sequence . map getDirectoryContents' >>>
    (fmap $ filter $ not . (`elem` [".", ".."]))

getDirs :: String -> FilePath -> IO [FilePath]
getDirs "" "./"        = pure (:) <*> getCurrentDirectory <*> getSearchPath
getDirs _  "./"        = fmap return getCurrentDirectory
getDirs _  dir@('/':_) = return [dir]
getDirs _  dir         = fmap return $ fmap (</> dir) getCurrentDirectory

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix (a:as) (b:bs)
    | a == b     = a:commonPrefix as bs
    | otherwise  = []
commonPrefix _ _ = []

longestCommonPrefix :: Eq a => [[a]] -> [a]
longestCommonPrefix [] = []
longestCommonPrefix lists = foldr1 commonPrefix lists

getCompletion :: (Ord a, Eq a) => [a] -> [[a]] -> [[a]]
getCompletion prefix list = (longestCommonPrefix results):results
    where results = sort . map (drop (length prefix)) . filter (isPrefixOf prefix) $ list

main = do
    (p, c) <- getArgs >>= parse
    let (dir, name) = splitFileName c
    lists <- getDirs p dir >>= listDirectory'
    mapM_ putStrLn $ map ((p ++ c) ++) $ getCompletion name lists
