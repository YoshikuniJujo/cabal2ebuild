import Gentoo.Cabal2Ebuild
import Network.Curl
import System.Environment
import System.Console.GetOpt
import System.Exit

main :: IO ()
main = do
	args <- getArgs
	let	(os, as, errs) = getOpt Permute descrs args
	(pn, pv) <- case (as, errs) of
			([n, v], []) -> return (n, readV v)
			_ -> do	putStrLn "Usage: hackage2ebuild [package name] [package version]"
				mapM_ putStr errs
				exitFailure
	(rsp, cnt) <- withCurlDo $ curlGetString (mkURL pn pv) []
	case cabal2ebuild cnt of
		Just (fn, eb) -> writeFile (getDir os ++ "/" ++ fn) eb
		_ -> putStrLn "cabal2ebuild: error"
	putStrLn $ mkURL pn pv
	print rsp
	print os

mkURL :: String -> Version -> URLString
mkURL pn pv =
	"http://hackage.haskell.org/package/" ++ pn ++ "-" ++ showV pv ++ "/" ++
	pn ++ ".cabal"

type Version = [Int]

readV :: String -> [Int]
readV str = case span (/= '.') str of
	(n, "") -> [read n]
	(n, '.' : ns) -> read n : readV ns
	_ -> error "never occur"

showV :: [Int] -> String
showV [n] = show n
showV (n : ns) = show n ++ "." ++ showV ns
showV _ = error "bad version"

descrs :: [OptDescr Option]
descrs = [
	Option "d" [] (ReqArg Directory "directory") "directory"
 ] 

data Option = Directory FilePath deriving Show

getDir :: [Option] -> FilePath
getDir [] = "."
getDir (Directory fp : _) = fp
-- getDir (_ : os) = getDir os
