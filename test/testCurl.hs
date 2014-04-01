import Network.Curl

main :: IO ()
main = do
	(rsp, cnt) <- withCurlDo $ curlGetString url []
	putStr cnt

url :: URLString
url = "http://hackage.haskell.org/package/yesod-platform-1.2.7/yesod-platform.cabal"
