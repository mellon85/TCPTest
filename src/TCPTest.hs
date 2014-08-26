module Main where

import TCPTest.FFIInterface

import Network.Socket
import Network.Socket.ByteString as B

import Control.Monad
import Control.Concurrent

import Data.Word
import Control.Exception

import System.Console.GetOpt
import System.Environment
import System.Random

import qualified Data.ByteString as B

-- Application options
data Options = Options
    { _verbose           :: Bool
    , _targetAddress     :: String
    , _targetPort        :: Word16
    , _reportInterval    :: Int
    , _sendInterval      :: Int
    , _packetSize        :: Word32
    } deriving(Show)

options =
    [ Option ['v'] ["verbose"]  (NoArg  (\o   -> o { _verbose        = True }))           "Verbose output enabled"
    , Option ['a'] ["address"]  (ReqArg (\v o -> o { _targetAddress  = v }) "localhost")  "Target address"
    , Option ['p'] ["port"]     (ReqArg (\v o -> o { _targetPort     = read v }) "80")    "Target port"
    , Option ['i'] ["interval"] (ReqArg (\v o -> o { _reportInterval = ms2us $ read v }) "1000")  "Reporting Interval in ms"
    , Option ['t'] ["send-interval"] (ReqArg (\v o -> o { _sendInterval = ms2us $ read v }) "10") "Send Interval in ms"
    , Option ['s'] ["size"] (ReqArg (\v o -> o { _packetSize = read v }) "128")           "Data packet size"
    ]

{- milliseconds to nanoseconds -}
ms2us = (*1000)

defaultOptions = Options False "localhost" 80 (ms2us $ 1000) (ms2us $ 100) 128

parseOptions = do
    args <- getArgs
    let (a,o,e) = getOpt Permute options args in do
        case (o,e) of
            ([],[]) -> return $! foldl (flip id) defaultOptions a
            _       -> fail   $ concat e

logIOException :: IOException -> IO ()
logIOException x = print x

eitherLog m g e = case e of 
    Left  l -> putStrLn m >> logIOException l
    Right r -> g r >> return ()

testAddress opts addr = do
    putStrLn $ "Trying " ++ (show $ addrAddress addr)
    sockAndThread <- try $ initialize 
    eitherLog "Couldn't create socket" (\x@(s,_) -> test s `finally` clean x) sockAndThread

    where
        test s = do
            r   <- try $ connect s (addrAddress addr) :: IO (Either IOException ())
            gen <- getStdGen
            eitherLog "Connection failed" (sendLoop s (randoms gen)) r

        logIn x = do
            logIOException x
            return $ Left x

        clean (s,t) = killThread t >> close s

        initialize = do
            sock   <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            thread <- forkIO . infoDumper sock . _reportInterval $ opts
            return (sock,thread)

        sendLoop sock ints _ = handle logIOException $ forever $ do
            packAndSend . fromIntegral . _packetSize $ opts
            threadDelay . _sendInterval $ opts
            where
                packAndSend size = B.send sock . B.pack . take size $ ints

main = parseOptions >>= appMain

appMain opts = do
    addrs <- try $ getAddrInfo
            (Just (defaultHints { addrSocketType = Stream, addrFlags = [AI_ADDRCONFIG, AI_CANONNAME] }))
            (Just $ _targetAddress opts)
            (Just . show $ _targetPort opts)
    eitherLog "Unable to resolve the address/port" (mapM_ (testAddress opts)) addrs

infoDumper :: Socket -> Int -> IO ()
infoDumper sock interval = do
    printTCPInfoHeader
    forever $ do
        info <- socketInfo (fdSocket sock)
        either print print info
        threadDelay interval
