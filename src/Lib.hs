-- nmp -- Not My Process
-- takes a PBS job ID and displays any nodes containing
-- processes left over from a previous user
{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}
module Lib
    ( libMain
    ) where

-- imports
import System.Console.CmdArgs
import System.Exit (exitWith, ExitCode(..))
import System.Process (readCreateProcess, readCreateProcessWithExitCode, shell)
import Text.Regex.PCRE.Heavy (split, scan, re, compileM,(=~))
import Data.Char (isDigit)
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromJust)
import Data.Either.Compat (fromRight)
import System.Posix.User (getLoginName)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

type JobID = String
data JobInfo = JobInfo { jid      :: JobID
                       , job_name :: String
                       , user     :: String
                       , run_time :: String
                       , state    :: String
                       , queue    :: String
                       } deriving (Eq, Show)

data NMP = NMP { jobid :: JobID } deriving (Data, Typeable, Show, Eq)

nmp = NMP { jobid = def &= args &= typ "Job ID"} &= 
      verbosity &=
      help "Display nodes in a PBS job with processes left over from a previous user" &= 
      summary "nmp v0.0.1" &=
      details ["The nmp command queries a given PBS job and returns"
              ,"a list of nodes with processes left over from a "
              ,"previous job."]

mode = cmdArgsMode nmp

-- passed to main
libMain :: IO ()
libMain = do
    args <- cmdArgs nmp
    let jid = jobid args
    if (invalidJobID jid)
        then Prelude.putStrLn ("Error: " ++ jid ++ " is not a valid job ID") >> exitWith (ExitFailure 137)
        else putStrLn $ "Checking job " ++ jid 
    
    -- Exit if the job is not running, otherwise get the nodes where the job is running
    job_info <- jobInfo jid
    if (state job_info /= "R")
        then putStrLn ("Job " ++ jid ++ " is not currently running") >> exitWith (ExitFailure 138)
        else putStrLn "Job is running, getting nodelist"
    
    -- get list of processes on all hosts
    np <- nonUserProcesses job_info
    if (np == [])
        then putStrLn $ "Job " ++ jid ++ " appears clean, no foreign user processes found"
        else putStrLn "Foreign user processes detected: " >> mapM_ putStrLn np

-- check to see if the Job ID has a valid format
invalidJobID :: JobID -> Bool
invalidJobID j = not $ (allDigits j) && (length j == 7)
                 where allDigits x = foldr (&&) True $ map isDigit x

-- get the state of job a job from a job id
--jobInfo :: JobID -> IO JobInfo
jobInfo :: JobID -> IO JobInfo
jobInfo j = do
    let job_str = "qstat " ++ j
    pbs_rval <- readCreateProcess (shell job_str) []
    let r = (words . head) $ filter (isPrefixOf j) (lines pbs_rval)
    return $ fromJust (mkJobInfo r)

-- get list of nodes associated with a running job
jobNodes :: JobInfo -> IO [String]
jobNodes j = do
    let job_str = "qstat -n " ++ (jid j)
    pbs_rval <- readCreateProcess (shell job_str) []
    let node_str = drop 6 (lines pbs_rval)
    let nodes = scan [re|r[0-9]+i[0-9]+n[0-9]+|] (concat node_str)
    return $ map fst nodes

-- list all processes on nodes using "ps"
nodeProcesses :: [String] -> IO [String]
nodeProcesses ns = do
    let node_str = intercalate "," ns
    let pdsh_cmd_str = "pdsh -w " ++ node_str ++ " ps aux"
    pdsh_rval <- readCreateProcess (shell pdsh_cmd_str) []
    return $ lines pdsh_rval

-- list processes on nodes, filtering out those owned by the user executing this
-- process, the job owner, and known system users
nonUserProcesses :: JobInfo -> IO [String]
nonUserProcesses ji = do
    all_procs <- jobNodes ji >>= nodeProcesses 
    log_name <- getLoginName
    let regex_str = BC.pack $ "(" ++ (user ji) ++ "|" ++ log_name ++ "|root|message+|daemon|nscd|USER|nagios|ntp|nobody|postfix)"
--    putStrLn $ show regex_str
    let regex = fromRight [re||] $ compileM regex_str []
    return $ filter (\x -> x /=~ regex) all_procs

-- make a JobInfo from an array of strings
mkJobInfo :: [String] -> Maybe JobInfo
mkJobInfo (i:n:u:t:s:q:[]) = Just (JobInfo i n u t s q)
mkJobInfo _ = Nothing

-- convenience function: non-matching regex operator
(/=~) x y = not (x =~ y)