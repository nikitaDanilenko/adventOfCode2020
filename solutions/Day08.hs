module Day08 where

import Day02                   ( int )
import Text.Parsec.String      ( Parser )
import Text.Parsec.Char        ( spaces, string )
import Text.Parsec.Combinator  ( sepBy )
import Text.Parsec             ( choice, parse )
import Data.Maybe              ( catMaybes )
import Data.Map                ( Map, fromList, (!), findMax, notMember )
import qualified Data.Set as S ( empty, insert, member ) 
import Data.List               ( findIndices, find )


data Op = Nop | Acc | Jmp
    deriving Show

data Cmd = Cmd { position :: Integer, op :: Op, change :: Integer }
    deriving Show

opParser :: Parser Op
opParser =
    choice [fmap (const Nop) (string "nop"), fmap (const Acc) (string "acc"), fmap (const Jmp) (string "jmp")]

changeParser :: Parser Integer
changeParser = do
    f <- choice [fmap (const id) (string "+"), fmap (const negate) (string "-")]
    n <- int
    pure (f n)

cmdParser :: Parser Cmd
cmdParser = do
    p <- int
    spaces
    o <- opParser
    spaces
    c <- changeParser
    pure (Cmd p o c)

parseCmd :: String -> Maybe Cmd
parseCmd t = case parse cmdParser "" t of
               Right c -> Just c
               Left _ -> Nothing

parseCmds :: [String] -> [Cmd]
parseCmds = catMaybes . map parseCmd

readCmds :: IO [Cmd]
readCmds = do
    text <- readFile "../inputs/day08.txt"
    let cmdsLs = zipWith (\i l -> unwords [show i, l]) [1 .. ] (lines text)
        cmds  = parseCmds cmdsLs
    pure cmds

type CmdMap = Map Integer Cmd

mkCmdMap :: [Cmd] -> CmdMap
mkCmdMap = fromList . map (\cmd -> (position cmd, cmd))

processUntilLoop :: [Cmd] -> Result
processUntilLoop cs = go 0 S.empty (head cs) where
    cmdMap = mkCmdMap cs
    highest = fst (findMax cmdMap)

    go acc visited cmd 
        | position cmd `S.member` visited = Result acc True
        | notMember nextPos cmdMap        = Result nextAcc False
        | otherwise                       = go nextAcc (S.insert (position cmd) visited) next 
        where
            (nextAcc, nextPos) = 
                case op cmd of
                    Nop -> (acc, 1 + position cmd)
                    Acc -> (acc + change cmd, 1 + position cmd)
                    Jmp -> (acc, change cmd + position cmd)
            next = cmdMap ! nextPos

data Result = Result { value :: Integer, looped :: Bool }
    deriving Show

solution1 :: IO Integer
solution1 = fmap (value . processUntilLoop) readCmds

isNopOrJmp :: Op -> Bool
isNopOrJmp Acc = False
isNopOrJmp _ = True

switchJmpNop :: Op -> Op
switchJmpNop Jmp = Nop
switchJmpNop Nop = Jmp
switchJmpNop o = o

switchAt :: Integer -> [Cmd] -> [Cmd]
switchAt pos = map (\c -> if position c == pos then c { op = switchJmpNop (op c) } else c)

attempt :: [Cmd] -> Maybe Result
attempt cs = guess where

    jmpNopIndices = map (+1) (findIndices (isNopOrJmp . op) cs)

    guess = find (not . looped) (map processUntilLoop (cs : map (flip switchAt cs . fromIntegral) jmpNopIndices))

solution2 :: IO (Maybe Integer)
solution2 = fmap (fmap value . attempt) readCmds 