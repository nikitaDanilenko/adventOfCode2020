module Day08 where

import Day02                   ( int )
import Text.Parsec.String      ( Parser )
import Text.Parsec.Char        ( spaces, string )
import Text.Parsec.Combinator  ( sepBy )
import Text.Parsec             ( choice, parse )
import Data.Maybe              ( catMaybes )
import Data.Map                ( Map, fromList, (!) )
import qualified Data.Set as S ( empty, insert, member ) 


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

processUntilLoop :: [Cmd] -> Integer
processUntilLoop cs = go 0 S.empty (head cs) where
    cmdMap = mkCmdMap cs

    go acc visited cmd 
        | position cmd `S.member` visited = acc
        | otherwise = go nextAcc (S.insert (position cmd) visited) next where
            (nextAcc, nextPos) = 
                case op cmd of
                    Nop -> (acc, 1 + position cmd)
                    Acc -> (acc + change cmd, 1 + position cmd)
                    Jmp -> (acc, change cmd + position cmd)
            next = cmdMap ! nextPos

solution1 :: IO Integer
solution1 = fmap processUntilLoop readCmds
