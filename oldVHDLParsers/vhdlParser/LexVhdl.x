-- -*- haskell -*-
-- This Alex file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}
module LexVhdl where



import qualified Data.Bits
import Data.Word (Word8)
import Data.Char (ord)
}


$l = [a-zA-Z\192 - \255] # [\215 \247]    -- isolatin1 letter FIXME
$c = [A-Z\192-\221] # [\215]    -- capital isolatin1 letter FIXME
$s = [a-z\222-\255] # [\247]    -- small isolatin1 letter FIXME
$d = [0-9]                -- digit
$i = [$l $d _ ']          -- identifier character
$u = [\0-\255]          -- universal: any character

@rsyms =    -- symbols and non-identifier-like reserved words
   \( | \) | \+ | \- | \& | \, | \; | \: | \. | \: \= | \' | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | \# | \" | \= \> | \< \= | \< \> | "I" \  "DONT" \  "KOW" \  "WHAT" \  "GOES" \  "HERE" | \_ | \* \* | \* | \/ | \= | \/ \= | \< | \> | \> \= | \$ | \ 

:-
"--" [.]* ; -- Toss single line comments

$white+ ;
@rsyms { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }
($d | \_)* { tok (\p s -> PT p (eitherResIdent (T_NumString . share) s)) }

$l $i*   { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }





{

tok :: (Posn -> String -> Token) -> (Posn -> String -> Token)
tok f p s = f p s

share :: String -> String
share = id

data Tok =
   TS !String !Int    -- reserved words and symbols
 | TL !String         -- string literals
 | TI !String         -- integer literals
 | TV !String         -- identifiers
 | TD !String         -- double precision float literals
 | TC !String         -- character literals
 | T_NumString !String

 deriving (Eq,Show,Ord)

data Token =
   PT  Posn Tok
 | Err Posn
  deriving (Eq,Show,Ord)

tokenPos :: [Token] -> String
tokenPos (PT (Pn _ l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn _ l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p
tokenPosn (Err p) = p

tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

mkPosToken :: Token -> ((Int, Int), String)
mkPosToken t@(PT p _) = (posLineCol p, prToken t)

prToken :: Token -> String
prToken t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> show s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s
  PT _ (T_NumString s) -> s


data BTree = N | B String Tok BTree BTree deriving (Show)

eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

resWords :: BTree
resWords = b "MAP" 96 (b "B" 48 (b "7" 24 (b "," 12 (b "'" 6 (b "#" 3 (b "\"" 2 (b " " 1 N N) N) (b "&" 5 (b "$" 4 N N) N)) (b "*" 9 (b ")" 8 (b "(" 7 N N) N) (b "+" 11 (b "**" 10 N N) N))) (b "1" 18 (b "/" 15 (b "." 14 (b "-" 13 N N) N) (b "0" 17 (b "/=" 16 N N) N)) (b "4" 21 (b "3" 20 (b "2" 19 N N) N) (b "6" 23 (b "5" 22 N N) N)))) (b ">=" 36 (b "<" 30 (b ":" 27 (b "9" 26 (b "8" 25 N N) N) (b ";" 29 (b ":=" 28 N N) N)) (b "=" 33 (b "<>" 32 (b "<=" 31 N N) N) (b ">" 35 (b "=>" 34 N N) N))) (b "ALL" 42 (b "ACCESS" 39 (b "ABS" 38 (b "A" 37 N N) N) (b "ALIAS" 41 (b "AFTER" 40 N N) N)) (b "ARRAY" 45 (b "ARCHITECTURE" 44 (b "AND" 43 N N) N) (b "ATTRIBUTE" 47 (b "ASSERT" 46 N N) N))))) (b "Format_effector" 72 (b "DISCONNECT" 60 (b "C" 54 (b "BODY" 51 (b "BLOCK" 50 (b "BEGIN" 49 N N) N) (b "BUS" 53 (b "BUFFER" 52 N N) N)) (b "CONFIGURATION" 57 (b "COMPONENT" 56 (b "CASE" 55 N N) N) (b "D" 59 (b "CONSTANT" 58 N N) N))) (b "ENTITY" 66 (b "ELSE" 63 (b "E" 62 (b "DOWNTO" 61 N N) N) (b "END" 65 (b "ELSIF" 64 N N) N)) (b "FILE" 69 (b "F" 68 (b "EXIT" 67 N N) N) (b "FUNCTION" 71 (b "FOR" 70 N N) N)))) (b "INERTIAL" 84 (b "H" 78 (b "GENERIC" 75 (b "GENERATE" 74 (b "G" 73 N N) N) (b "GUARDED" 77 (b "GROUP" 76 N N) N)) (b "IF" 81 (b "I DONT KOW WHAT GOES HERE" 80 (b "I" 79 N N) N) (b "IN" 83 (b "IMPURE" 82 N N) N))) (b "LABEL" 90 (b "J" 87 (b "IS" 86 (b "INOUT" 85 N N) N) (b "L" 89 (b "K" 88 N N) N)) (b "LITERAL" 93 (b "LINKAGE" 92 (b "LIBRARY" 91 N N) N) (b "M" 95 (b "LOOP" 94 N N) N)))))) (b "U" 144 (b "R" 120 (b "OPEN" 108 (b "NOR" 102 (b "NAND" 99 (b "N" 98 (b "MOD" 97 N N) N) (b "NEXT" 101 (b "NEW" 100 N N) N)) (b "O" 105 (b "NULL" 104 (b "NOT" 103 N N) N) (b "ON" 107 (b "OF" 106 N N) N))) (b "PORT" 114 (b "OUT" 111 (b "OTHERS" 110 (b "OR" 109 N N) N) (b "PACKAGE" 113 (b "P" 112 N N) N)) (b "PROCESS" 117 (b "PROCEDURE" 116 (b "POSTPONED" 115 N N) N) (b "Q" 119 (b "PURE" 118 N N) N)))) (b "SHARED" 132 (b "RETURN" 126 (b "REJECT" 123 (b "REGISTER" 122 (b "RECORD" 121 N N) N) (b "REPORT" 125 (b "REM" 124 N N) N)) (b "S" 129 (b "ROR" 128 (b "ROL" 127 N N) N) (b "SEVERITY" 131 (b "SELECT" 130 N N) N))) (b "SUBTYPE" 138 (b "SLL" 135 (b "SLA" 134 (b "SIGNAL" 133 N N) N) (b "SRL" 137 (b "SRA" 136 N N) N)) (b "TO" 141 (b "THEN" 140 (b "T" 139 N N) N) (b "TYPE" 143 (b "TRANSPORT" 142 N N) N))))) (b "e" 168 (b "Waveform" 156 (b "VARIABLE" 150 (b "UNTIL" 147 (b "UNITS" 146 (b "UNAFFECTED" 145 N N) N) (b "V" 149 (b "USE" 148 N N) N)) (b "WHEN" 153 (b "WAIT" 152 (b "W" 151 N N) N) (b "WITH" 155 (b "WHILE" 154 N N) N))) (b "_" 162 (b "XOR" 159 (b "XNOR" 158 (b "X" 157 N N) N) (b "Z" 161 (b "Y" 160 N N) N)) (b "body" 165 (b "b" 164 (b "a" 163 N N) N) (b "d" 167 (b "c" 166 N N) N)))) (b "q" 180 (b "k" 174 (b "h" 171 (b "g" 170 (b "f" 169 N N) N) (b "j" 173 (b "i" 172 N N) N)) (b "n" 177 (b "m" 176 (b "l" 175 N N) N) (b "p" 179 (b "o" 178 N N) N))) (b "u" 186 (b "s" 183 (b "return" 182 (b "r" 181 N N) N) (b "t" 185 (b "signal" 184 N N) N)) (b "x" 189 (b "w" 188 (b "v" 187 N N) N) (b "z" 191 (b "y" 190 N N) N))))))
   where b s n = let bs = id s
                  in B bs (TS bs n)

unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
      deriving (Eq, Show,Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case  s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}
