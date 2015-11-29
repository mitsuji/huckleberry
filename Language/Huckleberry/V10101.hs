{-# LANGUAGE GADTs #-}

{-|
The EDSL Provides bridge between IchigoJam BASIC and Haskell.

References for IchigoJam BASIC are available at
<http://ichigojam.net/IchigoJam-1.0.1.html>.


For example,
\"Jumping rome girl Sacchan\" <http://pcn.club/ns/diprogram.en.html>
could be expressed like this.

@
sacchan = do
  label  10 $ y=:25 >> v=:99 >> x=:17 >> u=:5 >> s=:0
  label  20 $ ifThen (v\/=99) $ y=:y+v >> v=:v+1
  label  30 $ ifThen (25\<y) $ y=:25 >> v=:99 >> s=:s+1
  label  40 $ x=:x+u
  label  50 $ ifThen (17\<x) $ u=:u-1
  label  60 $ ifThen (x\<17) $ u=:u+1
  label  70 $ k=:inkey
  label  80 $ ifThen (k==32) $ v=:(-3)
  label  90 $ cls
  label 100 $ locate 17 y >> print "\@"
  label 110 $ locate x 25 >> print \"-\"
  label 120 $ locate 0  0 >> print (\"SCORE:\" ++ s)
  label 130 $ ifThen (pre(y==25)*pre(x==17)) end
  label 140 $ wait 2
  label 150 $ goto 20
@


Get IchigoJam BASIC code like this.

@
showSacchan = putStr $ translate sacchan
@

Also, you can send the code to IchigoJam directly.

@
sendSacchan = do
  h \<- IJ.opend "\/dev\/ttyUSB0"
  IJ.send h $ translate sacchan
  IJ.close h
@


To use this, your code should begin with this code block.

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}
import Prelude hiding (print,(++),(+),(-),(*),(\/),(%),(==),(\/=),(>=),(>),(\<=),(\<),(&&),(||),not,abs,return)
import qualified Prelude as P
import qualified IchigoJam as IJ
import Language.Huckleberry.V10101
@

For more example.
<https://github.com/mitsuji/huckleberry>


() is 'pre'.

Put line number by 'label'.

\[n\] is 'arr'.

LET is 'let''.

\= is '=:'.

\; is '++'.

IN is 'in''.

& is '.&.'.

| is '.|.'.

^ is 'xor'.

\>\> is 'shiftR'.

\<\< is 'shiftL'.

~ is 'complement'.

-}
module Language.Huckleberry.V10101 (
  translate
  ,pre
  ,label
  ,ifThenElse
  ,ifThen
  ,forStepNext
  ,forNext
  ,a
  ,b
  ,c
  ,d
  ,e
  ,f
  ,g
  ,h
  ,i
  ,j
  ,k
  ,l
  ,m
  ,n
  ,o
  ,p
  ,q
  ,r
  ,s
  ,t
  ,u
  ,v
  ,w
  ,x
  ,y
  ,z
  ,arr
   -- * Commands for beginners
  ,let'
  ,(=:)
  ,print
  ,(++)
  ,led
  ,wait
  ,run
  ,list
  ,list'
  ,goto
  ,end
  ,btn
  ,new
  ,locate
  ,cls
  ,rnd
  ,save
  ,save'
  ,load
  ,load'
  ,files
  ,beep
  ,beep'
  ,play
  ,play'
  ,tempo
  ,(+)
  ,(-)
  ,(*)
  ,(/)
  ,(%)
  ,input
  ,tick
  ,clt
  ,inkey
  ,chr
  ,chr'
  ,asc
  ,scroll
  ,scr
  ,scr'
  ,vpeek
  ,vpeek'
  ,(==)
  ,(/=)
  ,(>=)
  ,(>)
  ,(<=)
  ,(<)
  ,(&&)
  ,(||)
  ,not
   -- * Commands for experts
  ,clv
  ,clear
  ,clk
  ,abs
  ,gosub
  ,return
  ,sound
  ,free
  ,ver
  ,renum
  ,renum'
  ,lrun
  ,file
  ,sleep
  ,video
  ,peek
  ,poke
  ,clp
  ,help
  ,ana
  ,out
  ,out'
  ,in'
  ,in''
  ,hex
  ,hex'
  ,bin
  ,bin'
  ,(.&.)
  ,(.|.)
  ,xor
  ,shiftR
  ,shiftL
  ,complement
  ,bps
  ,i2cr
  ,i2cw
  ,usr
) where


import Prelude hiding (print,(++),(+),(-),(*),(/),(%),(==),(/=),(>=),(>),(<=),(<),(&&),(||),not,abs,return)
import Data.Int(Int16)
import Data.List(intercalate)
import qualified Prelude as P
import qualified Data.String as S
import Control.Monad.Writer(Writer,execWriter,tell)

import qualified IchigoJam as IJ





data Expr r where
  
  Number :: (Num r) => r -> Expr r
  Str :: (S.IsString r) => r -> Expr r
  
  -- Bracket operator
  Pre :: Expr Int16 -> Expr Int16

  A :: Expr Int16
  B :: Expr Int16
  C :: Expr Int16
  D :: Expr Int16
  E :: Expr Int16
  F :: Expr Int16
  G :: Expr Int16
  H :: Expr Int16
  I :: Expr Int16
  J :: Expr Int16
  K :: Expr Int16
  L :: Expr Int16
  M :: Expr Int16
  N :: Expr Int16
  O :: Expr Int16
  P :: Expr Int16
  Q :: Expr Int16
  R :: Expr Int16
  S :: Expr Int16
  T :: Expr Int16
  U :: Expr Int16
  V :: Expr Int16
  W :: Expr Int16
  X :: Expr Int16
  Y :: Expr Int16
  Z :: Expr Int16
  Array :: Expr Int16 -> Expr Int16

  Concat :: (Show a, Show b, S.IsString c) => Expr a -> Expr b -> Expr c


  --
  -- Commands for beginners
  --

  -- [TODO] [num]：0(button)/UP/DOWN/RIGHT/LEFT/SPACE, defautl 0
  Btn :: Expr Int16 -> Expr Int16

  Rnd :: Expr Int16 -> Expr Int16
  
  Add :: Expr Int16 -> Expr Int16 -> Expr Int16
  Subtract :: Expr Int16 -> Expr Int16 -> Expr Int16
  Multiply :: Expr Int16 -> Expr Int16 -> Expr Int16
  Divide :: Expr Int16 -> Expr Int16 -> Expr Int16
  Remind :: Expr Int16 -> Expr Int16 -> Expr Int16
  
  Tick :: Expr Int16
  Inkey :: Expr Int16
  
  Chr :: [Expr Int16] -> Expr String
  
  Asc :: String -> Expr Int16

  Scr :: Expr Int16 -> Expr Int16 -> Expr Int16
  Scr' :: Expr Int16
  
  Equal :: Expr Int16 -> Expr Int16 -> Expr Int16
  NotEqual :: Expr Int16 -> Expr Int16 -> Expr Int16
  GreaterThanEqual :: Expr Int16 -> Expr Int16 -> Expr Int16
  GreaterThan :: Expr Int16 -> Expr Int16 -> Expr Int16
  LessThanEqual :: Expr Int16 -> Expr Int16 -> Expr Int16
  LessThan :: Expr Int16 -> Expr Int16 -> Expr Int16
  And :: Expr Int16 -> Expr Int16 -> Expr Int16
  Or :: Expr Int16 -> Expr Int16 -> Expr Int16
  Not :: Expr Int16 -> Expr Int16


  --
  -- Commands for experts
  --
  Abs :: Expr Int16 -> Expr Int16
  Sound :: Expr Int16
  Free :: Expr Int16
  Ver :: Expr Int16
  File :: Expr Int16
  Peek :: Expr Int16 -> Expr Int16
  Ana :: Expr Int16 -> Expr Int16

  In' :: Expr Int16 -> Expr Int16
  In'' :: Expr Int16

  Hex :: Expr Int16 -> Expr Int16 -> Expr String
  Hex' :: Expr Int16 -> Expr String

  Bin :: Expr Int16 -> Expr Int16 -> Expr String
  Bin' :: Expr Int16 -> Expr String

  BitAnd :: Expr Int16 -> Expr Int16 -> Expr Int16
  BitOr ::  Expr Int16 -> Expr Int16 -> Expr Int16
  XOr ::  Expr Int16 -> Expr Int16 -> Expr Int16
  ShiftR ::  Expr Int16 -> Expr Int16 -> Expr Int16
  ShiftL ::  Expr Int16 -> Expr Int16 -> Expr Int16
  Complement ::  Expr Int16 -> Expr Int16

  I2CR :: Expr Int16 -> Expr Int16 -> Expr Int16 -> Expr Int16 -> Expr Int16 -> Expr Int16
  I2CW :: Expr Int16 -> Expr Int16 -> Expr Int16 -> Expr Int16 -> Expr Int16 -> Expr Int16
  USR :: Expr Int16 -> Expr Int16 -> Expr Int16
  

-- Num Literal
instance (Num a) => Num (Expr a) where
  (+) (Number n1) (Number n2) = Number (n1 P.+ n2)
  (-) (Number n1) (Number n2) = Number (n1 P.- n2)
  (*) (Number n1) (Number n2) = Number (n1 P.* n2)
  negate (Number n) = Number (negate n)
  abs (Number n) = Number (P.abs n)
  signum (Number n) = Number (signum n)
  fromInteger n = Number (fromInteger n)
  

-- OverloadedStrings
instance (S.IsString a) => S.IsString (Expr a) where
  fromString s = Str (S.fromString s)




data Stmt where

  Label :: Int16 -> [Stmt] -> Stmt

  -- IF could be nested
  IfThenElse :: Expr Int16 -> [Stmt] -> [Stmt] -> Stmt
  IfThen :: Expr Int16 -> [Stmt] -> Stmt
  ForStepNext :: Expr Int16 -> Expr Int16 -> Expr Int16 -> Expr Int16 -> [Stmt] -> Stmt
  ForNext :: Expr Int16 -> Expr Int16 -> Expr Int16 -> [Stmt] -> Stmt
  

  --
  -- Commands for beginners
  --
  Let' :: Expr Int16 -> [Expr Int16] -> Stmt
  Assign :: Expr Int16 -> Expr Int16 -> Stmt

  Print :: (Show r) => Expr r -> Stmt

  Led :: Expr Int16 -> Stmt
  Wait :: Expr Int16 -> Stmt
  Run :: Stmt

  List :: Expr Int16 -> Expr Int16 -> Stmt
  List' :: Stmt

  
  Goto :: Expr Int16 -> Stmt
  End :: Stmt

  New :: Stmt
  Locate ::  Expr Int16 -> Expr Int16 -> Stmt
  Cls :: Stmt

  Save :: Expr Int16 -> Stmt
  Save' :: Stmt
  
  Load :: Expr Int16 -> Stmt
  Load' :: Stmt
  
  Files :: Expr Int16 -> Stmt

  Beep :: Expr Int16 -> Expr Int16 -> Stmt
  Beep' :: Stmt

  Play :: String -> Stmt
  Play' :: Stmt
  
  Tempo :: Expr Int16 -> Stmt

  Input :: String -> Expr Int16 -> Stmt
  Clt :: Stmt

  Scroll :: Expr Int16 -> Stmt


  --
  -- Commands for experts
  --
  Clv :: Stmt
  Clk :: Stmt

  Gosub :: Expr Int16 -> Stmt
  Return :: Stmt

  Renum :: Expr Int16 -> Stmt
  Renum' :: Stmt
  
  LRun :: Expr Int16 -> Stmt

  Sleep :: Stmt
  Video :: Expr Int16 -> Stmt
  Poke :: Expr Int16 -> Expr Int16 -> Stmt
  Clp :: Stmt
  Help :: Stmt

  Out :: Expr Int16 -> Expr Int16 -> Stmt
  Out' :: Expr Int16 -> Stmt

  Bps :: Expr Int16 -> Stmt
  




reify :: (Show r) => Expr r -> String

reify (Number v) = show v
reify (Str v) = show v

reify (Pre v) = concat ["(", reify v, ")"]

reify (A) = "A"
reify (B) = "B"
reify (C) = "C"
reify (D) = "D"
reify (E) = "E"
reify (F) = "F"
reify (G) = "G"
reify (H) = "H"
reify (I) = "I"
reify (J) = "J"
reify (K) = "K"
reify (L) = "L"
reify (M) = "M"
reify (N) = "N"
reify (O) = "O"
reify (P) = "P"
reify (Q) = "Q"
reify (R) = "R"
reify (S) = "S"
reify (T) = "T"
reify (U) = "U"
reify (V) = "V"
reify (W) = "W"
reify (X) = "X"
reify (Y) = "Y"
reify (Z) = "Z"
reify (Array i) = concat ["[", reify i, "]"]

reify (Concat v1 v2) = concat [reify v1, ";", reify v2]

reify (Btn v) = concat ["BTN(", reify v, ")"]
reify (Rnd v) = concat ["RND(", reify v, ")"]

reify (Add v1 v2) = concat [reify v1, "+", reify v2]
reify (Subtract v1 v2) = concat [reify v1, "-", reify v2]
reify (Multiply v1 v2) = concat [reify v1, "*", reify v2]
reify (Divide v1 v2) = concat [reify v1, "/", reify v2]
reify (Remind v1 v2) = concat [reify v1, "%", reify v2]

reify (Tick) = "TICK()"
reify (Inkey) = "INKEY()"
reify (Chr v) = concat ["CHR$(",(intercalate "," (fmap reify v)), ")"]
reify (Asc v) = concat ["ASC(\"", v, "\")"]
reify (Scr') = concat ["SCR()"]
reify (Scr v1 v2) = concat ["SCR(", reify v1, ",", reify v2, ")"]

reify (Equal v1 v2) = concat [reify v1, "=", reify v2]
reify (NotEqual v1 v2) = concat [reify v1, "!=", reify v2]
reify (GreaterThanEqual v1 v2) = concat [reify v1, ">=", reify v2]
reify (GreaterThan v1 v2) = concat [reify v1, ">", reify v2]
reify (LessThanEqual v1 v2) = concat [reify v1, "<=", reify v2]
reify (LessThan v1 v2) = concat [reify v1, "<", reify v2]
reify (And v1 v2) = concat [reify v1, "AND", reify v2]
reify (Or v1 v2) = concat [reify v1, "OR", reify v2]
reify (Not v) = concat ["!", reify v]

reify (Abs v) = concat ["ABS(", reify v, ")"]
reify (Sound) = "SOUND()"
reify (Free) = "FREE()"
reify (Ver) = "VER()"
reify (File) = "FILE()"
reify (Peek v) = concat ["PEEK(", reify v, ")"]
reify (Ana v) = concat ["ANA(", reify v, ")"]
reify (In' v) = concat ["IN(", reify v, ")"]
reify (In'') = concat ["IN()"]
reify (Hex v1 v2) = concat ["HEX$(", reify v1, ",", reify v2, ")"]
reify (Hex' v) = concat ["HEX$(", reify v, ")"]
reify (Bin v1 v2) = concat ["BIN$(", reify v1, ",", reify v2, ")"]
reify (Bin' v) = concat ["BIN$(", reify v, ")"]

reify (BitAnd v1 v2) = concat [reify v1, "&", reify v2]
reify (BitOr v1 v2) = concat [reify v1, "|", reify v2]
reify (XOr v1 v2) = concat [reify v1, "^", reify v2]
reify (ShiftR v1 v2) = concat [reify v1, ">>", reify v2]
reify (ShiftL v1 v2) = concat [reify v1, "<<", reify v2]
reify (Complement v) = concat ["~", reify v]

reify (I2CR v1 v2 v3 v4 v5) = concat ["I2CR(", reify v1, ",", reify v2, ",", reify v3, ",", reify v4, ",", reify v5, ")"]
reify (I2CW v1 v2 v3 v4 v5) = concat ["I2CW(", reify v1, ",", reify v2, ",", reify v3, ",", reify v4, ",", reify v5, ")"]
reify (USR v1 v2) = concat ["USR(", reify v1, ",", reify v2, ")"]



lToString :: [Stmt] -> String
lToString st = intercalate ":" $ map toString st

  
toString :: Stmt -> String

toString (Label n st) = concat [(show n), " ", lToString st]
toString (IfThenElse c st1 st2) = concat ["IF", reify c, lToString st1, "ELSE", lToString st2]
toString (IfThen c st) = concat ["IF", reify c, lToString st]
toString (ForStepNext v ini inc step st) = concat ["FOR", reify v, "=", reify ini, "TO", reify inc, "STEP", reify step, ":", lToString st, ":NEXT"]
toString (ForNext v ini inc st) = concat ["FOR", reify v, "=", reify ini, "TO", reify inc, ":", lToString st, ":NEXT"]
toString (Let' v1 v2) = concat ["LET", reify v1, ",", (intercalate "," (fmap reify v2))]
toString (Assign v1 v2) = concat [reify v1, "=", reify v2]
toString (Print v) = concat ["?", reify v]
toString (Led v) = concat ["LED", reify v]
toString (Wait v) = concat ["WAIT", reify v]
toString (Run) = "RUN"
toString (List v1 v2) = concat ["LIST", reify v1, ",", reify v2]
toString (List') = "LIST"
toString (Goto v) = concat ["GOTO", reify v]
toString (End) = "END"
toString (New) = "NEW"
toString (Locate v1 v2) = concat ["LC", reify v1, ",", reify v2]
toString (Cls) = "CLS"
toString (Save v) = concat ["SAVE", reify v]
toString (Save') = "SAVE"
toString (Load v) = concat ["LOAD", reify v]
toString (Load') = "LOAD"
toString (Files v) = concat ["FILES", reify v]
toString (Beep v1 v2) = concat ["BEEP", reify v1, ",", reify v2]
toString (Beep') = "BEEP"
toString (Play v) = concat ["PLAY\"", v, "\""]
toString (Play') = "PLAY"
toString (Tempo v) = concat ["TEMPO", reify v]
toString (Input v1 v2) = concat ["INPUT\"", v1, "\",", reify v2]
toString (Clt) = "CLT"
toString (Scroll v) = concat ["SCROLL", reify v]
toString (Clv) = "CLV"
toString (Clk) = "CLK"
toString (Gosub v) = concat ["GOSUB", reify v]
toString (Return) = "RETURN"
toString (Renum v) = concat ["RENUM", reify v]
toString (Renum') = "RENUM"
toString (LRun v) = concat ["LRUN", reify v]
toString (Sleep) = "SLEEP"
toString (Video v) = concat ["VIDEO", reify v]
toString (Poke v1 v2) = concat ["POKE", reify v1, ",", reify v2]
toString (Clp) = "CLP"
toString (Help) = "HELP"
toString (Out v1 v2) = concat ["OUT", reify v1, ",", reify v2]
toString (Out' v) = concat ["OUT", reify v]
toString (Bps v) = concat ["BPS", reify v]




type Code = Writer [Stmt]

{-| Translate huckleberry code to IchigoJam BASIC code.

-}
translate :: Code() -> String
translate c = (intercalate "\n" $ map toString (execWriter c)) P.++ "\n"

{-| Bracket operator.

the expression

@
pre(y==25)*pre(x==17)
@

evaluated as this expression in IchigoJam BASIC. 

@
(Y=25)*(X=17)
@

-}
pre = Pre

{-| Line number statement.

this expression

@
label  40 $ x=:x+u
@

evaluated as this statement in IchigoJam BASIC.

@
40 X=X+U
@

-}
label :: Int16 -> Code() -> Code()
label l st = tell [Label l (execWriter st)]

{-| IF .. THEN .. ELSE .. statement.

-}
ifThenElse :: Expr Int16 -> Code() -> Code() -> Code()
ifThenElse c st1 st2 = tell [IfThenElse c (execWriter st1) (execWriter st2)]

{-| IF .. THEN .. statement.

-}
ifThen :: Expr Int16 -> Code() -> Code()
ifThen c st = tell [IfThen c (execWriter st)]

{-| FOR .. = .. TO .. STEP .. .. NEXT statement.

-}
forStepNext :: Expr Int16 -> Expr Int16 -> Expr Int16 -> Expr Int16 -> Code() -> Code()
forStepNext v ini inc step st = tell [ForStepNext v ini inc step (execWriter st)]

{-| FOR .. = .. TO .. .. NEXT statement.

-}
forNext :: Expr Int16 -> Expr Int16 -> Expr Int16 -> Code() -> Code()
forNext v ini inc st = tell [ForNext v ini inc (execWriter st)]


a = A
b = B
c = C
d = D
e = E
f = F
g = G
h = H
i = I
j = J
k = K
l = L
m = M
n = N
o = O
p = P
q = Q
r = R
s = S
t = T
u = U
v = V
w = W
x = X
y = Y
z = Z

{-| Array valiables expression.

This expression

@
arr 3
@

evaluated as this expression in IchigoJam BASIC.

@
\[3\]
@
-}
arr = Array



--
-- Commands for beginners
--

{-| LET statement.

This expression

@
let' a [3]
@

evaluated as this statement in IchigoJam BASIC. 

@
LET A,3
@

Also, this expression

@
let' (arr 3) [11,12,13]
@

evaluated as this statement in IchigoJam BASIC.

@
LET[3],11,12,13
@
-}
let' :: Expr Int16 -> [Expr Int16] -> Code()
let' v1 v2  = tell [Let' v1 v2]

{-| Assignment operator.(\=)

This expression

@
x =: x+u
@

evaluated as this statement in IchigoJam BASIC.

@
X=X+U
@

-}
(=:) :: Expr Int16 -> Expr Int16 -> Code()
(=:) v1 v2  = tell [Assign v1 v2]
infix 2 =:

{-| PRINT statement.

-}
print :: (Show r) => Expr r -> Code()
print x = tell [Print x]

{-| Concatation operator.(;)

-}
(++) :: (Show a, Show b, S.IsString c) => Expr a -> Expr b -> Expr c
(++) = Concat
infixl 2 ++

led :: Expr Int16 -> Code()
led s = tell [Led s]

wait :: Expr Int16 -> Code()
wait t = tell [Wait t]

run :: Code()
run = tell [Run]

list :: Expr Int16 -> Expr Int16 -> Code()
list v1 v2 = tell [List v1 v2]

list' :: Code()
list' = tell [List']

goto :: Expr Int16 -> Code()
goto t = tell [Goto t]

end :: Code()
end = tell [End]

btn = Btn

new :: Code()
new = tell [New]

locate ::  Expr Int16 -> Expr Int16 -> Code()
locate v1 v2 = tell [Locate v1 v2]

cls :: Code()
cls = tell [Cls]

rnd = Rnd

save :: Expr Int16 -> Code()
save v = tell [Save v]
save' :: Code()
save' = tell [Save']

load :: Expr Int16 -> Code()
load v = tell [Load v]
load' :: Code()
load' = tell [Load']

files :: Expr Int16 -> Code()
files v = tell [Files v]

beep :: Expr Int16 -> Expr Int16 -> Code()
beep v1 v2 = tell [Beep v1 v2]
beep' :: Code()
beep' = tell [Beep']

play :: String -> Code()
play c = tell [Play c]
play' :: Code()
play' = tell [Play']

tempo :: Expr Int16 -> Code()
tempo t = tell [Tempo t]

(+) = Add
infixl 6 +

(-) = Subtract
infixl 6 -

(*) = Multiply
infixl 7 *

(/) = Divide
infixl 7 /

(%) = Remind
infixl 7 %

input :: String -> Expr Int16 -> Code()
input s v = tell [Input s v]

tick = Tick

clt :: Code()
clt = tell [Clt]

inkey = Inkey

chr :: Expr Int16 -> Expr String
chr v = Chr [v]

{-|

-}
chr' :: [Expr Int16] -> Expr String
chr' = Chr

asc = Asc

scroll :: Expr Int16 -> Code()
scroll v = tell [Scroll v]

scr = Scr
scr' = Scr'
vpeek = scr -- alias
vpeek' = scr' -- alias

(==) = Equal
infix 5 ==

(/=) = NotEqual
infix 5 /=

(>=) = GreaterThanEqual
infix 5 >=

(>) = GreaterThan
infix 5 >

(<=) = LessThanEqual
infix 5 <=

(<) = LessThan
infix 5 <

(&&) = And
infixr 4 &&

(||) = Or
infixr 3 ||

not = Not



--
-- Commands for experts
--

clv :: Code()
clv = tell [Clv]
clear = clv -- alias

clk :: Code()
clk = tell [Clk]

abs = Abs

gosub :: Expr Int16 -> Code()
gosub t = tell [Gosub t]

return :: Code()
return = tell [Return]

sound = Sound

free = Free

ver = Ver

renum :: Expr Int16 -> Code()
renum v = tell [Renum v]
renum' :: Code()
renum' = tell [Renum']

lrun :: Expr Int16 -> Code()
lrun v = tell [LRun v]

file = File

sleep :: Code()
sleep = tell [Sleep]

video :: Expr Int16 -> Code()
video sw = tell [Video sw]

peek = Peek

poke :: Expr Int16 -> Expr Int16 -> Code()
poke v1 v2 = tell [Poke v1 v2]

clp :: Code()
clp = tell [Clp]

help :: Code()
help = tell [Help]

ana = Ana

out :: Expr Int16 -> Expr Int16 -> Code()
out v1 v2 = tell [Out v1 v2]
out' :: Expr Int16 -> Code()
out' v = tell [Out' v]

{-| IN expression.

-}
in' = In'
in'' = In''

hex = Hex
hex' = Hex'

bin = Bin
bin' = Bin'

{-| Bitwise AND operator (&)

-}
(.&.) = BitAnd
infixl 7 .&.

{-| Bitwise OR operator (|)

-}
(.|.) = BitOr
infixl 6 .|.

{-| Bitwise XOR expression (^)

-}
xor = XOr
infixl 7 `xor`

{-| Right shift expression (>>)

-}
shiftR = ShiftR
infixl 7 `shiftR`

{-| Left shift expression (\<\<)

-}
shiftL = ShiftL
infixl 7 `shiftL`

{-| Complement expression (~)

-}
complement = Complement

bps :: Expr Int16 -> Code()
bps s = tell [Bps s]

i2cr = I2CR

i2cw = I2CW

usr = USR

