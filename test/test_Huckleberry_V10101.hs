{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Data.Char (toUpper)

import Prelude hiding (print,(++),(+),(-),(*),(/),(%),(==),(/=),(>=),(>),(<=),(<),(&&),(||),not,abs,return)
import qualified Prelude as P
import Language.Huckleberry.V10101


exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts P./= 0 P.|| errors counts P./= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList [ TestList huckleberryTests ]

huckleberryTests :: [Test]
huckleberryTests =
  [ testCase "number literal"        $ "?1234\n"                @=? (translate $ print 1234)
  , testCase "number literal"        $ "?102575\n"              @=? (translate $ print 0x190AF)
  , testCase "string literal"        $ "?\"abcd\"\n"            @=? (translate $ print "abcd")
  , testCase "variable"              $ "?A\n"                   @=? (translate $ print a)
  , testCase "variable"              $ "?A+1\n"                 @=? (translate $ print $ a+1)
  , testCase "array"                 $ "?[8]\n"                 @=? (translate $ print $ arr 8)
  , testCase "array"                 $ "?[A]\n"                 @=? (translate $ print $ arr a)
  , testCase "array"                 $ "?[A+1]\n"               @=? (translate $ print $ arr (a+1))
  , testCase "++"                    $ "?\"abc\";\"def\"\n"     @=? (translate $ print $ "abc" ++ "def")
  , testCase "++"                    $ "?1234;5678\n"           @=? (translate $ print $ 1234 ++ 5678)
  , testCase "++"                    $ "?\"abc\";5678\n"        @=? (translate $ print $ "abc" ++ 5678)
  , testCase "++"                    $ "?1234;\"def\"\n"        @=? (translate $ print $ 1234 ++ "def")
  , testCase "let'"                  $ "LETA,1\n"               @=? (translate $ let' a [1])
  , testCase "let'"                  $ "LETA+1,1\n"             @=? (translate $ let' (a+1) [1])
  , testCase "let'"                  $ "LETA,1,2,3\n"           @=? (translate $ let' a [1,2,3])
  , testCase "let'"                  $ "LETA+1,1,2,3\n"         @=? (translate $ let' (a+1) [1,2,3])
  , testCase "let'"                  $ "LET[8],1,2,3\n"         @=? (translate $ let' (arr 8) [1,2,3])
  , testCase "let'"                  $ "LET[A],A+1,2,3\n"       @=? (translate $ let' (arr a) [a+1,2,3])
  , testCase "let'"                  $ "LET[A+1],1,2,3\n"       @=? (translate $ let' (arr (a+1)) [1,2,3])
  , testCase "=:"                    $ "A=1\n"                  @=? (translate $ a=:1)
  , testCase "=:"                    $ "A=B\n"                  @=? (translate $ a=:b)
  , testCase "ifThenElse"            $ "IFA=B?\"S\"ELSE?\"N\"\n"
    @=? (translate $ ifThenElse (a==b) (print "S") (print "N"))
  , testCase "ifThen"                $ "IFA=B?\"S\"\n"
    @=? (translate $ ifThen (a==b) (print "S"))
  , testCase "forStepNext"           $ "FORA=0TO99STEP2:?[A+1]+2:NEXT\n"
    @=? (translate $ forStepNext a 0 99 2 (print $ arr (a+1) +2))
  , testCase "forNext"             $ "FORA=0TO99:?[A+1]+2:NEXT\n"
    @=? (translate $ forNext a 0 99 (print $ arr (a+1) +2))
  , testCase "pre"                   $ "?(A-1)*(B+1)\n"         @=? (translate $ print $ pre(a-1)*pre(b+1))
  , testCase "pre"                   $ "?A-1*B+1\n"             @=? (translate $ print $ (a-1)*(b+1))
  , testCase "led"                   $ "LEDA+1\n"               @=? (translate $ led (a+1))
  , testCase "wait"                  $ "WAITA+1\n"              @=? (translate $ wait(a+1))
  , testCase "run"                   $ "RUN\n"                  @=? (translate $ run)
  , testCase "list"                  $ "LISTA+1,B+2\n"          @=? (translate $ list (a+1) (b+2))
  , testCase "list'"                 $ "LIST\n"                 @=? (translate $ list')
  , testCase "goto"                  $ "GOTOA+1\n"              @=? (translate $ goto (a+1))
  , testCase "end"                   $ "END\n"                  @=? (translate $ end)
  , testCase "btn"                   $ "?BTN(A+1)+2\n"          @=? (translate $ print $ (btn (a+1)) +2)
  , testCase "new"                   $ "NEW\n"                  @=? (translate $ new)
  , testCase "locate"                $ "LCA+1,B+2\n"            @=? (translate $ locate (a+1) (b+2))
  , testCase "cls"                   $ "CLS\n"                  @=? (translate $ cls)
  , testCase "rnd"                   $ "?RND(A+1)+2\n"          @=? (translate $ print $ (rnd (a+1)) +2)
  , testCase "save"                  $ "SAVEA+1\n"              @=? (translate $ save (a+1))
  , testCase "save'"                 $ "SAVE\n"                 @=? (translate $ save')
  , testCase "load"                  $ "LOADA+1\n"              @=? (translate $ load (a+1))
  , testCase "load'"                 $ "LOAD\n"                 @=? (translate $ load')
  , testCase "files"                 $ "FILESA+1\n"             @=? (translate $ files (a+1))
  , testCase "beep"                  $ "BEEPA+1,B+2\n"          @=? (translate $ beep (a+1) (b+2))
  , testCase "beep'"                 $ "BEEP\n"                 @=? (translate $ beep')
  , testCase "play"                  $ "PLAY\"ABCD\"\n"         @=? (translate $ play "ABCD")
  , testCase "play'"                 $ "PLAY\n"                 @=? (translate $ play')
  , testCase "tempo"                 $ "TEMPOA+1\n"             @=? (translate $ tempo (a+1))
  , testCase "+"                     $ "?A+B\n"                 @=? (translate $ print $ a+b)
  , testCase "-"                     $ "?A-B\n"                 @=? (translate $ print $ a-b)
  , testCase "*"                     $ "?A*B\n"                 @=? (translate $ print $ a*b)
  , testCase "/"                     $ "?A/B\n"                 @=? (translate $ print $ a/b)
  , testCase "%"                     $ "?A%B\n"                 @=? (translate $ print $ a%b)
  , testCase "input"                 $ "INPUT\"Q?\",A\n"        @=? (translate $ input "Q?" a)
  , testCase "tick"                  $ "?TICK()+1\n"            @=? (translate $ print $ tick +1)
  , testCase "clt"                   $ "CLT\n"                  @=? (translate $ clt)
  , testCase "inkey"                 $ "?INKEY()+1\n"           @=? (translate $ print $ inkey +1)
  , testCase "chr"                   $ "?CHR$(A+1);\"def\"\n"   @=? (translate $ print $ chr (a+1) ++ "def")
  , testCase "chr'"                  $ "?CHR$(65,66,67)\n"      @=? (translate $ print $ chr'[65,66,67])
  , testCase "asc"                   $ "?ASC(\"abcd\")\n"       @=? (translate $ print $ asc "abcd")
  , testCase "scroll"                $ "SCROLLA+1\n"            @=? (translate $ scroll (a+1))
  , testCase "scr"                   $ "?SCR(A+1,B+2)+2\n"      @=? (translate $ print $ (scr (a+1) (b+2))+2)
  , testCase "scr'"                  $ "?SCR()+1\n"             @=? (translate $ print $ scr'+1)
  , testCase "vpeek"                 $ "?SCR(A+1,B+2)+2\n"      @=? (translate $ print $ (vpeek (a+1) (b+2))+2)
  , testCase "vpeek'"                $ "?SCR()+1\n"             @=? (translate $ print $ vpeek'+1)
  , testCase "=="                    $ "?A=B\n"                 @=? (translate $ print $ a==b)
  , testCase "/="                    $ "?A!=B\n"                @=? (translate $ print $ a/=b)
  , testCase ">="                    $ "?A>=B\n"                @=? (translate $ print $ a>=b)
  , testCase ">"                     $ "?A>B\n"                 @=? (translate $ print $ a>b)
  , testCase "<="                    $ "?A<=B\n"                @=? (translate $ print $ a<=b)
  , testCase "<"                     $ "?A<B\n"                 @=? (translate $ print $ a<b)
  , testCase "&&"                    $ "?AANDB\n"               @=? (translate $ print $ a&&b)
  , testCase "||"                    $ "?AORB\n"                @=? (translate $ print $ a||b)
  , testCase "not"                   $ "?!A\n"                  @=? (translate $ print $ not a)
  , testCase "clv"                   $ "CLV\n"                  @=? (translate $ clv)
  , testCase "clear"                 $ "CLV\n"                  @=? (translate $ clear)
  , testCase "clk"                   $ "CLK\n"                  @=? (translate $ clk)
  , testCase "abs"                   $ "?ABS(A+1)+2\n"          @=? (translate $ print $ (abs (a+1)) +2)
  , testCase "gosub"                 $ "GOSUBA+1\n"             @=? (translate $ gosub (a+1))
  , testCase "return"                $ "RETURN\n"               @=? (translate $ return)
  , testCase "sound"                 $ "?SOUND()+1\n"           @=? (translate $ print $ sound +1)
  , testCase "free"                  $ "?FREE()+1\n"            @=? (translate $ print $ free +1)
  , testCase "ver"                   $ "?VER()+1\n"             @=? (translate $ print $ ver +1)
  , testCase "renum"                 $ "RENUMA+1\n"             @=? (translate $ renum (a+1))
  , testCase "renum'"                $ "RENUM\n"                @=? (translate $ renum')
  , testCase "lrun"                  $ "LRUNA+1\n"              @=? (translate $ lrun (a+1))
  , testCase "file"                  $ "?FILE()+1\n"            @=? (translate $ print $ file +1)
  , testCase "sleep"                 $ "SLEEP\n"                @=? (translate $ sleep)
  , testCase "video"                 $ "VIDEOA+1\n"             @=? (translate $ video (a+1))
  , testCase "peek"                  $ "?PEEK(A+1)+2\n"         @=? (translate $ print $ (peek (a+1)) +2)
  , testCase "poke"                  $ "POKEA+1,B+2\n"          @=? (translate $ poke (a+1) (b+2))
  , testCase "clp"                   $ "CLP\n"                  @=? (translate $ clp)
  , testCase "help"                  $ "HELP\n"                 @=? (translate $ help)
  , testCase "ana"                   $ "?ANA(A+1)+2\n"          @=? (translate $ print $ (ana (a+1)) +2)
  , testCase "out"                   $ "OUTA+1,B+2\n"           @=? (translate $ out (a+1) (b+2))
  , testCase "out'"                  $ "OUTA+1\n"               @=? (translate $ out' (a+1))
  , testCase "in'"                   $ "?IN(A+1)+2\n"           @=? (translate $ print $ (in' (a+1)) +2)
  , testCase "in''"                  $ "?IN()+1\n"              @=? (translate $ print $ in'' +1)
  , testCase "hex"                   $ "?HEX$(A+1,B+2);\"A\"\n" @=? (translate $ print $ hex (a+1) (b+2) ++ "A")
  , testCase "hex'"                  $ "?HEX$(A+1);\"A\"\n"     @=? (translate $ print $ hex' (a+1) ++ "A")
  , testCase "bin"                   $ "?BIN$(A+1,B+2);\"A\"\n" @=? (translate $ print $ bin (a+1) (b+2) ++ "A")
  , testCase "bin'"                  $ "?BIN$(A+1);\"A\"\n"     @=? (translate $ print $ bin' (a+1) ++ "A")
  , testCase ".&."                   $ "?A&B\n"                 @=? (translate $ print $ a.&.b)
  , testCase ".|."                   $ "?A|B\n"                 @=? (translate $ print $ a.|.b)
  , testCase "xor"                   $ "?A^B\n"                 @=? (translate $ print $ a`xor`b)
  , testCase "shiftR"                $ "?A>>B\n"                @=? (translate $ print $ a`shiftR`b)
  , testCase "shiftL"                $ "?A<<B\n"                @=? (translate $ print $ a`shiftL`b)
  , testCase "complement"            $ "?~A\n"                  @=? (translate $ print $ complement a)
  , testCase "bps"                   $ "BPSA+1\n"               @=? (translate $ bps (a+1))
  , testCase "i2cr"                  $ "?I2CR(A+1,B+2,C+3,D+4,E+5)+2\n"
    @=? (translate $ print $ (i2cr (a+1) (b+2) (c+3) (d+4) (e+5)) +2)
  , testCase "i2cw"                  $ "?I2CW(A+1,B+2,C+3,D+4,E+5)+2\n"
    @=? (translate $ print $ (i2cw (a+1) (b+2) (c+3) (d+4) (e+5)) +2)
  , testCase "usr"                   $ "?USR(A+1,B+2)+2\n"      @=? (translate $ print $ (usr (a+1) (b+2)) +2)

  ]
