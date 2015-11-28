{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (print,(++),(+),(-),(*),(/),(%),(==),(/=),(>=),(>),(<=),(<),(&&),(||),not,abs,return)
import qualified Prelude as P
import qualified IchigoJam as IJ
import Language.Huckleberry.V10101


main = P.putStrLn "example"

port = "/dev/ttyUSB0"

{--
10 Y=25:V=99:X=17:U=5:S=0
20 IF V!=99 Y=Y+V:V=V+1
30 IF 25<Y Y=25:V=99:S=S+1
40 X=X+U
50 IF 17<X U=U-1
60 IF X<17 U=U+1
70 K=INKEY()
80 IF K=32 V=-3
90 CLS
100 LOCATE 17,Y:PRINT"@"
110 LOCATE X,25:PRINT"-"
120 LOCATE 0,0:PRINT"SCORE:";S
130 IF (Y=25)*(X=17) END
140 WAIT 2
150 GOTO 20
--}
sacchan0 = do
  label  10 $ y=:25 >> v=:99 >> x=:17 >> u=:5 >> s=:0
  label  20 $ ifThen (v/=99) $ y=:y+v >> v=:v+1
  label  30 $ ifThen (25<y) $ y=:25 >> v=:99 >> s=:s+1
  label  40 $ x=:x+u
  label  50 $ ifThen (17<x) $ u=:u-1
  label  60 $ ifThen (x<17) $ u=:u+1
  label  70 $ k=:inkey
  label  80 $ ifThen (k==32) $ v=:(-3)
  label  90 $ cls
  label 100 $ locate 17 y >> print "@"
  label 110 $ locate x 25 >> print "-"
  label 120 $ locate 0  0 >> print ("SCORE:" ++ s)
  label 130 $ ifThen (pre(y==25)*pre(x==17)) end
  label 140 $ wait 2
  label 150 $ goto 20


sacchan1 = do
  let playerYPos = y
  let playerYVel = v
  let ropeXPos   = x
  let ropeXVel   = u
  let score      = s
  
  label 10 $ do
    playerYPos =: 25
    playerYVel =: 99
    ropeXPos   =: 17
    ropeXVel   =: 5
    score      =: 0
    
  label 20 $ ifThen (playerYVel /= 99) $ do
    playerYPos +=: playerYVel -- move player
    playerYVel +=: 1          -- accelerate player
  
  label 30 $ ifThen (25 < playerYPos) $ do
    playerYPos =: 25 -- reset home position
    playerYVel =: 99
    score +=: 1 -- score up !!
  
  label 40 $ ropeXPos +=: ropeXVel -- move rope

  -- accelerate rope
  label 50 $ ifThen (17 < ropeXPos) $ ropeXVel -=: 1
  label 60 $ ifThen (ropeXPos < 17) $ ropeXVel +=: 1
  
  label 70 $ k =: inkey
  label 80 $ ifThen (k==32) $ playerYVel =: (-3)
  
  label  90 $ cls
  label 100 $ print' 17 playerYPos "@"
  label 110 $ print' ropeXPos 25 "-"
  label 120 $ print' 0 0 ("SCORE:" ++ score)
  label 130 $ ifThen (pre(playerYPos==25)*pre(ropeXPos==17)) end
  label 140 $ wait 2
  label 150 $ goto 20
  where
    print' x y s = locate x y >> print s
    (+=:) l r = ( l =: l + r ) 
    (-=:) l r = ( l =: l - r ) 



{--
10 A=17:X=0:Y=0:U=0:V=0:C=0:D=0
20 T=0:S=-1
30 IF U*(X<=0)+U*(35<=X) U=U*-1
40 IF V*(Y<=0) V=V*-1
50 IF U X=X+U:Y=Y+V
60 IF (Y=24)*(A-3<X)*(X<A+3) V=V*-1
70 IF V*(25<Y) X=0:Y=0:U=0:V=0
80 IF U*(Y=D)*(C-3<X)*(X<C+3) T=0
90 IF T=0 S=S+1
100 IF T%30=0 C=RND(31)+2:D=RND(18)
110 K=INKEY()
120 IF K=28 IF 3<A A=A-2
130 IF K=29 IF A<31 A=A+2
140 IF K=32 X=A:Y=24:U=1:V=-1
150 CLS
160 PRINT"SCORE:";S
170 LOCATE A-2,25:PRINT"====="
180 IF U LOCATE X,Y:PRINT"O"
190 LOCATE C-2,D:PRINT"#####"
200 WAIT 2:T=T+1:GOTO 30
--}
galaxy = do
  label  10 $ a=:17 >> x=:0 >> y=:0 >> u=:0 >> v=:0 >> c=:0 >> d=:0
  label  20 $ t=:0 >> s=:(-1)
  label  30 $ ifThen (u*pre(x<=0)+u*pre(35<=x)) $ u=:u*(-1)
  label  40 $ ifThen (v*pre(y<=0))              $ v=:v*(-1)
  label  50 $ ifThen (u) $ x=:x+u >> y=:y+v
  label  60 $ ifThen (pre(y==24)*pre(a-3<x)*pre(x<a+3)) $ v=:v*(-1)
  label  70 $ ifThen (v*pre(25<y)) $ x=:0 >> y=:0 >> u=:0 >> v=:0
  label  80 $ ifThen (u*pre(y==d)*pre(c-3<x)*pre(x<c+3)) $ t=:0
  label  90 $ ifThen (t==0) $ s=:s+1
  label 100 $ ifThen (t%30==0) $ c=:rnd(31)+2 >> d=:rnd(18)
  label 110 $ k=:inkey
  label 120 $ ifThen (k==28) $ ifThen (3<a)  (a=:a-2)
  label 130 $ ifThen (k==29) $ ifThen (a<31) (a=:a+2)
  label 140 $ ifThen (k==32) $ x=:a >> y=:24 >> u=:1 >> v=:(-1)
  label 150 $ cls
  label 160 $ print $ "SCORE:" ++ s
  label 170 $ locate (a-2) 25 >> print "====="
  label 180 $ ifThen (u) $ locate x y >> print "0"
  label 190 $ locate (c-2) d  >> print "#####"
  label 200 $ wait 2 >> t=:t+1 >> goto 30



{--
10 A=12:X=0:Y=0:B=99:S=0:T=1000
20 IF X X=X+4
30 IF (34<X)*(Y=B) B=99:S=S+1
40 IF 35<X X=0
50 IF B=99 B=RND(24)+1
60 K=INKEY()
70 IF K=30 A=A-1
80 IF K=31 A=A+1
90 IF (K=32)*(X=0) X=1:Y=A
100 CLS
110 LOCATE 0,A:PRINT"}"
120 IF X LOCATE X,Y:PRINT"-"
130 LOCATE 35,B:PRINT"X"
140 LOCATE 0,0:PRINT"SCORE:";S
150 LOCATE 10,0:PRINT"TIME:";T
160 WAIT 2
170 T=T-5
180 IF T<0 LOCATE 0,25:END
190 GOTO 20;
--}
gunman = do
  label  10 $ a=:12 >> x=:0 >> y=:0 >> b=:99 >> s=:0 >> t=:1000
  label  20 $ ifThen (x) $ x=:x+4
  label  30 $ ifThen (pre(34<x)*pre(y==b)) $ b=:99 >> s=:s+1
  label  40 $ ifThen (35<x) $ x=:0
  label  50 $ ifThen (b==99) $ b=:rnd(24)+1
  label  60 $ k=:inkey
  label  70 $ ifThen (k==30) $ a=:a-1
  label  80 $ ifThen (k==31) $ a=:a+1
  label  90 $ ifThen (pre(k==32)*pre(x==0)) $ x=:1 >> y=:a
  label 100 $ cls
  label 110 $ locate 0 a >> print "}"
  label 120 $ ifThen (x) $ locate x y >> print "-"
  label 130 $ locate 35 b >> print "X"
  label 140 $ locate 0  0 >> print ("SCORE:" ++ s)
  label 150 $ locate 10 0 >> print ("TIME:" ++ t)
  label 160 $ wait 2
  label 170 $ t=:t-5
  label 180 $ ifThen (t<0) $ locate 0 25 >> end
  label 190 $ goto 20 



{--
10 X=17:Y=13:A=99:B=99:S=5:T=0
20 K=INKEY()
30 IF K=28 X=X-1:IF X<0 X=35
40 IF K=29 X=X+1:IF 35<X X=0
50 IF K=30 Y=Y-1:IF Y<0 Y=25
60 IF K=31 Y=Y+1:IF 25<Y Y=0
70 IF (X=A)*(Y=B) S=S-1:A=99:B=99
80 IF A=99 A=RND(35):B=RND(24)+1
90 CLS
100 LOCATE X,Y:PRINT"@"
110 LOCATE A,B:PRINT"0"
120 LOCATE 0,0:PRINT"BREADS:";S
130 LOCATE 12,0:PRINT"TIME:";T
140 IF S=0 END
150 WAIT 2
160 T=T+1
170 GOTO 20
--}
pakkun = do
  label  10 $ x=:17 >> y=:13 >> a=:99 >> b=:99 >> s=:5 >> t=:0
  label  20 $ k=:inkey
  label  30 $ ifThen (k==28) (x=:x-1) >> ifThen (x <0) (x=:35)
  label  40 $ ifThen (k==29) (x=:x+1) >> ifThen (35<x) (x=:0)
  label  50 $ ifThen (k==30) (y=:y-1) >> ifThen (y <0) (y=:25)
  label  60 $ ifThen (k==31) (y=:y+1) >> ifThen (25<y) (y=:0)
  label  70 $ ifThen (pre(x==a)*pre(y==b)) $ s=:s-1 >> a=:99 >> b=:99
  label  80 $ ifThen (a==99) $ a=:rnd(35) >> b=:rnd(24)+1
  label  90 $ cls
  label 100 $ locate x  y >> print "@"
  label 110 $ locate a  b >> print "0"
  label 120 $ locate 0  0 >> print ("BREADS:" ++ s)
  label 130 $ locate 12 0 >> print ("TIME:" ++ t)
  label 140 $ ifThen (s==0) end
  label 150 $ wait 2
  label 160 $ t=:t+1
  label 170 $ goto 20



{--
10 CLS:S=0:Y=21:Z=1:L=10:X=2:I=2
20 LOCATE L,I:PRINT"|  |":I=I+1
30 IF I<21 GOTO 20
40 IF S%Y=0 A=RND(2)+1:B=1
50 K=INKEY()
60 LOCATE X+L,20:PRINT" "
70 IF K=28 IF X>1 X=X-1
80 IF K=29 IF X<2 X=X+1
90 LOCATE X+L,20:PRINT"A"
100 IF B-Z<21 LOCATE L+A,B-Z:PRINT" "
110 IF B<21 LOCATE L+A,B:PRINT"V":B=B+Z
120 IF VPEEK(L+X,19)!=32 END
130 LOCATE 1,1:PRINT"SCORE:";S/10
140 S=S+1:GOTO 40
--}
gyakusou = do
  label  10 $ cls >> s=:0 >> y=:21 >> z=:1 >> l=:10 >> x=:2 >> i=:2
  label  20 $ locate l i >> print "|  |" >> i=:i+1
  label  30 $ ifThen (i<21) $ goto 20
  label  40 $ ifThen (s%y==0) $ a=:rnd(2)+1 >> b =:1
  label  50 $ k=:inkey
  label  60 $ locate (x+l) 20 >> print " "
  label  70 $ ifThen (k==28) $ ifThen (x>1) $ x=:x-1
  label  80 $ ifThen (k==29) $ ifThen (x<2) $ x=:x+1
  label  90 $ locate (x+l) 20 >> print "A"
  label 100 $ ifThen (b-z<21) $ locate (l+a)(b-z) >> print " "
  label 110 $ ifThen (  b<21) $ locate (l+a)    b >> print "V" >> b=:b+z
  label 120 $ ifThen (vpeek (l+x) 19 /= 32) end
  label 130 $ locate 1 1 >> print ("SCORE:" ++ s/10)
  label 140 $ s=:s+1 >> goto 40



{--
10 H=15:I=16:X=1:Y=1:T=0:S=0:C=100
20 IF T=0 X=RND(3):Y=RND(3)
30 T=T+1
40 CLS
50 LOCATE 0,0:PRINT"SCORE:";S
60 LOCATE 10,0:PRINT"TIME:";C
70 LOCATE H,I-0:PRINT"1 2 3"
80 LOCATE H,I-3:PRINT"4 5 6"
90 LOCATE H,I-6:PRINT"7 8 9"
100 LOCATE H+X*2,I-Y*3-1:PRINT"@"
110 IF C=0 END
120 K=INKEY()
130 IF K-49=Y*3+X S=S+1:T=0
140 IF T%10=0 T=0
150 WAIT 5
160 C=C-1
170 GOTO 20
--}
mogura = do
  label  10 $ h=:15 >> i=:16 >> x=:1 >> y=:1 >> t=:0 >> s=:0 >> c=:100
  label  20 $ ifThen (t==0) $ x=:rnd(3) >> y=:rnd(3)
  label  30 $ t=:t+1
  label  40 cls
  label  50 $ locate 0 0  >> print ("SCORE:" ++ s)
  label  60 $ locate 10 0 >> print ("TIME:" ++ c)
  label  70 $ locate h (i-0) >> print "1 2 3"
  label  80 $ locate h (i-3) >> print "4 5 6"
  label  90 $ locate h (i-6) >> print "7 8 9"
  label 100 $ locate (h+x*2) (i-y*3-1) >> print "@"
  label 110 $ ifThen (c==0) end
  label 120 $ k=:inkey
  label 130 $ ifThen (k-49==y*3+x) $ s=:s+1 >> t=:0
  label 140 $ ifThen (t%10==0) $ t=:0
  label 150 $ wait 5
  label 160 $ c=:c-1
  label 170 $ goto 20



{--
10 CLS:X=1:Y=4:P=0:V=0:A=0:B=0:C=0:D=0:E=0:T=0
20 LOCATE12,4:?"$"
30 LOCATE18,9:?"$"
40 LOCATE4,14:?"$"
50 LOCATE21,19:?"$"
60 LOCATE0,24:?"$"
70 LOCATEX,Y:?"K"
80 LOCATE0,5:?"==================================="
90 LOCATE0,10:?"==================================="
100 LOCATE0,15:?"==================================="
110 LOCATE0,20:?"==================================="
120 LOCATE0,25:?"==================================="
130 LOCATE29,0:?"$=";P
140 LOCATE0,0:?"TIME:";T
150 K=INKEY()
160 IF K=28 IF X>0 X=X-1:LOCATEX+1,Y:?" "
170 IF K=29 IF X<35 X=X+1:LOCATEX-1,Y:?" "
180 IF K=30 LOCATEX,Y:?" ":Y=Y-5
190 IF K=31 LOCATEX,Y+1:?" "
200 IF (VPEEK(X,Y+1)!=61) Y=Y+V:V=V+1:IF V>1 V=1
210 IF X=12 IF Y=4 IF A=0 P=P+1:A=1
220 IF X=18 IF Y=9 IF B=0 P=P+1:B=1
230 IF X=4 IF Y=14 IF C=0 P=P+1:C=1
240 IF X=21 IF Y=19 IF D=0 P=P+1:D=1
250 IF X=0 IF Y=24 IF E=0 P=P+1:E=1
260 IF P=5 LOCATEX,Y:?"K":LOCATE29,0:?"$=";P:END
270 IF Y=25 END
280 LOCATEX,Y-1:?" ":WAIT 1:T=T+1:GOTO 70
--}
tanken = do
  label  10 $ cls >> x=:1 >> y=:4 >> p=:0 >> v=:0 >> a=:0 >> b=:0 >> c=:0 >> d=:0 >> e=:0 >> t=:0
  label  20 $ locate 12 14 >> print "$"
  label  30 $ locate 18  9 >> print "$"
  label  40 $ locate  4 14 >> print "$"
  label  50 $ locate 21 19 >> print "$"
  label  60 $ locate  0 24 >> print "$"
  label  70 $ locate  x  y >> print "K"
  label  80 $ locate  0  5 >> print "==================================="
  label  90 $ locate  0 10 >> print "==================================="
  label 100 $ locate  0 15 >> print "==================================="
  label 110 $ locate  0 20 >> print "==================================="
  label 120 $ locate  0 25 >> print "==================================="
  label 130 $ locate 29  0 >> print ("$=" ++ p)
  label 140 $ locate  0  0 >> print ("TIME:" ++ t)
  label 150 $ k=:inkey
  label 160 $ ifThen (k==28) $ ifThen (x> 0) $ x=:x-1 >> locate (x+1) y >> print " "
  label 170 $ ifThen (k==29) $ ifThen (x<35) $ x=:x+1 >> locate (x-1) y >> print " "
  label 180 $ ifThen (k==30) $ locate x y >> print " " >> y=:y-5
  label 190 $ ifThen (k==31) $ locate x (y+1) >> print " "
  label 200 $ ifThen (vpeek x (y+1) /= 61) $ y=:y+v >> v=:v+1 >> ifThen (v>1) (v=:1)
  label 210 $ ifThen (x==12) $ ifThen (y== 4) $ ifThen (a==0) $ p=:p+1 >> a=:1
  label 220 $ ifThen (x==18) $ ifThen (y== 9) $ ifThen (b==0) $ p=:p+1 >> b=:1
  label 230 $ ifThen (x== 4) $ ifThen (y==14) $ ifThen (c==0) $ p=:p+1 >> c=:1
  label 240 $ ifThen (x==21) $ ifThen (y==19) $ ifThen (d==0) $ p=:p+1 >> d=:1
  label 250 $ ifThen (x== 0) $ ifThen (y==24) $ ifThen (e==0) $ p=:p+1 >> e=:1
  label 260 $ ifThen (p==5) $ locate x y >> print "K" >> locate 29 0 >> print ("$=" ++ p) >> end
  label 270 $ ifThen (y==25) end
  label 280 $ locate x (y-1) >> print " " >> wait 1 >> t=:t+1 >> goto 70





exArray1 = do
  label 100 $ do
    let' (arr 10) [101,202,303]
    forNext a 8 14 $ do
      print $ arr a
      print $ arr a + 3


exFor1 = do
  label 100 $ do
    forNext a 1 3 $ do
      forNext b 11 13 $ do
        print $ a ++ ":" ++ b
  
exFor2 = do
  label 100 $ do
    forStepNext a 1 30 5 $ do
      forStepNext b 11 130 6 $ do
        print $ a ++ ":" ++ b
  


exPlay1 = do
  label 100 $ do
    play "CDEFGAB"
    tempo 500
    end


exPrint1 = do
  label 100 $ do
    let' a [100]
    print "ABCD"
    print $ a+1
    print $ a+1 ++ "ABCD" ++ asc "a" ++ chr 65
    print $ chr 65 ++ chr' [65,66,67]


exButtonLed1 = do
  label 100 $ do
    let' a [0]
    led a
  
  label 110 $ do
    ifThen (btn 0) (let' a [not a])

  label 120 $ do      
    led a
    wait 30
    goto 110


exButtonLed2 = do
  label 100 $ do
    led 0
  
  label 110 $ do
    ifThenElse (btn 0) (led 1) (led 0)

  label 120 $ do      
     goto 110


exFor = do
  label 100 $ do
    forNext a 100 120 (print $ a+1)
    
  label 110 $ do
    forStepNext a 100 120 3 (print $ a+1)

  label 120 $ do
    end


exPre = do
  print $ 11 + 12 * 13
  print $ pre(11 + 12) * 13
  print $ 11 - 12 * 13
  print $ pre(11 - 12) * 13
  print $ 1 && 1
  print $ 1 && 0
  print $ 0 && 1
  print $ 0 && 0
  print $ 1 || 1
  print $ 1 || 0
  print $ 0 || 1
  print $ 0 || 0




show' c = putStr $ translate c

send' c = do
  h <- IJ.opend port
  IJ.send h $ translate c
  IJ.close h

run' = do
  h <- IJ.opend port
  IJ.send h $ "RUN\n"
  IJ.close h

new' = do
  h <- IJ.opend port
  IJ.send h $ "NEW\n"
  IJ.close h


