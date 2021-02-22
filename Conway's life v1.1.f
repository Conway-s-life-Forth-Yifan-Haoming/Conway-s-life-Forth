
{ ---- This is program which implement Conway's life rule. ----- }
{ ---- The rule of life was implemented and can be altered later ---- }
{ ---- Two arrays was used in the program ---- }
{ ---- The program first fill the first array with random generated 1(live) and 0(dead) ---- }
{ ---- Then, start looping on the first array to check the position of each cell ---- }
{ ---- Check the current state of the cell and the number of live neighbors ---- }
{ ---- Apply rule of life to that cell ---- }
{ ---- copy the result to the second array ---- }


{ --------------------Global variable-------------------- }


VARIABLE DIM                    { -------- dimension of the grid ------- }
VARIABLE NOL                    { -------- Number of live neighbor of a particular cell --------- }
VARIABLE I1                        { -------- intermeidate variable --------- }
VARIABLE NOG                   { -------- numebr of generations --------- }
VARIABLE NOLC1                { -------- Total Number of living cell of grid 1 --------- }
VARIABLE NOLC2                { -------- Total Number of living cell of grid 1 --------- }
VARIABLE NOB                    { ---------- number of cell born ----------- }
VARIABLE NOD                    { ---------- number of cell died ----------- }
0 NOLC1 !                            { ---------- Initialized the variables ------------ } 
0 NOLC2 !          
0 NOB !
0 NOD !
0 NOG !
10 DIM !
0 NOL !
DIM @ DUP * ALLOCATE DROP CONSTANT LA1
DIM @ DUP * ALLOCATE DROP CONSTANT LA2


{ ----------- random number generation(Taken from Conway's life helper code) ----------- }


CREATE SEED 123456789 ,
: RND
SEED
DUP >R
@ 127773 /MOD
2836 * SWAP 16807 *
2DUP > IF -
ELSE - 2147483647 +
THEN DUP R> !
SWAP MOD
;  


{ --------- text output for Conway's life ------------ }

{ --------- This program takes the statistics of each generation and output on a seperate window -------- }


10 constant L-Spaces    { Set value of number of leading Spaces for right jst.}

8  constant Char-Pixels { Amount / pixels per text char for right just. outpt.}


: Text-Out  ( hdc -- )     { Output formatted text - numbers right justified  }

 {  DUP Font-Handle @ SelectObject drop  \ set font type here }

   DUP ( hdc)  10  20 S" Number of generations       "      TextOut DROP  { Write titles to window }
   DUP ( hdc)  80 20 S" "      TextOut DROP
   DUP ( hdc)  L-Spaces NOG @ (.) 
               swap drop - Char-Pixels *
               120 + 20 NOG @ (.)        TextOut DROP

   DUP ( hdc)  10  40 S" Number of living cells       "      TextOut DROP
   DUP ( hdc)  80 40 S" "      TextOut DROP
   DUP ( hdc)  L-Spaces NOLC1 @ (.)
               swap drop - Char-Pixels *
               120 + 40 NOLC1 @ (.)        TextOut DROP

   DUP ( hdc)  10  60 S" Number of cell born       "      TextOut DROP
   DUP ( hdc)  80 60 S" "      TextOut DROP
   DUP ( hdc)  L-Spaces NOB @   (.)
               swap drop - Char-Pixels *
               120 + 60 NOB @   (.)      TextOut DROP

   DUP ( hdc)  10  80 S" Number of cell died       "      TextOut DROP
   DUP ( hdc)  80 80 S" "      TextOut DROP
   DUP ( hdc)  L-Spaces NOD @   (.)
               swap drop - Char-Pixels *
               120 + 80 NOD @   (.)      TextOut DROP
   DROP
   ;



0 VALUE hAPP-Text                 { Handle for the Text Window                }


CREATE CLASSNAME ,Z" Text-Output" { Classname                                 }


[SWITCH MESSAGE-HANDLER DEFWINPROC ( -- res )
   ( no behavior yet)
SWITCH]


:NONAME ( -- res )
   MSG LOWORD MESSAGE-HANDLER ;  4 CB: APPLICATION-CALLBACK


:PRUNE   ?PRUNE -EXIT
   hAPP-Text IF hAPP-Text WM_CLOSE 0 0 SendMessage DROP THEN
   CLASSNAME HINST UnregisterClass DROP ;


{ --------------------------- Register the class ---------------------------- }


: REGISTER-CLASS ( -- )
   CLASSNAME APPLICATION-CALLBACK DefaultClass DROP ;


{ ------------------------ Create and show the window ----------------------- }

: CREATE-TEXT-WINDOW ( -- handle )
      0                                 \ extended style
      CLASSNAME                         \ window class name
      Z" Statistics"                    \ window caption
      WS_OVERLAPPEDWINDOW               \ window style
      40 40 250 220                     \ static initial position and size
      0                                 \ parent window handle
      0                                 \ window menu handle
      HINST                             \ program instance handle
      0                                 \ creation parameter
   CreateWindowEx ;


: START-TEXT-WINDOW ( -- flag )
   REGISTER-CLASS CREATE-TEXT-WINDOW DUP IF
      DUP SW_NORMAL ShowWindow DROP
      DUP UpdateWindow DROP
      TO hAPP-Text
   THEN ;


{ ---------------  Encapsulate the functionality of the program ------------- }


50 Constant Text-Update-Timer   { Sets Text windows update rate     }


: TEXT-CREATE ( -- res ) HWND 0 Text-Update-Timer 0 SetTimer DROP  0 ; 


: REFRESH ( -- )
   HWND 0 0 InvalidateRect DROP ;


: REPAINT ( -- res )
   HWND PAD BeginPaint ( hdc)
   HWND HERE GetClientRect DROP
   ( hdc)  Text-Out
   HWND PAD EndPaint DROP  0 ;


[+SWITCH MESSAGE-HANDLER ( -- res )
   WM_CREATE RUNS TEXT-CREATE
   WM_TIMER  RUNS REFRESH
   WM_PAINT  RUNS REPAINT
   WM_CLOSE  RUN: HWND DestroyWindow DROP 0 TO hAPP-Text  0 ;
   WM_DESTROY RUN: 0 'MAIN @ ?EXIT PostQuitMessage ;
SWITCH]




: Text-Test1             { Simple time limited test routine }
  NOG
  NOLC1
  NOB
  NOD
 ;

: Text-Test2             { Simple time limited test routine }
  NOG
  NOLC2
  NOB
  NOD
 ;



{ ----------- Add cell to the array ---------- }
{ ----------- LA1_!(n1, n2, n3) -- assign n1 to the (n2, n3) of the array ---------- }
: LA1_!
LA1 ROT 1 - DIM @ * ROT + 1 - + C! ;  

: LA2_!
LA2 ROT 1 - DIM @ * ROT + 1 - + C! ;  

{ --------- Print the value of the element of the given coordinate --------- }
{ ----------- LA1_!(n1, n2)--output the value of (n2, n3) of the array to the top of the stack ---------- }
: LA1_@
LA1 ROT 1 - DIM @ * ROT + 1 - + C@ ;  

: LA2_@
LA2 ROT 1 - DIM @ * ROT + 1 - + C@ ;  


{ ------------- Show the array ------------ }
: SHOW_LA1
DIM @ DUP * 1 - 0 DO
CR
DIM @ 0 DO
LA1 I + J + C@ .
LOOP
DIM @ +LOOP
;

: SHOW_LA2
DIM @ DUP * 1 - 0 DO
CR
DIM @ 0 DO
LA2 I + J + C@ .
LOOP
DIM @ +LOOP
;
{ ------------- Random fill the array with 1(live) and 0 (dead) -------------- }
: RND_FILL_LA1
DIM @ DUP *  0 DO
2 RND I DUP DIM @ / 1 + SWAP DIM @ MOD 1 +
LA1_!
LOOP
;  

{ ------------ Check the number of neighbors alive and add to the count -------------- }
: N_DOA
1 =
IF NOL @ 1 + NOL !
ELSE NOL @ 0 + NOL !
THEN ;

{ ------------ Calculate the number of live neighbors of the ceter part of the grid ----------- }
: CAL_NOL_C_LA1
I1 @ DIM @ - 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ - 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ + 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ + 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
;

{ ------------ Calculate the number of live neighbors of the edge part of the grid ----------- }

: CAL_NOL_L_LA1
I1 @ 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ - 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ + 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ DUP + + 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ + 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
;

: CAL_NOL_R_LA1
I1 @ DIM @ - 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ DUP + - 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ - 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ + 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ DIM @ + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
I1 @ 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ N_DOA
;

: CAL_NOL_U_LA1
DIM @ I1 @ DIM @ MOD LA1_@ N_DOA
DIM @ I1 @ DIM @ MOD 1 + LA1_@ N_DOA
DIM @ I1 @ DIM @ MOD 2 + LA1_@ N_DOA
1 I1 @ LA1_@ N_DOA
1 I1 @ 2 + LA1_@ N_DOA
2 I1 @ DIM @ MOD LA1_@ N_DOA
2 I1 @ DIM @ MOD 1 + LA1_@ N_DOA
2 I1 @ DIM @ MOD 2 + LA1_@ N_DOA
;

: CAL_NOL_D_LA1
DIM @ 1 - I1 @ DIM @ MOD LA1_@ N_DOA
DIM @ 1 - I1 @ DIM @ MOD 1 + LA1_@ N_DOA
DIM @ 1 - I1 @ DIM @ MOD 2 + LA1_@ N_DOA
DIM @ I1 @ DIM @ MOD LA1_@ N_DOA
DIM @ I1 @ DIM @ MOD 2 + LA1_@ N_DOA
1 I1 @ DIM @ MOD LA1_@ N_DOA
1 I1 @ DIM @ MOD 1 + LA1_@ N_DOA
1 I1 @ DIM @ MOD 2 + LA1_@ N_DOA
;

{ ------------ Calculate the number of live neighbors of the corner part of the grid ----------- }

: CAL_NOL_LU_LA1
DIM @ DUP LA1_@ N_DOA
DIM @ 1 LA1_@ N_DOA
DIM @ 2 LA1_@ N_DOA
1 DIM @ LA1_@ N_DOA
1 2 LA1_@ N_DOA
2 DIM @ LA1_@ N_DOA
2 1 LA1_@ N_DOA
2 2 LA1_@ N_DOA
;

: CAL_NOL_RU_LA1
DIM @ DUP 1 - LA1_@ N_DOA
DIM @ DUP LA1_@ N_DOA
DIM @ 1 LA1_@ N_DOA
1 DIM @ 1 - LA1_@ N_DOA
1 1 LA1_@ N_DOA
2 DIM @ 1 - LA1_@ N_DOA
2 DIM @ LA1_@ N_DOA
2 1 LA1_@ N_DOA
;

: CAL_NOL_LD_LA1
DIM @ 1 - DIM @ LA1_@ N_DOA
DIM @ 1 - 1 LA1_@ N_DOA
DIM @ 1 - 2 LA1_@ N_DOA
DIM @ DUP LA1_@ N_DOA
DIM @ 2 LA1_@ N_DOA
1 DIM @ LA1_@ N_DOA
1 1 LA1_@ N_DOA
1 2 LA1_@ N_DOA
;

: CAL_NOL_RD_LA1
DIM @ 1 - DUP LA1_@ N_DOA
DIM @ 1 - DIM @ LA1_@ N_DOA
DIM @ 1 - 1 LA1_@ N_DOA
DIM @ DUP 1 - LA1_@ N_DOA
DIM @ 1 LA1_@ N_DOA
1 DIM @ 1 - LA1_@ N_DOA
1 DIM @ LA1_@ N_DOA
1 1 LA1_@ N_DOA
;

{ ---------------- Check if the cell is at the left or right edge of the grid ------------- }

: LR_EDGE_LA1
I1 @ DIM @ MOD
CASE 
0 OF CAL_NOL_L_LA1 ENDOF 
DIM @ 1 - OF CAL_NOL_R_LA1 ENDOF
CAL_NOL_C_LA1
ENDCASE
;

{ ---------------- Check if the cell at the top is at the edge or corner --------------- }
: CORNER_OR_NOT_UP_LA1
I1 @ DIM @ MOD 
CASE
0 OF CAL_NOL_LU_LA1 ENDOF
DIM @ 1 - OF CAL_NOL_RU_LA1 ENDOF
CAL_NOL_U_LA1
ENDCASE
;

{ ---------------- Check if the cell at the bottom is at the edge or corner --------------- }
: CORNER_OR_NOT_DOWN_LA1
I1 @ DIM @ MOD 
CASE
0 OF CAL_NOL_LD_LA1 ENDOF
DIM @ 1 - OF CAL_NOL_RD_LA1 ENDOF
CAL_NOL_D_LA1
ENDCASE
;

{ ---------------- Check if the cell at the top or the bottom of the grid --------------- }

: UD_EDGE_LA1
I1 @ DIM @ /
CASE
0 OF CORNER_OR_NOT_UP_LA1 ENDOF
DIM @ 1 - OF CORNER_OR_NOT_DOWN_LA1 ENDOF
LR_EDGE_LA1
ENDCASE
;



{ -------------- rule of live -------------- }
: LIVE_CASE_LA1
CASE
0 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! NOD @ 1 + NOD ! ENDOF
1 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! NOD @ 1 + NOD ! ENDOF
2 OF 1 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! ENDOF
3 OF 1 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! ENDOF
4 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! NOD @ 1 + NOD ! ENDOF
5 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! NOD @ 1 + NOD ! ENDOF
6 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! NOD @ 1 + NOD ! ENDOF
7 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! NOD @ 1 + NOD ! ENDOF
8 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! NOD @ 1 + NOD ! ENDOF
." SOMETHING WRONG " 
ENDCASE ;

{ --------------- rule of dead --------------- }
: DEAD_CASE_LA1
CASE
0 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! ENDOF
1 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! ENDOF
2 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! ENDOF
3 OF 1 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! NOB @ 1 + NOB ! ENDOF
4 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! ENDOF
5 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! ENDOF
6 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! ENDOF
7 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! ENDOF
8 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_! ENDOF
." SOMETHING WRONG " 
ENDCASE ;

{ --------------- Check if the cell is dead or live after checking its neighbors -------------- }

: DOA_LA1
1 =
IF 
NOL @ LIVE_CASE_LA1 
NOLC1 @ 1 + NOLC1 !
ELSE 
NOL @ DEAD_CASE_LA1
THEN ;

{ --------------- The outer loop of the program ----------------- }

: NEW_GENERATION1
0 NOB !
0 NOD !
0 NOLC2 !
DIM @ DUP * 0 DO
I I1 !
UD_EDGE_LA1
I DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_@ DOA_LA1
0 NOL !
LOOP
NOG @ 1 + NOG !
." GENERATION " NOG @ . ." COMPLETE "
CR
NOLC1 @ . ." LIVING CELLS "
CR
NOB @ . ." CELLS WILL BORN "
CR
NOD @ . ." CELLS DYING "
CR
NOLC1 @ DIM @ DUP * MOD . ." % ACTIVITY "
CR 
NOLC1 @ NOLC2 @ - . ." % CHANGE IN ACTIVITY "
CR
TEXT-TEST1
;

{ ---- The following section is the reverse of the above program ---- }
{ ---- This part is used after one generation is completed ---- }
{ ---- Instead of creating a new array, we reuse the first array and apply the same procedure ---- }

: CAL_NOL_C_LA2
I1 @ DIM @ - 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ - 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ + 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ + 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
;

: CAL_NOL_L_LA2
I1 @ 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ - 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ + 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ DUP + + 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ + 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
;

: CAL_NOL_R_LA2
I1 @ DIM @ - 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ DUP + - 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ - 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ + 1 - DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ DIM @ + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
I1 @ 1 + DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ N_DOA
;


: CAL_NOL_LU_LA2
DIM @ DUP LA2_@ N_DOA
DIM @ 1 LA2_@ N_DOA
DIM @ 2 LA2_@ N_DOA
1 DIM @ LA2_@ N_DOA
1 2 LA2_@ N_DOA
2 DIM @ LA2_@ N_DOA
2 1 LA2_@ N_DOA
2 2 LA2_@ N_DOA
;

: CAL_NOL_RU_LA2
DIM @ DUP 1 - LA2_@ N_DOA
DIM @ DUP LA2_@ N_DOA
DIM @ 1 LA2_@ N_DOA
1 DIM @ 1 - LA2_@ N_DOA
1 1 LA2_@ N_DOA
2 DIM @ 1 - LA2_@ N_DOA
2 DIM @ LA2_@ N_DOA
2 1 LA2_@ N_DOA
;

: CAL_NOL_U_LA2
DIM @ I1 @ DIM @ MOD LA2_@ N_DOA
DIM @ I1 @ DIM @ MOD 1 + LA2_@ N_DOA
DIM @ I1 @ DIM @ MOD 2 + LA2_@ N_DOA
1 I1 @ LA2_@ N_DOA
1 I1 @ 2 + LA2_@ N_DOA
2 I1 @ DIM @ MOD LA2_@ N_DOA
2 I1 @ DIM @ MOD 1 + LA2_@ N_DOA
2 I1 @ DIM @ MOD 2 + LA2_@ N_DOA
;

: CAL_NOL_LD_LA2
DIM @ 1 - DIM @ LA2_@ N_DOA
DIM @ 1 - 1 LA2_@ N_DOA
DIM @ 1 - 2 LA2_@ N_DOA
DIM @ DUP LA2_@ N_DOA
DIM @ 2 LA2_@ N_DOA
1 DIM @ LA2_@ N_DOA
1 1 LA2_@ N_DOA
1 2 LA2_@ N_DOA
;

: CAL_NOL_RD_LA2
DIM @ 1 - DUP LA2_@ N_DOA
DIM @ 1 - DIM @ LA2_@ N_DOA
DIM @ 1 - 1 LA2_@ N_DOA
DIM @ DUP 1 - LA2_@ N_DOA
DIM @ 1 LA2_@ N_DOA
1 DIM @ 1 - LA2_@ N_DOA
1 DIM @ LA2_@ N_DOA
1 1 LA2_@ N_DOA
;

: CAL_NOL_D_LA2
DIM @ 1 - I1 @ DIM @ MOD LA2_@ N_DOA
DIM @ 1 - I1 @ DIM @ MOD 1 + LA2_@ N_DOA
DIM @ 1 - I1 @ DIM @ MOD 2 + LA2_@ N_DOA
DIM @ I1 @ DIM @ MOD LA2_@ N_DOA
DIM @ I1 @ DIM @ MOD 2 + LA2_@ N_DOA
1 I1 @ DIM @ MOD LA2_@ N_DOA
1 I1 @ DIM @ MOD 1 + LA2_@ N_DOA
1 I1 @ DIM @ MOD 2 + LA2_@ N_DOA
;

: LR_EDGE_LA2
I1 @ DIM @ MOD
CASE 
0 OF CAL_NOL_L_LA2 ENDOF 
DIM @ 1 - OF CAL_NOL_R_LA2 ENDOF
CAL_NOL_C_LA2
ENDCASE
;

: CORNER_OR_NOT_UP_LA2
I1 @ DIM @ MOD 
CASE
0 OF CAL_NOL_LU_LA2 ENDOF
DIM @ 1 - OF CAL_NOL_RU_LA2 ENDOF
CAL_NOL_U_LA2
ENDCASE
;

: CORNER_OR_NOT_DOWN_LA2
I1 @ DIM @ MOD 
CASE
0 OF CAL_NOL_LD_LA2 ENDOF
DIM @ 1 - OF CAL_NOL_RD_LA2 ENDOF
CAL_NOL_D_LA2
ENDCASE
;

: UD_EDGE_LA2
I1 @ DIM @ /
CASE
0 OF CORNER_OR_NOT_UP_LA2 ENDOF
DIM @ 1 - OF CORNER_OR_NOT_DOWN_LA2 ENDOF
LR_EDGE_LA2
ENDCASE
;



: LIVE_CASE_LA2
CASE
0 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! NOD @ 1 + NOD ! ENDOF
1 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! NOD @ 1 + NOD ! ENDOF
2 OF 1 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! ENDOF
3 OF 1 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! ENDOF
4 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! NOD @ 1 + NOD ! ENDOF
5 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! NOD @ 1 + NOD ! ENDOF
6 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! NOD @ 1 + NOD ! ENDOF
7 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! NOD @ 1 + NOD ! ENDOF
8 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! NOD @ 1 + NOD ! ENDOF
." SOMETHING WRONG " 
ENDCASE ;

: DEAD_CASE_LA2
CASE
0 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! ENDOF
1 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! ENDOF
2 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! ENDOF
3 OF 1 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! NOB @ 1 + NOB ! ENDOF
4 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! ENDOF
5 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! ENDOF
6 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! ENDOF
7 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! ENDOF
8 OF 0 I1 @ DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA1_! ENDOF
." SOMETHING WRONG " 
ENDCASE ;



: DOA_LA2
1 =
IF 
NOL @ LIVE_CASE_LA2
NOLC2 @ 1 + NOLC2 !
ELSE 
NOL @ DEAD_CASE_LA2
THEN ;


: NEW_GENERATION2
0 NOLC1 !
0 NOB !
0 NOD !
DIM @ DUP * 0 DO
I I1 !
UD_EDGE_LA2
I DUP DIM @ / 1 + SWAP DIM @ MOD 1 + LA2_@ DOA_LA2
0 NOL !
LOOP
NOG @ 1 + NOG !
." GENERATION " NOG @ . ." COMPLETE"
CR
NOLC2 @ . ." LIVING CELLS "
CR
NOB @ . ." CELLS WILL BORN "
CR
NOD @ . ." CELLS DYING "
CR
NOLC2 @ DIM @ DUP * MOD . ." % ACTIVITY "
CR 
NOLC2 @ NOLC1 @ - . ." % CHANGE IN ACTIVITY "
CR
Text-Test2
;

: 3_GENERATIONS
START-TEXT-WINDOW
RND_FILL_LA1
SHOW_LA1
CR
NEW_GENERATION1
SHOW_LA2
CR
NEW_GENERATION2
SHOW_LA1
CR
NEW_GENERATION1
SHOW_LA2
CR
;



3_generations

