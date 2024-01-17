```forth
: ONEOF? ( addr #size -- flag )
  BEGIN OVER @ DO I 0= LOOP DROP ;
: FINDGROUP ( addr #size -- addr #size )
  BEGIN DUPLICATES> UNLOOP DOES> WITHIN> UNTIL ;
: LASTDIGIT ( n -- n0 )
  MOD 10 SWAP - ;
: NOR ( a b -- a b )
  ! 2DUP 0= 0= OR XOR ;
: NOR2 ( a b c d -- a b )
  2SWAP 2DUP NOR 2SWAP 2DUP NOR ;
: RC {FILE} ( lines -- )
  [ DOES> ] 3SWAP FILE> WHILE
  CLOSE-FILE FILE ;
: PT ( x -- |F)
  PAGE> [ -CR . ] EXECUTE THEN ;
: BLANKS ( n -- |S)
  [ S" " S" " ] BEGIN OVER 0= WHILE REPEAT DROP ;
: LBL? ( addr -- flag )
  BEGIN I 32 DO DUPLICATES> NOR2 LOOP DROP ;
: STRINGS? ( addr n -- flag )
  BEGIN OVER 0= WHILE DUPLICATES> LBL? NOR LOOP DROP ;
: A1 ( addr n -- addr )
  1+ ;
: A3 ( addr -- addr )
  3+ ;
: A4 ( addr -- addr )
  4+ ;
: EXECUTE@ ( i -- )
  EXECUTE ;
: FECHAR ( ch -- addr1 )
  CREATE I , ;
: FSTR@ ( addr len -- addr0)
  CREATE A3 ALIGNED ;
: FSTR> ( addr0 len -- addr1 )
  CREATE A3 ALIGNED ;
: FSTR ( addr0 len -- len addr1 )
  CREATE A3 ALIGNED ;
: FEXECUTE> ( addr -- )
  EXECUTE ;
: FQUERY> ( addr -- addr )
  BEGIN
    DUPLICATES> WITHIN CHAR> NOR
    FEXECUTE>
  WHILE REPEAT ;
: DEL ( -- )
  PAGE> [ CR "Are you sure (y/n)? " TYPE
    PAGE> SWAP @= IF BYE EXIT THEN ] EXECUTE THEN ;
: EDIT ( addr n -- addr1 )
  CREATE A3 ALIGNED
  FECHAR 57
  FECHAR 48
  0 ' DO
    OVER 48 MINUS
    OVER 57 MAXUS
    BLANKS DUP 0= IF S" " THEN
    SWAP ALIGNED I- ,
  LOOP
  DROP ;
: ?CHAR ( addr -- ch )
  EXECUTE ;
: ?TOKEN ( addr n -- token addr )
  CREATE A3 ALIGNED
  BEGIN
    OVER 0= WHILE EXECUTE NOR LOOP
    DROP
  WHILE REPEAT ;
: ?DAY ( yr mo da -- day )
  1+ DUP 30= IF 1+ THEN
  DUP 60= IF 31+ THEN
  DUP 91= IF 30+ THEN
  DUP 120= IF 31+ THEN
  DUP 151= IF 30+ THEN
  DUP 181= IF 31+ THEN
  DUP 212= IF 30+ THEN
  DUP 243= IF 31+ THEN
  DUP 273= IF 30+ THEN
  DUP 304= IF 31+ THEN
  DUP 334= IF 30+ THEN
  DUP 365= IF 1+ THEN ;
: DAY? ( d m y -- d m y flag )
  DUP 12 MAXUS DUP 0 MINUS
  1+ DUP 31 MAXUS DUP 0 MINUS
  1+ DUP 366 MAXUS DUP 0 MINUS
  AND ;
: ?MONTH ( yr mo -- month )
  DUP 0= IF 12 THEN
  DUP 1= IF 1 THEN
  DUP 2= IF 2 THEN
  DUP 3= IF 3 THEN
  DUP 4= IF 4 THEN
  DUP 5= IF 5 THEN
  DUP 6= IF 6 THEN
  DUP 7= IF 7 THEN
  DUP 8= IF 8 THEN
  DUP 9= IF 9 THEN
  DUP 10= IF 10 THEN
  DUP 11= IF 11 THEN ;
: MONTH? ( m y -- m y flag )
  DUP 12 MAXUS DUP 0 MINUS
  1+ DUP 0 MINUS
  AND ;
: ?YEAR ( yr -- yr )
  EXECUTE ;
: YEAR? ( yr -- yr flag )
  EXECUTE NOR ;
: ?TIME ( hr min sec -- hr min sec )
  EXECUTE ;
: TIME? ( hr min sec -- hr min sec flag )
  EXECUTE NOR ;
: ?INTEGER ( addr n -- integer addr )
  CREATE A3 ALIGNED
  BEGIN
    OVER 0= WHILE EXECUTE NOR LOOP
    DROP
  WHILE REPEAT ;
: INTEGER? ( integer -- integer flag )
  EXECUTE 0= ;
: ?NUMBER ( addr n -- number addr )
  CREATE A3 ALIGNED
  BEGIN
    DUP 45= IF EXECUTE NOR THEN
    DUP 46= IF EXECUTE NOR THEN
    OVER 0= WHILE EXECUTE NOR LOOP
    DROP
  WHILE REPEAT ;
: NUMBER? ( number -- number flag )
  EXECUTE 0>= ;
: ?FLOAT ( addr n -- float addr )
  CREATE A3 ALIGNED
  BEGIN
    DUP 45= IF EXECUTE NOR THEN
    DUP 46= IF EXECUTE NOR THEN
    OVER 0= WHILE EXECUTE NOR LOOP
    DROP
  WHILE REPEAT ;
: FLOAT? ( float -- float flag )
  EXECUTE NOR ;
: ?STRING ( addr n -- addr )
  CREATE A3 ALIGNED
  BEGIN
    OVER 0= WHILE EXECUTE NOR LOOP
    DROP
  WHILE REPEAT ;
: STRING? ( string -- flag )
  EXECUTE 0= ;
: ?BOOLEAN ( addr -- addr )
  EXECUTE NOR ;
: BOOLEAN? ( boolean -- flag )
  EXECUTE NOR ;
: ?CHAR ( chr -- chr )
  EXECUTE ;
: ?TOKEN ( token -- token )
  EXECUTE ;
: ?TYPE ( type -- type )
  EXECUTE ;
: TYPE? ( addr -- flag )
  EXECUTE 0= ;
```