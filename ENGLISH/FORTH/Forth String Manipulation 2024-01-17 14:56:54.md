```forth
CREATE DICT 10000 CELLS

: DOCREATE ( string -- addr )
BEGIN
    DICT @ >R
    HERE DICT !
    R> ,
    AGAIN
;

: DOKILL ( addr -- )
BEGIN
    DICT @ OVER , DICT !
    SWAP DICT !
    DROP DROP
;

: DOCREATECHAR ( char string -- addr )
BEGIN
    DOCREATE
    2DUP . C!
    SWAP DROP
;

: DOCREATECHARLIST ( char -- )
BEGIN
    DUP DOCREATECHAR
    WHILE .NEXT R> , LOOP
    DROP
;

: DOLOOP ( count string -- )
BEGIN
    DUP WHILE DOCREATECHARLIST LOOP
    DROP
;

: DOCREATEWORD ( string -- addr )
BEGIN
    DOCREATECHAR DOLOOP
;

: SUM ( n1 n2 -- n3 ) 2DUP + ; IMMEDIATE

: SQUARE ( n -- n2 ) DUP * ; IMMEDIATE

: POWER ( x y -- )
BEGIN
    dup 0 <>
    WHILE
        dup
        1 -
        rot
        dup
        1 =
        until
    drop
    dup
    >R
    0 R>
    BEGIN
        2dup
        >R
        R@
        over
        {drop begin dup while 2dup * recurse then repeat} if
        drop
        swap
        R@
        dup
        1 =
        until
    drop
    R>
; IMMEDIATE

: CHAR? ( char -- flag )
DUP 0x20 IF TRUE ELSE FALSE THEN ; IMMEDIATE

: LOWER? ( char -- flag )
DUP 0x61 IF TRUE ELSE FALSE THEN ; IMMEDIATE

: UPPER? ( char -- flag )
DUP 0x41 IF TRUE ELSE FALSE THEN ; IMMEDIATE

: NUMBER? ( addr -- result )
BEGIN
    0
    CHAR?
    WHILE
        0 SWAP -
        10 <
        IF
            TRUE
            EXIT
        ELSE
            CHAR?
            LOOP
        THEN
    REPEAT
    FALSE
; IMMEDIATE

: DIGIT? ( char -- flag )
CHAR? AND 0x30 0x39 BETWEEN ; IMMEDIATE

: SPACE? ( c -- flag )
C@ 13 = OR 32 = ; IMMEDIATE

: STRING? ( addr -- flag )
BEGIN
    CHAR? NOT
    UNTIL
    [SPACE?] UNTIL
    DUP C@ 0 =
; IMMEDIATE
```