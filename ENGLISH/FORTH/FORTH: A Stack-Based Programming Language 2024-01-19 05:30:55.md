```forth
: 1+ ( n -- n+1 )
    1 + ;
: 1- ( n -- n-1 )
    1 - ;
: 2* ( n -- 2n )
    2 * ;
: 2/ ( n -- n/2 )
    2 / ;
: Swap ( x y -- y x )
    Over Rot ;
: Drop ( n -- )
    Pop ;
: 2Swap ( x y z -- z x y )
    Over 2Dup Swap 2Drop ;
: Store ( n addr -- )
    ! ;
: Fetch ( addr -- n )
    @ ;
: C! ( char, addr -- )
    Cell+ ! ;
: C@ ( addr -- char )
    Cell+ @ ;
: , ( c -- )
    C! ;
: . ( n -- )
    . ;
: Space ( -- )
    32 C! ;
: String ( str -- )
    Begin
        Dup 0 C@ While
            Cell+ @ Emit
        Repeat
        Drop ;
: Word ( str -- )
    Begin
        Dup 0 C@ While
            Cell+ @ C@ <> While
                Cell+ @ Emit
            Repeat
            Drop
            Space
        Repeat
        Drop ;
: Do ( -- )
    Depth While
        Swap Execute
    Repeat
    Drop ;
: Loop ( -- )
    Begin
        Do
        Exit
    Again
    Drop ;
: Until ( -- )
    Begin
        Do
        [ Done ] If Exit
        Then
    Again
    Drop ;
: Find ( char, str -- addr )
    Begin
        [ StrCmp 0 <> ]
        While
            Cell+ @ C@ <> Do
                1+ Loop
            Again
            [ Addr @ ] If Exit Then
        Repeat
        [ Over -1 C@ 0 C@ = ] While
            1- Loop
            Rot
        Drop
        [ 0 Swap 2+ 1+ ] If Exit Then
    Again
    Drop ;
: Swap>R ( -- )
    Dup Rot ;
: R>Swap ( -- )
    2Swap Swap ;
: ? ( -- addr )
    0 Cell+ ;
: ! ( -- )
    @ ;
: Copy ( n1, n2 -- )
    Begin
        [ Over 0 = ]
        While
            [ 1+ Rot Fetch C! ]
            While
                1+ Loop
            1- Loop
        Drop
        Swap Drop ;
: #> ( str, n -- )
    Begin
        [ Over 0 = ]
        While
            [ 1+ Rot Cell+ C@ C! ]
            While
                1+ Loop
            1- Loop
        Drop
        Swap Drop ;
```