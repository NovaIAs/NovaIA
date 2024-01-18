```forth
: fib ( n -- n-th Fibonacci number )
        0 swap
        begin
          while [ < r@ ]
            tuck * + dup @ r>
          repeat
          drop r@
        end ;

: fact ( n -- n! )
        0 swap 1
        begin
          while [ < r@ ]
            tuck * dup @ r>
          repeat
          drop r@
        end ;

: prime? ( n -- flag )
        2 > swap
        if
          dup 2 swap mod 0=
        else
          2dup r@ [ < ] while
            [ swap >r ] swap [ swap r@ ] while
            [ swap >r ] swap [ - ] [ * ] dip [ drop ]
            if
          repeat
        then ;

: primes ( n -- number list )
        0 0
        do
          [ prime? ] while
            [ dup 1 + ] [ r> ] swap
          loop
          drop r@
        loop ;

: ack ( m n -- result )
        m 1 <= if
          m 1 - ack n *
        else
          n 1 <= if
            n 1 - ack 1 +
          else
            m n ack + ack
          then
        then ;

: hanoi ( n from to -- )
        0 swap >r if
          dup 1 - hanoi from 3 - dup 3 - hanoi to 1 +
          "move " from " to " to . cr
          1 - hanoi to 3 - from 1 +
        then r> drop ;

: print-factors ( n -- )
        0 dup 2 = if
          2 cr
          dup n / 2 cr
        else
          dup 2 > if
            2 - begin
              [ 2 ] 2swap while
              dup n / mod 0 if
                cr n / .
                1 - begin
                  2 - begin
                    [ 2 ] 2swap while
                    dup n / mod 0 if
                      cr n / .
                    repeat drop
                  dup 2 < while
                    [ 2 - ] 2swap
                  repeat drop
                while
                1 + 2dup n / >r if
                  nip
                  dup 2swap [ mod 0 ] while
                  drop
                  drop
                then
              repeat
              2swap
            end
          else
            cr n .
          then
        then ;

: collatz ( n -- )
        begin
          swap 2 mod 0= if
            2dup [ / 2 ] [ * 3 + 1 ] if
              cr dup r@
              dup 1 <> while
                dup 1 <= if
                  drop
                else : again
                  begin
                    [ again ] 2swap while
                  repeat [ drop 1 - ]
                  r> nip
                then
              repeat
            then
          else
            cr dup r@
            dup 1 <> while
              dup 8 <= if
                : again
                  begin
                    [ again ] 2swap while
                  repeat [ drop 4 - ]
                  r> nip
              else
                dup 32 <= if
                  : again
                    begin
                      [ again ] 2swap while
                    repeat [ drop 8 - ]
                    r> nip
                else
                  drop 1 +
                then
              then
            repeat drop
          then
        end ;

: hex2dec ( str -- dec )
        begin
          dup 0 = if
            drop 0
          else
            begin
              tuck 10 if
                [ drop 16 ]
              else
                [ dup 120 >= if
                  drop 10
                else
                  drop 1
                then ]
              then
              swap +
            end
          end
        end until ;

: base2dec ( str n -- dec )
        0 swap
        begin
          tuck n > if
            nip
          else
            begin
              tuck 2 if
                [ drop 2 ]
              else
                [ dup 9 >= if
                  drop 10
                else
                  drop 1
                then ]
              then
              swap +
            end
          end
        end until ;

: str2dec ( str -- dec )
        begin
          dup 1 = if nip
            [ drop 0 ]
          else
            dup < drop
            begin
              dup 0 = if drop else
                tuck hex2dec
              then
            forever
          then
        end ;

: dec2bin ( dec -- bin )
        0 swap
        begin
          dup 2 > if
            tuck 1 +
          then
          nip
        end until
        [ drop ]
        [ 0 pick ] while
        drop ;

: ms ( -- n )
        2dup < if nip else
          over 1000 *
          nip
        then ;

: us ( -- n )
        2dup < if nip else
          over 1000000 *
          nip
        then ;

\ List manipulation
: dupl ( x -- x x )
        2dup ;
: dropd ( x -- )
        drop ;
: rot ( a b c -- b c a )
        over over swap ;
: rswap ( a b -- b a )
        swap ;
\ Memory allocation with GC
: allot ( n -- addr )
        \ Request n cells; ptr should point to free cell
        \ After call, ptr points to 1st cell in block
        [ CELLS ] @
        cells2cell
        tuck dup + -
        \ Limit in cells
        or
        \ Limit in bytes. Result < CELLS
        <cell2cells if
           CELLS -
          swap @
        else drop
        then ;
: >r ( x -- )
        r> r@ + r> ! ;
: r> ( x -- )
        r> @ + r> ! ;
: r@ ( -- x )
        @ r> + r@ ;
: 2r@ ( -- x )
        2 @ r> + 2 @ ;
: 2>r ( x -- )
        2dup < if
          2 >r nip
        else
          2tuck 2swap + 2 >r
        then ;
: r2> ( x -- )
        2tuck 2swap + r2> ;
: cell+ ( addr n -- addr )
        tuck swap + ;
: cell- ( addr n -- addr )
        tuck swap - ;
: cell2cells ( addr -- int )
        tuck [ CELLS ] @ if
          or
        else \ out of bounds - return 0
          drop 0
        then ;
: cells2cell ( n -- addr )
        over [ CELLS ] @ if
          swap cells -> int
          tuck or
        else \ out of bounds - return 0
          drop 0
        then ;
\ Input & Output
: space ( -- c )
        32 ;
: type ( addr -- )
        begin
          dup 0 = if
            drop exit
          then
          2dup @ <> if
            swap @ .
            1 + recurse
          then
          drop
        end ;
: line ( addr -- )
        begin
          dup 0 = if
            drop exit
          then
          2dup @ <> if
            swap @ .
          then
          drop cr
        end ;
: getc ( -- c )
        0 [ IN ] @ + @ ;
: getchar ( -- c )
        getc space ;
\ Control structures
: defv ( name -- )
        swap : ;
: defn ( name -- )
        swap allot over [ ] ! word ;
: defv2 ( name -- )
        2 allot [ dup ] word ;
: defn2 ( name -- )
        4 allot [ dup ! 2dup ! ] word ;
: defcon ( name -- )
        dup constant ;
: cell ( -- addr )
        allot 0 ! ;
: while ( addr -- )
        [ ] loop ;
: until ( addr -- )
        [ ] until ;
: begin ( -- )
        [ ] ;
: end ( -- )
        ] ;
: if ( addr addr -- )
        [ ] then ;
: then ( -- )
        ] ;
: repeat ( addr -- )
        [ ] ;
: recurse ( addr -- )
        tuck @ ;
: again ( addr -- )
        dup @ ;
: do ( addr -- )
        [ ] loop ;
: for ( addr addr addr -- )
        nip [ ] do ;
: fori ( addr addr addr -- )
        nip 1 + [ ] do ;
: reverse ( addr -- )
        [ ] 2swap while ;
: again ( addr -- )
        dup nip @ ;
\ Specialized data structures
\ environments: values & locals
: variables ( n -- )
        do [ 0 ] loop ;

: locals ( n -- addr )
        variables
        dup 2 - allot
        swap + ;

\ words: names & addresses
: make-word ( name -- addr )
        2 allot
        tuck tuck [ ] swap !
        swap [ CONSTANT ] swap ! ;

: make-value ( name val -- addr )
        make-word [ swap ] ;

: defname ( name -- name )
        [ make-word ] word ;

\ Find nth arg of a word
: find-nth ( n -- addr )
        [ CELLS ] @ swap 2dup < if
          tuck 2swap + swap 2 >r nip
        else
          2dup <> if
            nip 0
          else
            2 >r
            2drop 1 - recurse
          then
        then ;

\ Find pointer to nth arg of a word
: find-arg ( n -- addr )
        find-nth - ;

\ Find nth local of a word
: find-local ( n -- addr )
        0 2cells+ find-arg + ;

: def ( name -- addr )
        1 allot find-nth 2cells+ ! ;

: seek ( addr n -- addr )
        tuck swap 2cells+ + ;

: definition ( name -- addr )
        [ make-value ] word
        2 find-nth find-arg + ! ;
\ arrays
: make-array ( n -- addr )
        dup
        tuck over
        [ CELLS ] @
        tuck
        \ Limit in cells
        swap or
        \ Limit in bytes. Result < CELLS
        tuck <cell2cells
        if
           CELLS -
          swap @
        else drop
        then ;
: a@ ( addr index -- val )
        2dup
        cells2cell
        \ index out-of-bounds?
        tuck < if
          drop 0
        else
          swap [ CELLS ] @ or + @
        then ;
: a! ( addr index val -- )
        2dup
        cells2cell
        \ index out-of-bounds?
        tuck < if
          drop
        else
          swap [ CELLS ] @ or + !
        then ;

: sfind ( str -- addr )
        create 256 dictionary
        begin
          : again
            begin
              nip
              dup if
                1 + tuck recurse
                [' space ]
              else
                nip drop
              then
            repeat ;
          tuck str2dec
          dup 0 = if
            drop exit
          else
            find-word
            dup if
              tuck seek recurse
              drop
            else
              nip
              tuck @ 0 = if
                drop
              else
                again
              then
            then
          then
        end
        drop ;

: list-create ( n -- list )
        n 2 * make-array definition ;

: list-count ( list -- n )
        dup 0 2cells+ @ ;

: list-ref ( list index -- val )
        2dup 2cells+ a@ ;

: list-set ( list index val -- )
        2dup 2cells+ a! ;

: list-get ( list index -- )
        2dup 2cells+ a@ ;

: set ( list index val -- )
        2dup 2cells+ a! ;

: set2 ( list index val -- )
        2tuck 2cells+ a! ;

: list-insert ( list index val -- )
        dup 2cells+ @
        dup 2dup > if
          dup 0 > if
            tuck swap
            fori [ 1 - cell+ a@ cell+ a! ]
            drop
          then
          2 >r swap 2 cells+ !
        then ;

: list-delete ( list index -- val )
        dup 2cells+ @
        dup 2dup > if
          2dup 0 > if
            drop
            tuck swap
            fori [ 1 - cell+ a@ cell+ a! ]
          then
          2 >r swap 2cells+ !
        then ;

: list ( -- list )
        list-create definition ;

: cons ( a b -- cell )
        2 allot a ! swap b ! ;

: car ( cell -- a )
        @ ;

: cdr ( cell -- b )
        1 + @ ;

: list? ( x -- ? )
        dup 0 = if
          drop false
        else
          2dup @ [ ] if
            drop false
          else
            drop true
          then
        then ;

: list-print ( list -- )
        dup 0 = if
          drop
        else
          [ car ] type
          1 + recurse
          space
        then ;

: list-printl ( list -- )
        dup 0 = if
          drop
        else
          [ car ] type
          1 + recurse
        then
        cr ;

: dot-less ( x -- )
        tuck 0 = if
          drop
        else
          type .
        then ;

: list-p ( x -- ? )
        dup 0 = if
          drop false
        else
          2 >r 2dup @ @ [ ] if
            drop true
          else
            drop false
          then
        then ;

: list-append ( list1 list2 -- list )
        begin
          dup 0 = if
            drop list2
          else
            dup 2dup @ @ = if
              drop
            else
              dup 2dup @ dup @ = if
                drop
              else
                swap 2dup @
                [ cons car cdr list-append ] recurse
              then
            then
          then
        end ;

: list-and ( list -- ? )
        dup 0 = if
          drop false
        else
          [ car @ and ] recurse
        then ;

: list-or ( list -- ? )
        dup 0 = if
          drop false
        else
          [ car @ or ] recurse
        then ;

: list-not ( list -- ? )
        dup 0 = if
          drop true
        else
          [ car not ] recurse
        then ;

: list-xor ( list1 list2 -- ? )
        list-and
        list-or
        not ;

: list-map ( list expr -- list )
        begin
          dup 0 = if
            drop
          else
            2 >r swap 2dup @
            [ cons car [ expr ] recurse cdr list-append ] recurse
          then
        end ;
```