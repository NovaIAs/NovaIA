**Get and replicate input string in reverse order**

```forth
: revdup ( -- string# string# )
    begin
        tuck swap string-size
        dup 0 do
            i 1 - swap tuck over c@ swap 1 - swap c!
        loop
        tuck swap string-size
    again
    drop ;  \ drop spare string-size
```

**Draw a line from Point A to Point B (16-color)**

```forth
: line16 ( x1 y1 x2 y2 col )
    2dup x1 y1 x2 y2 - abs >r >r
    tuck swap x1 y1 x2 y2 <r <r
    if 3drop exit then
    x1 x2 - 2dup x2 > if
        tuck over swap
        2swap swap
    else
        x1 x2 - 2dup y2 > if
            drop tuck swap
            over swap 2swap
            3dup 2swap 2drop
        else
            drop drop
        then
    then
    dup <# #> abs 2dup ># abs <
    if               \ horizontal
        2dup swap 4- swap % #>r 1 -
        over >r 0 max #>r
        begin
            dup >r 0 < while
                100 /swap dup * 2dup x2 < over y1 > and
                if width 4 cells 6 chars #> exit then
                else dup cells @ 0 do
                    dup swap cells < while
                        dup 1+ swap cells over c!
                    loop
                loop
                dup 1+ width !
            repeat
        drop
    else            \ vertical
        2dup y2 < if swap then
        2dup swap 4- swap % #>r 1 -
        over >r 0 max #>r
        begin
            dup >r 0 < while
                100 /swap dup * 2dup y2 < over x1 > and
                if width 4 cells 6 chars #> exit then
                else dup cells @ 0 do
                    dup swap cells < while
                        dup 1+ swap cells over c!
                    loop
                loop
                dup 1+ height !
            repeat
        drop
    then
    #>> ;
```

**Cellular automata pseudorandom number generator**

```forth
: prng ( -- n )
    random 0 swamp 1 swap
    begin
        dup 1 << <# #do
            tuck c@ 0x400 < or
            <# 1 - 8 - 1 lshift 0x171 XOR
            <# 1 - 8 - 1 rshift 0x171 XOR
        loop
        drop
    repeat
    #>> ;
```

**Generate a spiral matrix**

```forth
: spiral ( N N -- matrix )
    begin
        2dup > ( \ generate matrix
        2dup 1 - tuck 1 -
        1 do
            tuck begin
                0 do
                    tuck dup c@ dup 1-
                    tuck dup 1+ -1 c@
                    tuck dup c@ 1+
                    tuck dup 1+ c@ -1
                loop
            while
            tuck 2dup 1 - dup 1+ swap 2dup 1+ dup 1+ swap
            loop
            i
        loop
    ) 2drop ;
```