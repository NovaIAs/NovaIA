```forth
include string.f
include convert.f
include math.f
include unicode.f
include kernel.f
include context.f
include exec.f
include limits.f

: sizeof ( n -- addr )
tuck swap @ ;

: align ( n -- alloc )
tuck 1+ swap rshift 1+ >r swap -r >r swap r> ;

: bitmap-put ( data bitmap offs -- )
over @ 1 rshift
swap drop tuck tuck +
2drop 1<< or >b ;

: bitmap-clear ( data bitmap offs -- )
over @ 1 rshift
swap drop tuck tuck +
2drop 1<< and >b ;

: bitmap-get ( data bitmap offs -- bit )
over @ 1 rshift
swap drop tuck tuck +
2drop 1<< and ;

: bitmap-1? ( data bitmap offs -- bool )
bitmap-get 0= ;

: bitmap-0? ( data bitmap offs -- bool )
bitmap-get 0=not ;

: bitmap-set ( data bitmap offs -- )
bitmap-put 1 ;

: bitmap-clear ( data bitmap offs -- )
bitmap-put 0 ;

: bitmap-flip ( data bitmap offs -- )
bitmap-put bitmap-get xor ;

: bitmap-move ( src dst n -- )
0 do
  src tuck bitmap-get dst bitmap-put
  src 1+ dst 1+ n 1- loop
drop drop ;

: bitmap-fill ( data bitmap offs n -- )
0 do
  data dst bitmap-put
  dst 1+ n 1- loop
drop drop ;

: bitmap-search-for ( data pattern bitmap offs len -- newer-offs )
len 0=
if
  -1
else
  len 1- do
    data tuck pattern bitmap-put
    data len 1- offs + bitmap-search
  loop
drop ;

: bitmap-search ( data bitmap offs len -- newer-offs )
0 do
  data tuck bitmap-search-for offs len
  swap 0=while
    drop len + loop
  data 1+ offs 1+ len 1- loop
drop ;

: bitmap-copy ( src dst n -- )
bitmap-find-common-divisor n swap
if
  bitmap-copy-unaligned src dst n
else
  bitmap-copy-aligned src dst n
then ;

: bitmap-copy-aligned ( src dst n -- )
src sizeof align dst sizeof align <
if
  src sizeof align dst sizeof align while
    [ src tuck bitmap-move dst align @ + ]
  repeat
else
  dst sizeof align src sizeof align while
    [ dst tuck bitmap-move src align @ + ]
  repeat
then ;

: bitmap-copy-unaligned ( src dst n -- )
over c@ 1=
if
  dst 1+ src 1+ n 1- while
    [ dst @ dup 0<= while
        [ drop 0 ]
      repeat
    >b
    dst @ dup 1<= while
        [ drop 1<< ]
      repeat
    >b
    dst @ dup 7<= while
        [ drop 7<< ]
      repeat
    >b
    [ src @ ]
  repeat
  drop
else
  dst 1+ src 1+ n 1- while
    [ dst @ dup 0< while
        [ drop 0 ]
      repeat
    >b
    dst @ dup 1< while
        [ drop 1<< ]
      repeat
    >b
    dst @ dup 7< while
        [ drop 7<< ]
      repeat
    >b
    [ src @ ]
  repeat
  drop
then ;

: bitmap-and ( dst src -- dst )
swap 0 do
  tuck bitmap-get and >b
  1+ loop
drop ;

: bitmap-or ( dst src -- dst )
swap 0 do
  tuck bitmap-get or >b
  1+ loop
drop ;

: bitmap-xor ( dst src -- dst )
swap 0 do
  tuck bitmap-get xor >b
  1+ loop
drop ;

: bitmap-find-common-divisor ( n n-div -- n )
tuck n dup 128<
if
  4 dup 2dup 8<< 8>> 8/
  16 64 128 1/
else
  4 dup 2dup 8<< 8>> 8/
then ;

: bitmap-divisible? ( bigger smaller -- bool )
bigger smaller swap / exact ;

: bitmap-width ( width height -- width )
tuck drop ;

: bitmap-height ( width height -- height )
over drop ;

: bitmap-pixels ( width height -- pixels )
width height * ;

: bitmap-bits ( width height -- bits )
bitmap-pixels 1 shl ;

: bitmap-bytes ( width height -- bytes )
bitmap-bits 8 / ;

: bitmap-new ( width height -- bitmap )
bitmap-bytes width height * >r
bitmap-pixels tuck align >r
swap bitmap-pixels tuck >r
>r ;

: bitmap-free ( bitmap -- )
bitmap-bytes @ r> drop
r> drop
r> drop ;

: bitmap-get ( bitmap offs -- pixel )
tuck sizeof @ -
bitmap + bitmap-get >r ;

: bitmap-set ( bitmap offs pixel -- )
tuck sizeof @ -
bitmap + bitmap-set >r ;

: bitmap-clear ( bitmap offs -- )
tuck sizeof @ -
bitmap + bitmap-clear >r ;

: bitmap-flip ( bitmap offs -- )
tuck sizeof @ -
bitmap + bitmap-flip >r ;

: bitmap-resize ( bitmap width height -- new-bitmap )
dup drop bitmap-width bitmap-height bitmap-new swap
dup size
swap size

do
  over over dup dup
  0 do
    tuck bitmap-get rotate bitmap-set drop 1+ drop
  loop
  drop drop 1+ drop
loop

drop drop ;

: bitmap-shrink ( bitmap width height -- new-bitmap )
over over dup dup
bitmap-new swap
dup size
swap size

do
  over over dup dup
  0 do
    tuck bitmap-get swap bitmap-set drop 1+ drop
  loop
  drop drop 1+ drop
loop

drop drop ;

: bitmap-expand ( bitmap width height -- new-bitmap )
dup drop bitmap-width bitmap-height bitmap-new swap
dup size
swap size

do
  over over dup dup
  0 to
    tuck bitmap-get swap bitmap-set drop 1+ drop
  loop
  drop drop 1+ drop
loop

drop drop ;

: bitmap-scale ( bitmap width height -- new-bitmap )
over over dup dup
bitmap-new swap
dup size
swap size

do
  over over dup dup
  0 to
    tuck bitmap-get swap bitmap-set drop 1+ drop
  loop
  drop drop 1+ drop
loop

drop drop ;

: bitmap-rotate ( bitmap width height -- new-bitmap )
swap 0 do
  bitmap width bitmap-get dup
  width height - 1- drop bitmap-set
  1+ loop
drop
swap bitmap-width bitmap-height bitmap-new swap
dup size
swap size

do
  over over dup dup
  0 do
    tuck bitmap-get rotate bitmap-set drop 1+ drop
  loop
  drop drop 1+ drop
loop

drop drop ;

: bitmap-flip-x ( bitmap width height -- new-bitmap )
swap 0 do
  swap bitmap width bitmap-get dup
  width 1- drop bitmap-set
  1+ loop
drop
swap bitmap-width bitmap-height bitmap-new swap
dup size
swap size

do
  over over dup dup
  0 do
    tuck bitmap-get rotate bitmap-set drop 1+ drop
  loop
  drop drop 1+ drop
loop

drop drop ;

: bitmap-flip-y ( bitmap width height -- new-bitmap )
swap 0 do
  bitmap width bitmap-get dup
  height 1- drop bitmap-set
  1+ loop
drop
swap bitmap-width bitmap-height bitmap-new swap
dup size
swap size

do
  over over dup dup
  0 do
    tuck bitmap-get rotate bitmap-set drop 1+ drop
  loop
  drop drop 1+ drop
loop

drop drop ;

: bitmap-flip-h-and-v ( bitmap width height -- new-bitmap )
swap 0 do
  bitmap width bitmap-get dup
  height 1- width 1- drop bitmap-set
  1+ loop
drop
swap bitmap-width bitmap-height bitmap-new swap
dup size
swap size

do
  over over dup dup
  0 do
    tuck bitmap-get rotate bitmap-set drop 1+ drop
  loop
  drop drop 1+ drop
loop

drop drop ;

: bitmap-save-to-file ( filename bitmap width height -- )
filename bitmap-width bitmap-height bitmap-bits @ create
swap bitmap-pixels @
0 do
  dup swap @
  swap 1+ >
  1+ loop
drop ;

: bitmap-load-from-file ( filename -- bitmap )
filename file-read-all
dup 3- >>r
dup 3- >>r
swap over over 3 >>r
3dup bitmap-new swap
size

do
  over over dup dup
  0 do
    tuck @ swap bitmap-set drop 1+ drop
  loop
  drop drop 1+ drop
loop

r> drop r> drop r> drop ;

: rle-encode ( bitmap width height -- encoded )
bitmap-width bitmap-height bitmap-new swap
bitmap-pixels -
loop
  tuck bitmap-get 1=
  while
    tuck 1+
    swap >r
  repeat
  swap bitmap-get swap r>
  bitmap-set
  1+
loop
drop ;

: rle-decode ( encoded width height -- bitmap )
bitmap-new swap
bitmap-pixels -
loop
  encoded tuck bitmap-get =
  while
    encoded tuck bitmap-get .
    1+ drop swap >r
  repeat
  encoded bitmap-get swap r>
  bitmap-set
  1+
loop
drop ;

: bitmap-to-text ( bitmap width height -- text )
bitmap-width bitmap-height bitmap-new swap
bitmap-pixels -
loop
  tuck bitmap-get =
  cr
  1+
loop
drop ;

: bitmap-from-text ( text width height -- bitmap )
bitmap-new swap
bitmap-pixels -
loop
  tuck 1+
  over 2dup
  10 > if drop dup 13= while
  tuck 10 10 dup = while
    drop dup 32= while
  repeat
  dup dot >r
  r> swap bitmap-set
  1+ drop
loop
drop r> ;

: bitmap-to-png ( bitmap width height filename -- )
bitmap width height /
filename png-save ;

: bitmap-from-png ( filename -- bitmap )
filename png-load ;
```