```forth

: aleatorio ( -- número )
    14272521 rand 0x7fffffff and dup ;

: cuadrado ( número -- número )
    dup * ;

: cubo ( número -- número )
    dup cuadrado * ;

: factorial ( número -- número )
    1 dup 0 do i swap -1 swap over * loop drop ;

: suma-cuadrados ( arreglo -- suma)
    0 do i @ cuadrado + loop drop ;

: media-cubos ( arreglo -- media )
    0 do i @ cubo + loop drop nip suma-cuadrados / ;

: combinaciones ( tamaño arreglo -- permutas )
    0 do d i dup do j dup
      begin j > if leave end then
      d @ + j @ swap @ > if j + 1 swap d 0 do 1 + loop nip d @ swap
        d 1 - swap begin d > if nip leave end then swap tuck loop leave
      end then
      j @ d @ + j @ swap d @ + loop swap drop loop drop nip ;

: triangulo-pascal ( nivel -- arreglo )
    { 0 - } alloc
    0 do i 1 swap do j 0 swap

      if i j 2 = if 0 else over @ over j 1 - @ + then else 0 then
      tuck swap + tuck

    loop drop
    swap drop ;

: fibonacci ( número -- número )
    dup 0 = if 1
           dup 1 = if 1
           else 2over - fibonacci 3over - fibonacci + then
           then ;

: fibonacci-array ( número -- arreglo )
    { 0 - } alloc
    0 do i fibonacci tuck i @ swap ! loop drop ;

: 0-1 ( -- arreglo )
    [ 0 1 ] ;

: permutacion ( número arreglo -- arreglo )
    dup 0 = if nip drop 0-1 exit then
    tuck copy

    0 do i
      begin
        i swap @ 1 - swap nip
        dup > if nip drop i tuck exit end then
      dup @ 1 + tuck swap enddo swap drop

    0 do i dup @ swap 1 - swap i @ swap dup
      while nip
        swap 1 - tuck swap tuck @ swap @ swap tuck tuck
        swap @ tuck tuck

      repeat drop nip

    array-length-to drop ;

```