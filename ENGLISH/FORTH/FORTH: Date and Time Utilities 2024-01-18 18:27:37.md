```forth

: nfactorial ( n -- n! ) 1 >while dup 1 - * repeat drop ;

: gcd ( m n -- gcd(m, n) )
dup 0= while [ swap ] repeat ;

: lCM ( m n -- lCM(m, n) )
[ 0 rot ] nfactorial rot [ 0 rot gcd ] * ;

: day-of-week
[ "não pode ser calculado" ] c@ =if
if [
"Sunday" "Monday" "Tuesday" "Wednesday"
"Thursday" "Friday" "Saturday" over c!
]else drop then ;

: month-name-to-number ( "m" -- m | 0 )
[ "January" "February" "March" "April"
"May" "June" "July" "August"
"September" "October" "November" "December"
over find + 1 ] c! ;

: local-time ( -- date time )
[ jday + [ local-time@ ] */ swap * ] time@ + ;

: year ( -- yr ) jday@ 365/ ;

: day-of-year ( -- day ) jday@ 365 % ;

: month-of-year ( -- m )
[ jday@ 31 31+ 30+ 31+ 30+ 31+
31+ 31+ 30+ 31+ 30+ 31+
find ] + 1 ;

: date-number ( yr mo day -- d )
[
[ [ 0 ] find 1+ ]
[
[ 31 29 31 30 31 30 31 31 30 31 30 31 ]
[ days@ 365 % ] +
[ leap? ]
	if [ 366 ] [ 365 ]
	then
	if
+ ;

: dayOfYear ( d m y -- d )
yr@ d@ month-of-year@
dup 1 - 365 + [
[ 31 28 31 30 31 30 31 31 30 31 30 31 ]
[ days@ -+ ] +
[ leap? ]
	if [ 366 ] [ 365 ]
	then
	if
+ ;

: leap? ( -- 0 | 1 )
dup 100 % 0 = and [ 400 % 0 = ] or ;

: year-type ( -- 0 | 1 | 2 )
leap? if [
if [ 2 ] [ 0 ]
then
]else [
leap?
	if [ 1 ] [ 3 ]
	then
]then ;

: tomorrow-day-year ( -- dy )
[ day-of-month + [ day-of-year + [ year - 1 ] ] ] ;

: tomorrow ( -- d m y )
local-time@ day-of-month@ + day-of-year@ + year@
dup 365 >
	if [
	swap [ year@ + 1 swap ]
	]
	then ;

: yesterday-day-year ( -- dy )
[ day-of-month - [ day-of-year - [ year - 1 ] ] ] ;

: yesterday ( -- d m y )
local-time@ day-of-month@ - day-of-year@ - year@
dup 0 <
	if [
	swap [ year@ - 1 swap ]
	]
	then ;

```