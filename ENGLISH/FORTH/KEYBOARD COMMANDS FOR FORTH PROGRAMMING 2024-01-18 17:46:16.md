```
HEX
: FORTH-STRING spaces @ drop ;

DECIMAL
: PROMPT-LINE [ quit? ] WHILE [
. " :: " FORTH-STRING type cr
] REPEAT ;

DECIMAL
: PROMPT-FUNCTION [
. " :: " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-FILES-AND-DIRECTORY [
. " ?/ " FORTH-STRING type cr
] ;

DECIMAL
: DUMP-FORTH-HEX [
dup [ 0= ] WHILE [ . "- " ] REPEAT
dup [ 0= ] WHILE [ 2dup >r 0 IF 0 DO [ hex digit ] THEN . ] REPEAT
drop ;

DECIMAL
: DUMP-PRINTER-HEX [
dup [ 0= ] WHILE [ . "- " ] REPEAT
dup [ 0= ] WHILE [ 2dup >r 0 IF 0 DO [ hex digit ] THEN . ] REPEAT
. cr ;

DECIMAL
: PROMPT-DIRECTORY-AND-FILE-NAME [
. " :: " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-FILE-NAME [
. " :| " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-ASCII [
. " &#x" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " ; " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-NUMERIC [
. " <#" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " ; " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-ASCII-AND-NUMERIC [
. " &#x" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " <#" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " ; " FORTH-STRING type cr
] ;

DECIMAL
: DUMP-HEX-NUMERIC [
dup [ 0= ] WHILE [ . "- " ] REPEAT
dup [ 0= ] WHILE [ 2dup >r 0 IF 0 DO [ hex digit ] THEN . ] REPEAT
hexa
dup [ 0= ] WHILE [ . "- " ] REPEAT
dup [ 0= ] WHILE [ 2dup >r 0 IF 0 DO [ hex digit ] THEN . ] REPEAT
drop ;

DECIMAL
: DUMP-DEC-NUMERIC [
decimala
dup [ 0= ] WHILE [ . "- " ] REPEAT
dup [ 0= ] WHILE [ 2dup >r 0 IF 0 DO [ dec digit ] THEN . ] REPEAT
drop ;

DECIMAL
: DUMP-HEX-ASCI-NUMERIC [
dup [ 0= ] WHILE [ . "- " ] REPEAT
dup [ 0= ] WHILE [ 2dup >r 0 IF 0 DO [ hex digit ] THEN . ] REPEAT
. " "
hexa
dup [ 0= ] WHILE [ . "- " ] REPEAT
dup [ 0= ] WHILE [ 2dup >r 0 IF 0 DO [ hex digit ] THEN . ] REPEAT
. " "
decimala
dup [ 0= ] WHILE [ . "- " ] REPEAT
dup [ 0= ] WHILE [ 2dup >r 0 IF 0 DO [ dec digit ] THEN . ] REPEAT
drop ;

DECIMAL
: DUMP-FORTH-HEX-DEC [
dup [ 0= ] WHILE [ . "- " ] REPEAT
dup [ 0= ] WHILE [ 2dup >r 0 IF 0 DO [ hex digit ] THEN . ] REPEAT
drop . "  "
decimala
dup [ 0= ] WHILE [ . "- " ] REPEAT
dup [ 0= ] WHILE [ 2dup >r 0 IF 0 DO [ dec digit ] THEN . ] REPEAT
drop ;

DECIMAL
: PROMPT-HEX-NUMERIC [
. " <#" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " ; " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-HEX-ASCI-NUMERIC [
. " &#x" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " <#" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " ; " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-FORTH-HEX-DEC [
. " :: " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-HEX-FORTH-DEC [
. " <#" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " :: " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-HEX-DEC-FORTH [
. " <#" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " #" 2dup * 2swap 2dup >r [ 0 DO [ dec digit ] THEN ] THEN
. " :: " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-HEX-DEC-NUMERIC [
. " <#" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " #" 2dup * 2swap 2dup >r [ 0 DO [ dec digit ] THEN ] THEN
. " ; " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-DEC-HEX-NUMERIC [
. " #" 2dup * 2swap 2dup >r [ 0 DO [ dec digit ] THEN ] THEN
. " <#" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " ; " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-DEC-HEX-FORTH [
. " #" 2dup * 2swap 2dup >r [ 0 DO [ dec digit ] THEN ] THEN
. " <#" 2dup * 2swap 2dup >r [ 0 DO [ hex digit ] THEN ] THEN
. " :: " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-DEC-FORTH-HEX [
. " #" 2dup * 2swap 2dup >r [ 0 DO [ dec digit ] THEN ] THEN
. " :: " FORTH-STRING type cr
] ;

DECIMAL
: PROMPT-CASES [
. " ?? " FORTH-STRING type cr
] ;

DECIMAL
: BUILD-KEYBOARD-COMMANDS [
prompt-line
: @        [ @ ; ] build
: !        [ ! ; ] build
: <>       [ <> ; ] build
: >        [ > ; ] build
: <        [ < ; ] build
: >=       [ >= ; ] build
: <=       [ <= ; ] build
: +        [ + ; ] build
: -        [ - ; ] build
: *        [ * ; ] build
: /        [ / ; ] build
: MOD      [ MOD ; ] build
: &&       [ && ; ] build
: ~~       [ ~~ ; ] build
: >>       [ >> ; ] build
: <<       [ << ; ] build
: rot      [ rot ; ] build
: drop     [ drop ; ] build
: dup      [ dup ; ] build
: over     [ over ; ] build
: swap     [ swap ; ] build
: pick     [ pick ; ] build
: roll     [ roll ; ] build
: dip      [ dip ; ] build
: nip      [ nip ; ] build
: tuck     [ tuck ; ] build
: @r       [ @r ; ] build
: !r       [ !r ; ] build
: >r       [ >r ; ] build
: <r       [ <r ; ] build
: r>       [ r> ; ] build
: r<       [ r< ; ] build
: r@       [ r@ ; ] build
: ,        [ , ; ] build
: .        [ . ; ] build
: cr       [ cr ; ] build
: crlf     [ crlf ; ] build
: space    [ space ; ] build
: ,s      [ ,s ; ] build
: s>d      [ s>d ; ] build
: type     [ type ; ] build
: print    [ print ; ] build
: space    [ space ; ] build
: cr       [ cr ; ] build
: crlf     [ crlf ; ] build
: quit?    [ quit? ; ] build
: bye      [ bye ; ] build
: clear    [ clear ; ] build
: @cell    [ @cell ; ] build
: !cell    [ !cell ; ] build
: cells    [ cells ; ] build
: @char    [ @char ; ] build
: !char    [ !char ; ] build
: chars    [ chars ; ] build
: head     [ head ; ] build
: tail     [ tail ; ] build
: addr     [ addr ; ] build
: report   [ report ; ] build
: eval     [ eval ; ] build
: c@       [ c@ ; ] build
: c!       [ c! ; ] build
: s@       [ s@ ; ] build
: s!       [ s! ; ] build
: char     [ char ; ] build
: create   [ create ; ] build
: does>    [ does> ; ] build
: all      [ all ; ] build
: count    [ count ; ] build
: here     [ here ; ] build
: @here    [ @here ; ] build
: !here    [ !here ; ] build
: and      [ and ; ] build
: or       [ or ; ] build
: xor      [ xor ; ] build
: shiftl   [ shiftl ; ] build
: shiftr   [ shiftr ; ] build
: set!     [ set! ; ] build
: word      [ word ; ] build
: within   [ within ; ] build
: -literal [ -literal ; ] build
: literal  [ literal ; ] build
: string   [ string ; ] build
: "        [ " ; ] build
: '        [ ' ; ] build
: .s       [ .s ; ] build
: >r       [ >r ; ] build
: <r       [ <r ; ] build
: +loop    [ +loop ; ] build
: doloop   [ doloop ; ] build
: loop     [ loop ; ] build
: break    [ break ; ] build
: exit     [ exit ; ] build
: leave    [ leave ; ] build
: !s       [ !s ; ] build
: @s       [ @s ; ] build
: #       [ # ; ] build
: #       [ # ; ] build
: char>num [ char>num ; ] build
: num>char [ num>char ; ] build
: pad      [ pad ; ] build
: cut      [ cut ; ] build
: "mod     [ "mod ; ] build
: abs      [ abs ; ] build
: min      [ min ; ] build
: max      [ max ; ] build
: round    [ round ; ] build
: floor    [ floor ; ] build
: ceiling  [ ceiling ; ] build
: "over    [ "over ; ] build
: "drop    [ "drop ; ] build
: "swap    [ "swap ; ] build
: mod      [ mod ; ] build
: div      [ div ; ] build
: >mod     [ >mod ; ] build
: <mod     [ <mod ; ] build
: hex      [ hex ; ] build
: dec      [ dec ; ] build
: quot     [ quot ; ] build
: rem      [ rem ; ] build
: rshift   [ rshift ; ] build
: lshift   [ lshift ; ] build
: 0        [ 0 ; ] build
: 1        [ 1 ; ] build
: 2        [ 2 ; ] build
: 3        [ 3 ; ] build
: 4        [ 4 ; ] build
: 5        [ 5 ; ] build
: 6        [ 6 ; ] build
: 7        [ 7 ; ] build
: 8        [ 8 ; ] build
: 9        [ 9 ; ] build
: 10       [ 10 ; ] build
: 11       [ 11 ; ] build
: 12       [ 12 ; ] build
: 13       [ 13 ; ] build
: 14       [ 14 ; ] build
: 15       [ 15 ; ] build
: 16       [ 16 ; ] build
: 17       [ 17 ; ] build
: 18       [ 18 ; ] build
: 19       [ 19 ; ] build
: 20       [ 20 ; ] build
: 21       [ 21 ; ] build
: 22       [ 22 ; ] build
: 23       [ 23 ; ] build
: 24       [ 24 ; ] build
: 25       [ 25 ; ] build
: 26       [ 26 ; ] build
: 27       [ 27 ; ] build
: 28       [ 28 ; ] build
: 29       [ 29 ; ] build
: 30       [ 30 ; ] build
: 31       [ 31 ; ] build
: 32       [ 32 ; ] build
: 33       [ 33 ; ] build
: 34       [ 34 ; ] build
: 35       [ 35 ; ] build
: 36       [ 36 ; ] build
: 37       [ 37 ; ] build
: 38       [ 38 ; ] build
: 39       [ 39 ; ] build
: 40       [ 40 ; ] build
: 41       [ 41 ; ] build
: 42       [ 42 ; ] build
: 43       [ 43 ; ] build
: 44       [ 44 ; ] build
: 45       [ 45 ; ] build
: 46       [ 46 ; ] build
: 47       [ 47 ; ] build
: 48       [ 48 ; ] build
: 49       [ 49 ; ] build
: 50       [ 50 ; ] build
: 51       [ 51 ; ] build
: 52       [ 52 ; ] build
: 53       [ 53 ; ] build
: 54       [ 54 ; ] build
: 55       [ 55 ; ] build
: 56       [ 56 ; ] build
: 57       [ 57 ; ] build
: 58       [ 58 ; ] build
: 59       [ 59 ; ] build
: 60       [ 60 ; ] build
: 61       [ 61 ; ] build
: 62       [ 62 ; ] build
: 63       [ 63 ; ] build
: 64       [ 64 ; ] build
: 65       [ 65 ; ] build
: 66       [ 66 ; ] build
: 67       [ 67 ; ] build
: 68       [ 68 ; ] build
: 69       [ 69 ; ] build
: 70       [ 70 ; ] build
: 71       [ 71 ; ] build
: 72       [ 72 ; ] build
: 73       [ 73 ; ] build
: 74       [ 74 ; ] build
: 75       [ 75 ; ] build
: 76       [ 76 ; ] build
: 77       [ 77 ; ] build
: 78       [ 78 ; ] build
: 79       [ 79 ; ] build
: 80       [ 80 ; ] build
: 81       [ 81 ; ] build
: 82       [ 82 ; ] build
: 83       [ 83 ; ] build
: 84       [ 84 ; ] build
: 85       [ 85 ; ] build
: 86       [ 86 ; ] build
: 87       [ 87 ; ] build
: 88       [ 88 ; ] build
: 89       [ 89 ; ] build
: 90       [ 90 ; ] build
: 91       [ 91 ; ] build
: 92       [ 