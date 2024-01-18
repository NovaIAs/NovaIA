```modula-2
MODULE Main;
IMPORT Strings;

TYPE alphabet = ARRAY 0..25 OF CHAR;
VAR  alphabet_original: alphabet;
     vowels: ARRAY[CHAR] OF CARDINAL;     -- count for  each vowel

PROCEDURE InitArray(VAR a: ARRAY a OF CHAR);
-- to initialize an array a to a specific character a
BEGIN
  FOR i := LOW(a) TO HIGH(a) DO a[i] := a END
END InitArray;

PROCEDURE ShowAlphabet(a: ARRAY a OF CHAR);
-- to display on the console the characters in a
BEGIN
  FOR i := LOW(a) TO HIGH(a) - 1 DO Write(a[i], ".");
END; Writeln(a[HIGH(a)])
END ShowAlphabet;

PROCEDURE ToLower(VAR a: ARRAY a OF CHAR);
-- to convert a to lower case
BEGIN
  FOR i := LOW(a) TO HIGH(a) DO
    IF (a[i] >= 'A') AND (a[i] <= 'Z') THEN a[i] := a[i] + 32
    END
  END
END ToLower;

PROCEDURE CountVowels(a: ARRAY a OF CHAR);
-- to count the occurrences of each vowel in a
VAR c: CARDINAL;
BEGIN
  Strings.Clear(vowels);  -- vowels is set to all 0
  Strings.ToLower(a);
  FOR i := LOW(a) TO HIGH(a) DO
    IF (a[i] = 'a') OR (a[i] = 'e') OR (a[i] = 'i') OR (a[i] = 'o') OR (a[i] = 'u') THEN
      c := Ord(a[i]) - Ord('a');
      Inc(vowels[c]);
    END
  END
END CountVowels;

PROCEDURE ShowVowels(v: ARRAY[CHAR] OF CARDINAL);
BEGIN
  FOR i := LOW(v) TO HIGH(v) DO Writeln("Vowel ", Chr(i), ": ", v[i]);
END ShowVowels;

PROCEDURE TestAlphabet;
VAR test_string: ARRAY[0..25] OF CHAR;
BEGIN
  InitArray(alphabet_original, 'a');
  ShowAlphabet(alphabet_original);
  WriteLine;
  ToLower(alphabet_original);
  ShowAlphabet(alphabet_original);
  WriteLine;
  Strings.Assign(test_string, "##Hello WoRlD 123");
  ShowAlphabet(test_string);
  WriteLine;
  CountVowels(test_string);
  ShowVowels(vowels);
END TestAlphabet;

BEGIN
  TestAlphabet
END Main.
```
Explanation:

- `MODULE Main;`: This line starts the main module of the program.

- `IMPORT Strings;`: This line imports the Strings module, which provides various string-related functions.

- `TYPE alphabet = ARRAY 0..25 OF CHAR;`: This line defines a custom data type called alphabet, which is an array of 26 characters.

- `VAR alphabet_original: alphabet;`: This line declares a variable called alphabet_original, which is of type alphabet.

- `vowels: ARRAY[CHAR] OF CARDINAL;`: This line declares an array called vowels, which can store counts of each vowel.

- `PROCEDURE InitArray(VAR a: ARRAY a OF CHAR);`: This procedure initializes an array a to a specific character a.

- `PROCEDURE ShowAlphabet(a: ARRAY a OF CHAR);`: This procedure displays the characters in an array a on the console.

- `PROCEDURE ToLower(VAR a: ARRAY a OF CHAR);`: This procedure converts all the characters in an array a to lowercase.

- `PROCEDURE CountVowels(a: ARRAY a OF CHAR);`: This procedure counts the occurrences of each vowel in an array a.

- `PROCEDURE ShowVowels(v: ARRAY[CHAR] OF CARDINAL);`: This procedure displays the counts of each vowel in an array v on the console.

- `PROCEDURE TestAlphabet;`: This procedure is the main part of the program, where various tests are performed on the alphabet_original array.

- `BEGIN TestAlphabet END Main;`: This line starts and ends the execution of the Main module.

In the main part of the program, the alphabet_original array is initialized with all lowercase letters, then displayed on the console. It is then converted to lowercase and displayed again.

Next, a test string "##Hello WoRlD 123" is assigned to another array called test_string, and the characters in this array are displayed.

After that, the CountVowels procedure is called to count the occurrences of each vowel in the test_string array. Finally, the ShowVowels procedure is called to display the counts of each vowel.