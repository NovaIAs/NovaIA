```portugal
PROGRAM FIND_MAX_WORD_IN_SENTENCE;
VAR
  SENTENCE: STRING;
  MAX_WORD: STRING;
  MAX_WORD_LENGTH: INTEGER;
  WORD: STRING;
  WORD_LENGTH: INTEGER;
  I: INTEGER;
BEGIN
  WRITE("Enter a sentence: ");
  READLN(SENTENCE);

  MAX_WORD := "";
  MAX_WORD_LENGTH := 0;

  FOR I := 1 TO LENGTH(SENTENCE) DO
  BEGIN
    WORD := "";
    WORD_LENGTH := 0;

    WHILE (I <= LENGTH(SENTENCE)) AND (SENTENCE[I] <> ' ') DO
    BEGIN
      WORD := WORD + SENTENCE[I];
      WORD_LENGTH := WORD_LENGTH + 1;
      I := I + 1;
    END;

    IF WORD_LENGTH > MAX_WORD_LENGTH THEN
    BEGIN
      MAX_WORD := WORD;
      MAX_WORD_LENGTH := WORD_LENGTH;
    END;
  END;

  WRITELN("The longest word in the sentence is: ", MAX_WORD);
END.
```

Explanation:

1. We start by declaring the variables:
   - SENTENCE: A string to store the input sentence.
   - MAX_WORD: A string to store the longest word in the sentence.
   - MAX_WORD_LENGTH: An integer to store the length of the longest word.
   - WORD: A string to store the current word being processed.
   - WORD_LENGTH: An integer to store the length of the current word.
   - I: An integer used as a loop counter.

2. We prompt the user to enter a sentence using the WRITE statement and read it using the READLN statement.

3. We initialize MAX_WORD to an empty string and MAX_WORD_LENGTH to 0.

4. We use a FOR loop to iterate through each character in the SENTENCE string.

   - Inside the loop, we initialize WORD to an empty string and WORD_LENGTH to 0.

   - We use a WHILE loop to read characters from the SENTENCE string until we encounter a space ' ' or reach the end of the string.

     - We append the current character to the WORD string and increment the WORD_LENGTH.

     - We increment the loop counter I.

   - After the WHILE loop, we check if the length of the current word (WORD_LENGTH) is greater than the length of the longest word so far (MAX_WORD_LENGTH).

     - If it is, we update MAX_WORD to the current word and MAX_WORD_LENGTH to the current word length.

5. After processing all the characters in the SENTENCE, we display the longest word using the WRITELN statement.