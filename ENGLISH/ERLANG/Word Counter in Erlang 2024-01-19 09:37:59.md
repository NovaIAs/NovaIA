```erlang

-module(counting_words).
-export([count_words/1]).

count_words(Text) ->
    Words = string:tokens(Text, " "),
    length(Words).

```

This is a basic Erlang program that counts the number of words in a given text string. Here's how it works:

1. The count_words/1 function takes one argument, Text, which is a string containing the text to be processed.

2. The string:tokens/2 function is used to tokenize the text string, dividing it into a list of words. The " " (space) argument specifies that the string should be split on whitespace characters.

3. The resulting list of words is stored in the Words variable.

4. The length/1 function is applied to the Words list, which returns the number of elements in the list (i.e., the number of words in the text).

5. The count_words/1 function returns this count as its result.

To use this function, you would call it with a text string as the argument, and it will return the number of words in that string. For example:

```erlang
1> count_words("Hello world, this is a test").
5
```

This would output the result 5, as there are five words in the given text string.

This code can be useful for a variety of text processing tasks, such as counting the number of words in a document, extracting keywords from a text, or performing other types of analysis on textual data.