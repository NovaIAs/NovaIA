```groovy
//The program checks if a given string has any repeated characters.
//A large array of words is used for the verification.

import groovy.transform.TailRecursive

//Create a method to check if a string has any repeated characters.
@TailRecursive
boolean hasRepeatedChars(String str, Set<Character> seenChars) {
  //Empty strings have no repeated characters.
  if (str.isEmpty()) {
    return false
  }
  //If the first character is already in the set of seen characters, then there is a repeated character.
  else if (seenChars.contains(str[0])) {
    return true
  }
  //Otherwise, add the first character to the set of seen characters and recursively check the rest of the string.
  else {
    seenChars.add(str[0])
    return hasRepeatedChars(str[1..-1], seenChars)
  }
}

//Create a method to generate a list of all the words in a given file.
List<String> getWordsFromFile(String filename) {
  //Read the file into a string.
  String fileContents = new File(filename).text
  //Split the string into words.
  fileContents.split("\\W+")
}

//Create a list of all the words in the given file.
List<String> words = getWordsFromFile("words.txt")

//Create a set to store the words that have repeated characters.
Set<String> wordsWithRepeatedChars = new HashSet<>()

//Iterate over the words.
for (String word in words) {
  //Check if the word has any repeated characters.
  if (hasRepeatedChars(word, new HashSet<>())) {
    //If the word has repeated characters, add it to the set of words with repeated characters.
    wordsWithRepeatedChars.add(word)
  }
}

//Print the words that have repeated characters.
println("Words with repeated characters:")
for (String word in wordsWithRepeatedChars) {
  println(word)
}
```

Explanation:

The code starts by defining a method called `hasRepeatedChars` that takes a string and a set of characters as parameters. The method checks if the string has any repeated characters. It does this by iterating over the string and checking if each character is already in the set of seen characters. If a character is already in the set, then it means that it has been seen before, and therefore the string has repeated characters. Otherwise, the character is added to the set and the method recursively checks the rest of the string.

The code then defines a method called `getWordsFromFile` that takes a filename as a parameter. The method reads the file into a string and then splits the string into words. The words are returned as a list.

The code then uses the `getWordsFromFile` method to create a list of all the words in the "words.txt" file.

The code then defines a set called `wordsWithRepeatedChars`. This set will be used to store the words that have repeated characters.

The code then iterates over the list of words. For each word, it calls the `hasRepeatedChars` method to check if the word has any repeated characters. If the word has repeated characters, it is added to the set of words with repeated characters.

Finally, the code prints the words that have repeated characters.