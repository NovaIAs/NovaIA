```dart
import 'dart:io';
import 'dart:async';
import 'dart:convert';

class ComplexDartCode {
  // This function takes a list of integers and returns the sum of all the integers in the list.
  int sumOfIntegers(List<int> integers) {
    int sum = 0;
    for (int integer in integers) {
      sum += integer;
    }
    return sum;
  }

  // This function takes a list of strings and returns a new list containing only the strings that are longer than 10 characters.
  List<String> filterLongStrings(List<String> strings) {
    return strings.where((string) => string.length > 10).toList();
  }

  // This function takes a string and returns a new string with all the vowels removed.
  String removeVowels(String string) {
    return string.replaceAll(RegExp(r'[aeiouAEIOU]'), '');
  }

  // This function takes a file path and returns a list of all the lines in the file.
  Future<List<String>> readFileLines(String filePath) async {
    File file = File(filePath);
    return file.readAsLines();
  }

  // This function takes a list of strings and writes them to a file.
  Future<void> writeFileLines(String filePath, List<String> lines) async {
    File file = File(filePath);
    return file.writeAsString(lines.join('\n'));
  }

  // This function takes a JSON string and returns a Map object.
  Map<String, dynamic> parseJson(String json) {
    return jsonDecode(json);
  }

  // This function takes a Map object and returns a JSON string.
  String toJson(Map<String, dynamic> map) {
    return jsonEncode(map);
  }

  // This function takes a list of integers and returns a new list containing only the even integers.
  List<int> filterEvenIntegers(List<int> integers) {
    return integers.where((integer) => integer % 2 == 0).toList();
  }

  // This function takes a list of strings and returns a new list containing only the strings that start with a vowel.
  List<String> filterStringsStartingWithVowel(List<String> strings) {
    return strings.where((string) => RegExp(r'^[aeiouAEIOU]').hasMatch(string)).toList();
  }

  // This function takes a list of strings and returns a new list containing only the strings that end with a vowel.
  List<String> filterStringsEndingWithVowel(List<String> strings) {
    return strings.where((string) => RegExp(r'[aeiouAEIOU]$').hasMatch(string)).toList();
  }

  // This function takes a string and returns a new string with the first letter capitalized.
  String capitalizeFirstLetter(String string) {
    return string[0].toUpperCase() + string.substring(1);
  }

  // This function takes a string and returns a new string with the last letter capitalized.
  String capitalizeLastLetter(String string) {
    return string.substring(0, string.length - 1) + string[string.length - 1].toUpperCase();
  }

  // This function takes a string and returns a new string with all the letters reversed.
  String reverseString(String string) {
    return string.split('').reversed.join('');
  }

  // This function takes a string and returns a new string with all the words reversed.
  String reverseWords(String string) {
    return string.split(' ').reversed.join(' ');
  }

  // This function takes a string and returns a new string with all the characters in alphabetical order.
  String sortCharacters(String string) {
    return string.split('').sorted((a, b) => a.compareTo(b)).join('');
  }

  // This function takes a string and returns a new string with all the words in alphabetical order.
  String sortWords(String string) {
    return string.split(' ').sorted((a, b) => a.compareTo(b)).join(' ');
  }

  // This function takes a string and returns a new string with all the lines in alphabetical order.
  String sortLines(String string) {
    return string.split('\n').sorted((a, b) => a.compareTo(b)).join('\n');
  }

  // This function takes a list of strings and returns a new list containing only the unique strings.
  List<String> uniqueStrings(List<String> strings) {
    return strings.toSet().toList();
  }

  // This function takes a list of integers and returns a new list containing only the distinct integers.
  List<int> distinctIntegers(List<int> integers) {
    return integers.toSet().toList();
  }

  // This function takes a list of strings and returns a new list containing only the strings that are palindromes.
  List<String> filterPalindromes(List<String> strings) {
    return strings.where((string) => string == string.split('').reversed.join('')).toList();
  }

  // This function takes a list of integers and returns a new list containing only the prime integers.
  List<int> filterPrimeIntegers(List<int> integers) {
    return integers.where((integer) => isPrime(integer)).toList();
  }

  // This function takes an integer and returns true if the integer is prime, and false otherwise.
  bool isPrime(int integer) {
    if (integer <= 1) {
      return false;
    }
    for (int i = 2; i <= integer / 2; i++) {
      if (integer % i == 0) {
        return false;
      }
    }
    return true;
  }
}
```

This code is a collection of various functions that perform different operations on different types of data. Here is a brief explanation of each function:

* `sumOfIntegers`: This function takes a list of integers and returns the sum of all the integers in the list.
* `filterLongStrings`: This function takes a list of strings and returns a new list containing only the strings that are longer than 10 characters.
* `removeVowels`: This function takes a string and returns a new string with all the vowels removed.
* `readFileLines`: This function takes a file path and returns a list of all the lines in the file.
* `writeFileLines`: This function takes a file path and a list of strings, and writes the strings to the file.
* `parseJson`: This function takes a JSON string and returns a Map object.
* `toJson`: This function takes a Map object and returns a JSON string.
* `filterEvenIntegers`: This function takes a list of integers and returns a new list containing only the even integers.
* `filterStringsStartingWithVowel`: This function takes a list of strings and returns a new list containing only the strings that start with a vowel.
* `filterStringsEndingWithVowel`: This function takes a list of strings and returns a new list containing only the strings that end with a vowel.
* `capitalizeFirstLetter`: This function takes a string and returns a new string with the first letter capitalized.
* `capitalizeLastLetter`: This function takes a string and returns a new string with the last letter capitalized.
* `reverseString`: This function takes a string and returns a new string with all the letters reversed.
* `reverseWords`: This function takes a string and returns a new string with all the words reversed.
* `sortCharacters`: This function takes a string and returns a new string with all the characters in alphabetical order.
* `sortWords`: This function takes a string and returns a new string with all the words in alphabetical order.
* `sortLines`: This function takes a string and returns a new string with all the lines in alphabetical order.
* `uniqueStrings`: This function takes a list of strings and returns a new list containing only the unique strings.
* `distinctIntegers`: This function takes a list of integers and returns a new list containing only the distinct integers.
* `filterPalindromes`: This function takes a list of strings and returns a new list containing only the strings that are palindromes.
* `filterPrimeIntegers`: This function takes a list of integers and returns a new list containing only the prime integers.
* `isPrime`: This function takes an integer and returns true if the integer is prime, and false otherwise.