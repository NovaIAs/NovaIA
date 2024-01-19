```
import 'dart:math';

/// Makes a Dart string from a sequence of bytes.
String bytesToString(List<int> bytes) =>
    String.fromCharCodes(bytes);

/// Computes a SHA-256 hash of the input data.
List<int> sha256(String input) =>
    sha256Bytes(input.codeUnits);

/// Computes a SHA-256 hash of the input data.
List<int> sha256Bytes(List<int> bytes) =>
    _hash("sha256", bytes);

/// Computes an MD5 hash of the input data.
List<int> md5(String input) =>
    md5Bytes(input.codeUnits);

/// Computes an MD5 hash of the input data.
List<int> md5Bytes(List<int> bytes) =>
    _hash("md5", bytes);

/// Computes a hash of the input data using the given algorithm.
List<int> _hash(String algorithm, List<int> bytes) {
  var digest = sha256Sink.startCommon(algorithm);
  digest.add(bytes);
  digest.close();
  return digest.hash;
}

/// Generates a random number.
num rand(num max) =>
    Random().nextDouble() * max;

/// Parses a JSON string.
Map<String, dynamic> jsonParse(String json) =>
    jsonDecode(json) as Map<String, dynamic>;

/// Converts a JSON object to a string.
String jsonStringify(Map<String, dynamic> json) =>
    jsonEncode(json);

/// Makes a base64 string from a sequence of bytes.
String base64Encode(List<int> bytes) =>
    base64.encode(bytes);

/// Decodes a base64 string into a sequence of bytes.
List<int> base64Decode(String base64) =>
    base64.decode(base64);

/// Transforms a list of numbers to a list of strings.
List<String> numListToStringList(List<num> numbers) =>
    numbers.map((num) => num.toString()).toList();

/// Transforms a list of strings to a list of numbers.
List<num> stringListToNumList(List<String> strings) =>
    strings.map((String) => num.parse(String)).toList();
```

This code provides a variety of useful functions for working with strings, numbers, and binary data in Dart. It includes functions for hashing data using SHA-256 and MD5, generating random numbers, parsing and stringifying JSON data, encoding and decoding base64 data, and converting between lists of numbers and lists of strings. This code can be used in a variety of applications, such as cryptography, data validation, and data conversion.