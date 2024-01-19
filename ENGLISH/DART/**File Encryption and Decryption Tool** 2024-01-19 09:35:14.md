```dart
// Importing the necessary libraries.
import 'dart:convert';
import 'dart:io';
import 'package:args/command_runner.dart';
import 'package:args/args.dart';

// Creating a command runner object.
final CommandRunner commandRunner = CommandRunner('my_tool', 'My amazing tool!');

// Declaring the command runner's commands.
commandRunner.addCommand(EncryptCommand());
commandRunner.addCommand(DecryptCommand());

// The EncryptCommand class represents the 'encrypt' command.
class EncryptCommand extends Command {
  EncryptCommand() {
    argParser.addOption('input', abbr: 'i', help: 'The input file to encrypt.');
    argParser.addOption('output', abbr: 'o', help: 'The output file to write the encrypted data to.');
    argParser.addOption('key', abbr: 'k', help: 'The secret key to use for encryption.');
  }

  @override
  String get name => 'encrypt';

  @override
  String get description => 'Encrypts a file using the specified key.';

  @override
  void run() async {
    // Getting the command-line arguments.
    final String inputFile = argResults!['input'];
    final String outputFile = argResults!['output'];
    final String key = argResults!['key'];

    // Reading the input file.
    final File input = File(inputFile);
    final String inputText = await input.readAsString();

    // Encrypting the input text using the specified key.
    final String encryptedText = _encrypt(inputText, key);

    // Writing the encrypted text to the output file.
    final File output = File(outputFile);
    await output.writeAsString(encryptedText);

    // Printing a success message.
    print('Encryption successful!');
  }

  // The actual encryption function.
  String _encrypt(String inputText, String key) {
    // Converting the input text and key to bytes.
    final List<int> inputBytes = inputText.codeUnits;
    final List<int> keyBytes = key.codeUnits;

    // Creating an empty list to store the encrypted bytes.
    final List<int> encryptedBytes = <int>[];

    // Iterating over the input bytes and encrypting each byte using the key.
    for (int i = 0; i < inputBytes.length; i++) {
      encryptedBytes.add(inputBytes[i] ^ keyBytes[i % keyBytes.length]);
    }

    // Converting the encrypted bytes back to a string.
    return String.fromCharCodes(encryptedBytes);
  }
}

// The DecryptCommand class represents the 'decrypt' command.
class DecryptCommand extends Command {
  DecryptCommand() {
    argParser.addOption('input', abbr: 'i', help: 'The input file to decrypt.');
    argParser.addOption('output', abbr: 'o', help: 'The output file to write the decrypted data to.');
    argParser.addOption('key', abbr: 'k', help: 'The secret key used to encrypt the input file.');
  }

  @override
  String get name => 'decrypt';

  @override
  String get description => 'Decrypts a file using the specified key.';

  @override
  void run() async {
    // Getting the command-line arguments.
    final String inputFile = argResults!['input'];
    final String outputFile = argResults!['output'];
    final String key = argResults!['key'];

    // Reading the input file.
    final File input = File(inputFile);
    final String inputText = await input.readAsString();

    // Decrypting the input text using the specified key.
    final String decryptedText = _decrypt(inputText, key);

    // Writing the decrypted text to the output file.
    final File output = File(outputFile);
    await output.writeAsString(decryptedText);

    // Printing a success message.
    print('Decryption successful!');
  }

  // The actual decryption function.
  String _decrypt(String inputText, String key) {
    // Converting the input text and key to bytes.
    final List<int> inputBytes = inputText.codeUnits;
    final List<int> keyBytes = key.codeUnits;

    // Creating an empty list to store the decrypted bytes.
    final List<int> decryptedBytes = <int>[];

    // Iterating over the input bytes and decrypting each byte using the key.
    for (int i = 0; i < inputBytes.length; i++) {
      decryptedBytes.add(inputBytes[i] ^ keyBytes[i % keyBytes.length]);
    }

    // Converting the decrypted bytes back to a string.
    return String.fromCharCodes(decryptedBytes);
  }
}

// The main function is the entry point of the program.
void main(List<String> args) async {
  try {
    // Parsing the command-line arguments.
    await commandRunner.run(args);
  } on UsageException catch (e) {
    // Printing the usage information.
    print(e.usage);
  } catch (e) {
    // Printing the error message.
    print(e.toString());
  }
}

```

**Explanation of the code:**

1. We import the necessary libraries, including the `args` library for handling command-line arguments and the `convert` library for encoding and decoding data.

2. We create a `CommandRunner` object, which will be used to manage the different commands in our tool.

3. We define two command classes: `EncryptCommand` and `DecryptCommand`, which represent the 'encrypt' and 'decrypt' commands, respectively. These commands handle the encryption and decryption of files using a secret key.

4. In both command classes, we define the command's name, description, and arguments using the `argParser` property.

5. In the `run` method of each command class, we parse the command-line arguments, read the input file, perform the encryption or decryption operation, and write the result to the output file.

6. We also define the actual encryption and decryption functions (`_encrypt` and `_decrypt`) in each command class. These functions use a simple XOR operation to encrypt and decrypt the data using the specified secret key.

7. In the `main` function, we parse the command-line arguments using the `commandRunner` and execute the corresponding command. We handle any exceptions that may occur during this process.

This code provides a detailed and complex example of a Dart program that implements an encryption and decryption tool with command-line arguments and error handling.