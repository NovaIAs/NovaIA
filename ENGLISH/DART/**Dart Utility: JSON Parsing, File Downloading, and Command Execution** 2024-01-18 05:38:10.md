```dart
import 'dart:convert';
import 'dart:io';

/// Parses a JSON file and prints the values of the specified key.
void parseJson(String filePath, String key) {
  // Read the JSON file.
  var file = File(filePath);
  var json = jsonDecode(file.readAsStringSync());

  // Get the value of the specified key.
  var value = json[key];

  // Print the value.
  print(value);
}

/// Downloads a file from a URL and saves it to the specified path.
void downloadFile(String url, String path) {
  // Create a HTTP client.
  var client = HttpClient();

  // Send a GET request to the URL.
  var request = await client.getUrl(Uri.parse(url));

  // Receive the response from the server.
  var response = await request.close();

  // Save the response body to the specified path.
  var file = File(path);
  var sink = file.openWrite();
  await response.pipe(sink);
}

/// Runs the specified command in the terminal.
void runCommand(String command) {
  // Create a process.
  var process = Process.run(command, [], runInShell: true);

  // Wait for the process to complete.
  await process.then((process) {
    // Print the process output.
    print(process.stdout);
    print(process.stderr);
  });
}

/// Main function.
void main() {
  // Parse a JSON file.
  parseJson('data.json', 'name');

  // Download a file.
  downloadFile('http://example.com/file.zip', 'file.zip');

  // Run a command.
  runCommand('ls -l');
}
```

Explanation:

The code first defines three functions:

* `parseJson()` takes a JSON file path and a key as arguments, and it prints the value of the specified key in the JSON file.
* `downloadFile()` takes a URL and a path as arguments, and it downloads the file from the URL and saves it to the specified path.
* `runCommand()` takes a command as an argument, and it runs the command in the terminal.

The main function then calls these functions to:

* Parse a JSON file named `data.json` and print the value of the `name` key.
* Download a file from the URL `http://example.com/file.zip` and save it to the file `file.zip`.
* Run the command `ls -l` in the terminal.

This code demonstrates how to perform various tasks in Dart, including JSON parsing, file downloading, and command execution.