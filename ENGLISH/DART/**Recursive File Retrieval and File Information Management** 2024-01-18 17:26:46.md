```dart
// Importing necessary libraries.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Creating a class to represent a file.
class FileInfo {
  // Instance variables.
  String filePath;
  int fileSize;
  DateTime fileLastModified;

  // Constructor.
  FileInfo(this.filePath, this.fileSize, this.fileLastModified);
}

// Creating a function to get files recursively from a directory.
Future<List<FileInfo>> getFilesRecursively(String directoryPath) async {
  // Creating a list to store the files.
  List<FileInfo> files = [];

  // Getting the files in the directory.
  List<FileSystemEntity> entities = await Directory(directoryPath).list().toList();

  // Iterating over the files.
  for (FileSystemEntity entity in entities) {
    // Checking if the entity is a file.
    if (FileSystemEntity.isFileSync(entity.path)) {
      // Adding the file to the list.
      files.add(FileInfo(entity.path, entity.statSync().size, entity.statSync().modified));
    } else if (FileSystemEntity.isDirectorySync(entity.path)) {
      // Recursively getting the files in the directory.
      files.addAll(await getFilesRecursively(entity.path));
    }
  }

  // Returning the list of files.
  return files;
}

// Creating a function to write the file information to a file.
void writeFileInformationToFile(List<FileInfo> files, String outputFilePath) async {
  // Creating a file.
  File outputFile = File(outputFilePath);

  // Opening the file for writing.
  IOSink outputFileSink = outputFile.openWrite();

  // Writing the file information to the file.
  for (FileInfo file in files) {
    outputFileSink.writeln('File path: ${file.filePath}');
    outputFileSink.writeln('File size: ${file.fileSize}');
    outputFileSink.writeln('File last modified: ${file.fileLastModified}');
    outputFileSink.writeln('');
  }

  // Closing the file.
  outputFileSink.close();
}

// Creating a function to send the file information to a server.
void sendFileInformationToServer(List<FileInfo> files) async {
  // Creating a HTTP client.
  HttpClient httpClient = HttpClient();

  // Creating a HTTP request.
  Uri requestUri = Uri.parse('http://example.com/file-information');
  HttpClientRequest request = await httpClient.postUrl(requestUri);

  // Setting the request headers.
  request.headers.contentType = ContentType.json;

  // Writing the file information to the request body.
  request.write(jsonEncode(files));

  // Sending the request.
  HttpClientResponse response = await request.close();

  // Checking the response status code.
  if (response.statusCode == HttpStatus.ok) {
    // The server received the file information successfully.
    print('File information sent to server successfully.');
  } else {
    // The server failed to receive the file information.
    print('Failed to send file information to server.');
  }

  // Closing the HTTP client.
  httpClient.close();
}

// Creating a main function.
void main() async {
  // Getting the files recursively from a directory.
  List<FileInfo> files = await getFilesRecursively('directory_path');

  // Writing the file information to a file.
  writeFileInformationToFile(files, 'output_file_path');

  // Sending the file information to a server.
  sendFileInformationToServer(files);
}
```

**Explanation**:

1. **Importing Libraries**: We import the necessary libraries, such as `dart:async`, `dart:convert`, and `dart:io`, which are used for asynchronous programming, JSON encoding/decoding, and file system operations respectively.

2. **Creating the `FileInfo` Class**: This class represents a file object, with properties like the file path, file size, and file last modified time.

3. **Recursive File Retrieval Function (`getFilesRecursively`)**: This function takes a directory path as its argument and returns a list of `FileInfo` objects for all the files in the directory and its subdirectories. It uses the `FileSystemEntity.list()` method to get the list of files and directories in the specified directory. If the entity is a file, it adds it to the list. If it's a directory, it recursively calls `getFilesRecursively` on that directory.

4. **Write File Information to File Function (`writeFileInformationToFile`)**: This function takes a list of `FileInfo` objects and an output file path as its arguments. It opens the output file for writing and writes the file information to the file, one file per line, in a human-readable format.

5. **Send File Information to Server Function (`sendFileInformationToServer`)**: This function takes a list of `FileInfo` objects as its argument. It creates a HTTP client, opens a HTTP POST request to a specified server endpoint, sets the request headers, writes the file information to the request body in JSON format, and sends the request. It checks the response status code to determine if the server received the file information successfully.

6. **Main Function**: In the `main` function, we call the `getFilesRecursively` function to get a list of files from a specified directory, call the `writeFileInformationToFile` function to write the file information to a file, and call the `sendFileInformationToServer` function to send the file information to a server.