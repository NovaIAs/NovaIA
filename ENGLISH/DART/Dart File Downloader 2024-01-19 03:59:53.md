```dart
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'package:path_provider/path_provider.dart';
import 'package:permission_handler/permission_handler.dart';

class FileDownloader extends StatefulWidget {
  @override
  _FileDownloaderState createState() => _FileDownloaderState();
}

class _FileDownloaderState extends State<FileDownloader> {
  final Completer<Directory> _downloadsDirectoryCompleter = Completer();
  String _downloadsDirectory;

  final http.Client _httpClient = http.Client();

  @override
  void initState() {
    super.initState();

    _requestPermissions();

    _downloadsDirectoryCompleter.future.then((directory) {
      _downloadsDirectory = directory.path;
    });
  }

  Future<void> _requestPermissions() async {
    Map<Permission, PermissionStatus> statuses = await [
      Permission.storage,
    ].request();

    final info = statuses[Permission.storage].toString();

    print(info);
  }

  void _downloadFile(String url, String fileName) async {
    final file = File('$_downloadsDirectory/$fileName');

    final request = await _httpClient.get(Uri.parse(url));
    final bytes = await request.bodyBytes;

    await file.writeAsBytes(bytes);
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Downloads'),
      ),
      body: Center(
        child: ElevatedButton(
          onPressed: () {
            _downloadFile(
                'https://www.example.com/file.pdf', 'downloaded_file.pdf');
          },
          child: Text('Download File'),
        ),
      ),
    );
  }
}
```

This code demonstrates a more complex example of file downloading in Dart. It includes:

1. **Permission Requesting**: It checks and requests the necessary storage permissions using the `permission_handler` plugin.

2. **File Path Preparation**: It sets up the directory where the downloaded file will be saved using the `path_provider` plugin.

3. **File Downloading**: It downloads a file from a given URL using the `http` plugin and saves it to the specified directory.

4. **Flutter UI**: It includes a simple UI with a button to trigger the file download.

Explanations:

1. **Permission Requesting**: It uses the `request()` method of the `Permission` class to check and request storage permission. The result is printed out using `print()`.

2. **File Path Preparation**: It creates a `Directory` object using the `getDownloadsDirectory()` method of the `path_provider` plugin and assigns its path to the `_downloadsDirectory` variable.

3. **File Downloading**: It uses the `get()` method of the `http.Client` to fetch the file from the provided URL. It then writes the response body (which contains the file data) to the specified file using `writeAsBytes()`.

4. **Flutter UI**: It includes a simple `ElevatedButton` that triggers the file download when clicked.