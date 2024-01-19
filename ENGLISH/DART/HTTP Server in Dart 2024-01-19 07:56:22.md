```dart
import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

void main() {
  // Create a server socket on port 8080
  ServerSocket.bind(InternetAddress.anyIPv4, 8080).then((serverSocket) {
    // Listen for client connections
    serverSocket.listen((clientSocket) {
      // Read the HTTP request from the client
      clientSocket.transform(utf8.decoder).transform(LineSplitter()).listen((line) {
        // Parse the HTTP request
        var request = HttpRequest.fromString(line);

        // Create a HTTP response
        var response = HttpResponse();

        // Set the response status code
        response.statusCode = HttpStatus.ok;

        // Set the response content type
        response.headers.add('Content-Type', 'text/plain');

        // Write the response body
        response.write('Hello, world!');

        // Send the response to the client
        clientSocket.write(response.toString());

        // Close the client socket
        clientSocket.close();
      });
    });
  });
}

class HttpRequest {
  final String method;
  final String path;
  final String version;
  final Map<String, String> headers;
  final String body;

  HttpRequest.fromString(String requestString) {
    // Split the request string into its components
    var components = requestString.split('\r\n');

    // Parse the request line
    var requestLine = components[0];
    var requestLineParts = requestLine.split(' ');
    method = requestLineParts[0];
    path = requestLineParts[1];
    version = requestLineParts[2];

    // Parse the headers
    headers = {};
    for (var i = 1; i < components.length - 1; i++) {
      var headerLine = components[i];
      var headerLineParts = headerLine.split(': ');
      headers[headerLineParts[0]] = headerLineParts[1];
    }

    // Parse the body
    body = components[components.length - 1];
  }
}

class HttpResponse {
  int statusCode;
  Map<String, String> headers;
  String body;

  HttpResponse() {
    statusCode = HttpStatus.ok;
    headers = {};
    body = '';
  }

  void add(String key, String value) {
    headers[key] = value;
  }

  void write(String data) {
    body += data;
  }

  @override
  String toString() {
    // Create the status line
    var statusLine = 'HTTP/1.1 ${statusCode} ${HttpStatus.values[statusCode]}';

    // Create the header lines
    var headerLines = [];
    headers.forEach((key, value) {
      headerLines.add('${key}: ${value}');
    });

    // Create the response
    var response = [statusLine];
    response.addAll(headerLines);
    response.add('');
    response.add(body);
    return response.join('\r\n');
  }
}

class HttpStatus {
  static const int continue_ = 100;
  static const int switchingProtocols = 101;
  static const int processing = 102;
  static const int earlyHints = 103;
  static const int ok = 200;
  static const int created = 201;
  static const int accepted = 202;
  static const int nonAuthoritativeInformation = 203;
  static const int noContent = 204;
  static const int resetContent = 205;
  static const int partialContent = 206;
  static const int multiStatus = 207;
  static const int alreadyReported = 208;
  static const int imUsed = 226;
  static const int multipleChoices = 300;
  static const int movedPermanently = 301;
  static const int found = 302;
  static const int seeOther = 303;
  static const int notModified = 304;
  static const int useProxy = 305;
  static const int switchProxy = 306;
  static const int temporaryRedirect = 307;
  static const int permanentRedirect = 308;
  static const int badRequest = 400;
  static const int unauthorized = 401;
  static const int paymentRequired = 402;
  static const int forbidden = 403;
  static const int notFound = 404;
  static const int methodNotAllowed = 405;
  static const int notAcceptable = 406;
  static const int proxyAuthenticationRequired = 407;
  static const int requestTimeout = 408;
  static const int conflict = 409;
  static const int gone = 410;
  static const int lengthRequired = 411;
  static const int preconditionFailed = 412;
  static const int payloadTooLarge = 413;
  static const int uriTooLong = 414;
  static const int unsupportedMediaType = 415;
  static const int rangeNotSatisfiable = 416;
  static const int expectationFailed = 417;
  static const int imATeapot = 418;
  static const int misdirectedRequest = 421;
  static const int unprocessableEntity = 422;
  static const int locked = 423;
  static const int failedDependency = 424;
  static const int tooEarly = 425;
  static const int upgradeRequired = 426;
  static const int preconditionRequired = 428;
  static const int tooManyRequests = 429;
  static const int requestHeaderFieldsTooLarge = 431;
  static const int unavailableForLegalReasons = 451;
  static const int internalServerError = 500;
  static const int notImplemented = 501;
  static const int badGateway = 502;
  static const int serviceUnavailable = 503;
  static const int gatewayTimeout = 504;
  static const int httpVersionNotSupported = 505;
  static const int variantAlsoNegotiates = 506;
  static const int insufficientStorage = 507;
  static const int loopDetected = 508;
  static const int notExtended = 510;
  static const int networkAuthenticationRequired = 511;

  // Converts a numeric status code to a human-readable string.
  static String reasonPhrase(int statusCode) {
    switch (statusCode) {
      case continue_:
        return 'Continue';
      case switchingProtocols:
        return 'Switching Protocols';
      case processing:
        return 'Processing';
      case earlyHints:
        return 'Early Hints';
      case ok:
        return 'OK';
      case created:
        return 'Created';
      case accepted:
        return 'Accepted';
      case nonAuthoritativeInformation:
        return 'Non-Authoritative Information';
      case noContent:
        return 'No Content';
      case resetContent:
        return 'Reset Content';
      case partialContent:
        return 'Partial Content';
      case multiStatus:
        return 'Multi-Status';
      case alreadyReported:
        return 'Already Reported';
      case imUsed:
        return 'IM Used';
      case multipleChoices:
        return 'Multiple Choices';
      case movedPermanently:
        return 'Moved Permanently';
      case found:
        return 'Found';
      case seeOther:
        return 'See Other';
      case notModified:
        return 'Not Modified';
      case useProxy:
        return 'Use Proxy';
      case switchProxy:
        return 'Switch Proxy';
      case temporaryRedirect:
        return 'Temporary Redirect';
      case permanentRedirect:
        return 'Permanent Redirect';
      case badRequest:
        return 'Bad Request';
      case unauthorized:
        return 'Unauthorized';
      case paymentRequired:
        return 'Payment Required';
      case forbidden:
        return 'Forbidden';
      case notFound:
        return 'Not Found';
      case methodNotAllowed:
        return 'Method Not Allowed';
      case notAcceptable:
        return 'Not Acceptable';
      case proxyAuthenticationRequired:
        return 'Proxy Authentication Required';
      case requestTimeout:
        return 'Request Timeout';
      case conflict:
        return 'Conflict';
      case gone:
        return 'Gone';
      case lengthRequired:
        return 'Length Required';
      case preconditionFailed:
        return 'Precondition Failed';
