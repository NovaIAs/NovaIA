```typescript
// Import necessary modules
import { Injectable, EventEmitter, Output } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { catchError, map } from 'rxjs/operators';

// Define the service
@Injectable({
  providedIn: 'root'
})
export class ComplexService {

  // Define an event emitter for error handling
  @Output() errorEmitter = new EventEmitter<string>();

  // Define the base URL for API calls
  private apiUrl = 'https://api.example.com';

  // Inject the HTTP client
  constructor(private http: HttpClient) { }

  // Function to get data from the API
  getData(): Observable<any> {
    // Define the headers for the request
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + localStorage.getItem('token')
    });

    // Make the GET request
    return this.http.get(this.apiUrl + '/data', { headers })
      .pipe(
        map(response => response),
        catchError(error => {
          // Emit the error message
          this.errorEmitter.emit(error.message);

          // Return an observable with the error
          return throwError(error);
        })
      );
  }

  // Function to post data to the API
  postData(data: any): Observable<any> {
    // Define the headers for the request
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + localStorage.getItem('token')
    });

    // Make the POST request
    return this.http.post(this.apiUrl + '/data', data, { headers })
      .pipe(
        map(response => response),
        catchError(error => {
          // Emit the error message
          this.errorEmitter.emit(error.message);

          // Return an observable with the error
          return throwError(error);
        })
      );
  }
}
```

Explanation:

1. **Import necessary modules**: The code begins by importing the necessary modules from the Angular framework and the RxJS library.

2. **Define the service**: The `ComplexService` class is defined as an Angular service using the `@Injectable` decorator. The service is provided at the root level, meaning it can be injected anywhere in the application.

3. **Define the event emitter for error handling**: An `EventEmitter` is created for emitting error messages. This allows other components in the application to subscribe to these error events.

4. **Define the base URL for API calls**: The `apiUrl` property is set to the base URL of the API that the service will communicate with.

5. **Inject the HTTP client**: The `HttpClient` is injected into the service using the constructor. This allows the service to make HTTP requests to the API.

6. **Function to get data from the API**: The `getData()` function is defined to make a GET request to the API and retrieve data. It uses the `http.get()` method to send the request and returns an `Observable` that emits the response data.

7. **Function to post data to the API**: The `postData()` function is defined to make a POST request to the API and send data. It uses the `http.post()` method to send the request and returns an `Observable` that emits the response data.

8. **Error handling**: Both the `getData()` and `postData()` functions use the `catchError` operator to handle errors that may occur during the HTTP requests. If an error occurs, an error message is emitted using the `errorEmitter` and the error is re-thrown as an `Observable`.

This code demonstrates a complex Angular service with error handling, data fetching, and data posting capabilities. It is a comprehensive example of a TypeScript service that interacts with an API.