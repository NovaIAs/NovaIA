```typescript
// Import necessary libraries
import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { catchError, tap } from 'rxjs/operators';

// Define the service using the @Injectable decorator
@Injectable({
  providedIn: 'root'
})
export class ApiService {
  private apiUrl = 'https://my-api.com/api/'; // Replace with your API URL

  constructor(private http: HttpClient) { }

  // Function to make a GET request to the API
  get(endpoint: string): Observable<any> {
    return this.http.get(this.apiUrl + endpoint)
      .pipe(
        tap(data => console.log(data)), // Log the response data
        catchError(this.handleError) // Handle any errors
      );
  }

  // Function to make a POST request to the API
  post(endpoint: string, data: any): Observable<any> {
    return this.http.post(this.apiUrl + endpoint, data)
      .pipe(
        tap(data => console.log(data)), // Log the response data
        catchError(this.handleError) // Handle any errors
      );
  }

  // Function to make a PUT request to the API
  put(endpoint: string, data: any): Observable<any> {
    return this.http.put(this.apiUrl + endpoint, data)
      .pipe(
        tap(data => console.log(data)), // Log the response data
        catchError(this.handleError) // Handle any errors
      );
  }

  // Function to make a DELETE request to the API
  delete(endpoint: string): Observable<any> {
    return this.http.delete(this.apiUrl + endpoint)
      .pipe(
        tap(data => console.log(data)), // Log the response data
        catchError(this.handleError) // Handle any errors
      );
  }

  // Function to handle errors
  private handleError(err: any): Observable<never> {
    let errorMessage: string;
    if (err.error instanceof ErrorEvent) {
      // Client-side error
      errorMessage = `Error: ${err.error.message}`;
    } else {
      // Server-side error
      errorMessage = `Error Code: ${err.status}\nMessage: ${err.message}`;
    }
    console.error(errorMessage);
    return throwError(errorMessage);
  }
}
```

Explanation:

* The code defines an Angular service called `ApiService` using the `@Injectable` decorator.
* The service has a private `apiUrl` property that stores the base URL of the API.
* It has four methods: `get`, `post`, `put`, and `delete`, which are used to make HTTP requests to the API using the Angular `HttpClient`.
* Each method takes an `endpoint` parameter, which is the specific API endpoint to which the request is being made.
* The `get` method makes a GET request, the `post` method makes a POST request, the `put` method makes a PUT request, and the `delete` method makes a DELETE request.
* The `tap` operator is used to log the response data to the console.
* The `catchError` operator is used to handle any errors that occur during the request.
* The `handleError` method is a private method used to handle errors. It logs the error message to the console and returns an `Observable` that emits the error message.