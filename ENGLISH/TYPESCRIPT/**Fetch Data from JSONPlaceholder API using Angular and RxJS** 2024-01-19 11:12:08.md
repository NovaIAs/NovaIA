```typescript
// Import necessary modules
import { Component, OnInit } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { catchError, map, tap } from 'rxjs/operators';

// Define the main component
@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit {

  // Define properties for data and error handling
  data: any;
  errorMessage: string;

  // Inject the HttpClient service into the constructor
  constructor(private http: HttpClient) { }

  // Angular lifecycle method to fetch data on component initialization
  ngOnInit() {
    // Define the API endpoint
    const apiUrl = 'https://jsonplaceholder.typicode.com/posts';

    // Set the HTTP headers
    const headers = new HttpHeaders({
      'Content-Type': 'application/json'
    });

    // Fetch data from the API using the GET method
    this.http.get<any[]>(apiUrl, { headers })
      .pipe(
        map(data => data), // Extract the data from the HTTP response
        catchError(error => this.handleError(error)) // Handle any errors that occur
      )
      .subscribe(data => {
        // Store the fetched data in the 'data' property
        this.data = data;
      });
  }

  // Define a method to handle errors
  private handleError(error: any): Observable<any> {
    // Set the error message
    this.errorMessage = error.message;

    // Return an empty Observable to prevent further processing
    return Observable.empty();
  }
}
```

**Explanation:**

1. **Importing necessary modules:**
   - We start by importing necessary modules from Angular and RxJS libraries.

2. **Main component definition:**
   - `AppComponent` is defined as the main component of the Angular application. It handles the logic and interaction with the API.

3. **Properties for data and error handling:**
   - We define properties for `data` to store the fetched data and `errorMessage` to handle any errors that may occur.

4. **Injecting the `HttpClient` service:**
   - We inject the `HttpClient` service into the component's constructor, which is used to make HTTP requests.

5. **Angular lifecycle methods:**
   - `ngOnInit()` is an Angular lifecycle method that is called after the component is initialized. We use it to fetch data from the API.

6. **Defining the API endpoint:**
   - We define the API endpoint URL from which we want to fetch data.

7. **Setting HTTP headers:**
   - We set the HTTP headers to specify the content type as JSON.

8. **Fetching data using the `GET` method:**
   - We use the `HttpClient` service to make a GET request to the API endpoint and store the response in an Observable.

9. **Applying RxJS operators:**
   - We use RxJS operators `map()` and `catchError()` to process the HTTP response.
   - `map()` extracts the data from the HTTP response.
   - `catchError()` handles any errors that occur during the data fetching process.

10. **Subscribing to the Observable:**
    - We subscribe to the Observable to get the fetched data and store it in the `data` property.

11. **Error handling function:**
    - We define a private method `handleError()` to handle any errors that may occur during the API call. It sets the `errorMessage` property and returns an empty Observable to prevent further processing.