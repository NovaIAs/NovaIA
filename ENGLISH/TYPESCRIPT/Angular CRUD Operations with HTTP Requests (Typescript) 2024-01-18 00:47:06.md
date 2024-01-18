```typescript
// Import necessary libraries
import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

// Define the API endpoint URL
const API_URL = 'https://example.com/api/';

// Create an Angular service
@Injectable({
  providedIn: 'root'
})
export class ApiService {

  // Inject the HTTP client service
  constructor(private http: HttpClient) { }

  // Define a method to get data from the API
  getData(): Observable<any> {
    // Set the headers for the HTTP request
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': 'Bearer <your_token>'
    });

    // Make the HTTP GET request to the API
    return this.http.get(API_URL, { headers })
      .pipe(
        // Map the response to extract the data
        map(response => response.data)
      );
  }

  // Define a method to send data to the API
  sendData(data: any): Observable<any> {
    // Set the headers for the HTTP request
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': 'Bearer <your_token>'
    });

    // Make the HTTP POST request to the API
    return this.http.post(API_URL, data, { headers })
      .pipe(
        // Map the response to extract the data
        map(response => response.data)
      );
  }

  // Define a method to update data in the API
  updateData(id: string, data: any): Observable<any> {
    // Set the headers for the HTTP request
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': 'Bearer <your_token>'
    });

    // Make the HTTP PUT request to the API
    return this.http.put(`${API_URL}/${id}`, data, { headers })
      .pipe(
        // Map the response to extract the data
        map(response => response.data)
      );
  }

  // Define a method to delete data from the API
  deleteData(id: string): Observable<any> {
    // Set the headers for the HTTP request
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': 'Bearer <your_token>'
    });

    // Make the HTTP DELETE request to the API
    return this.http.delete(`${API_URL}/${id}`, { headers })
      .pipe(
        // Map the response to extract the data
        map(response => response.data)
      );
  }
}
```

Explanation:

1. **Import necessary libraries**: We import the necessary Angular libraries, including `HttpClient`, `HttpHeaders`, `Observable`, and `map`.

2. **Define the API endpoint URL**: We define the base URL for the API endpoint. In this example, it's `https://example.com/api/`.

3. **Create an Angular service**: We create an Angular service called `ApiService` using the `@Injectable` decorator. This service will handle the communication with the API.

4. **Inject the HTTP client service**: We inject the `HttpClient` service into the `ApiService` constructor.

5. **Define methods for CRUD operations**: We define methods for each of the CRUD operations (Create, Read, Update, and Delete) using the `HttpClient` service. These methods make HTTP requests to the API and return Observables.

   - `getData()`: This method makes a GET request to the API to retrieve data.

   - `sendData()`: This method makes a POST request to the API to send data.

   - `updateData()`: This method makes a PUT request to the API to update data.

   - `deleteData()`: This method makes a DELETE request to the API to delete data.

6. **Set headers for HTTP requests**: For each HTTP request, we set the appropriate headers, including `Content-Type` and `Authorization`.

7. **Handle the API response**: We use the `map` operator to extract the data from the API response and return it as an Observable.

   - For data retrieval methods (`getData()`), we extract the `data` property from the response.

   - For data sending, update, and deletion methods, we extract the `data` property from the response, which usually contains the updated or deleted entity.