```typescript
// Import necessary modules.
import { Injectable, Inject } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

// Define the service metadata.
@Injectable({
  providedIn: 'root'
})

// Define the service class.
export class ComplexService {

  // Define the base URL for the API.
  private baseUrl: string;

  // Inject the HTTP client and the base URL.
  constructor(private http: HttpClient, @Inject('BASE_URL') baseUrl: string) {
    this.baseUrl = baseUrl;
  }

  // Define a method to get all items.
  getAllItems(): Observable<any> {
    // Define the URL for the API endpoint.
    const url = `${this.baseUrl}api/items`;

    // Define the HTTP headers.
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    });

    // Make the HTTP GET request.
    return this.http.get(url, { headers })
      .pipe(map(response => response));
  }

  // Define a method to get a single item by its ID.
  getItemById(id: number): Observable<any> {
    // Define the URL for the API endpoint.
    const url = `${this.baseUrl}api/items/${id}`;

    // Define the HTTP headers.
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    });

    // Make the HTTP GET request.
    return this.http.get(url, { headers })
      .pipe(map(response => response));
  }

  // Define a method to create a new item.
  createItem(item: any): Observable<any> {
    // Define the URL for the API endpoint.
    const url = `${this.baseUrl}api/items`;

    // Define the HTTP headers.
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    });

    // Make the HTTP POST request.
    return this.http.post(url, item, { headers })
      .pipe(map(response => response));
  }

  // Define a method to update an existing item.
  updateItem(item: any): Observable<any> {
    // Define the URL for the API endpoint.
    const url = `${this.baseUrl}api/items/${item.id}`;

    // Define the HTTP headers.
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    });

    // Make the HTTP PUT request.
    return this.http.put(url, item, { headers })
      .pipe(map(response => response));
  }

  // Define a method to delete an existing item.
  deleteItem(id: number): Observable<any> {
    // Define the URL for the API endpoint.
    const url = `${this.baseUrl}api/items/${id}`;

    // Define the HTTP headers.
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    });

    // Make the HTTP DELETE request.
    return this.http.delete(url, { headers })
      .pipe(map(response => response));
  }
}
```

**Explanation:**

This code defines a complex Angular service class called `ComplexService`. The service provides a set of methods for interacting with a RESTful API:

1. `getAllItems()`: This method fetches all items from the API.
2. `getItemById(id)`: This method fetches a single item from the API by its ID.
3. `createItem(item)`: This method creates a new item in the API.
4. `updateItem(item)`: This method updates an existing item in the API.
5. `deleteItem(id)`: This method deletes an existing item from the API.

The service uses the Angular HTTP client to make HTTP requests to the API. It also uses the `map()` operator from RxJS to transform the HTTP responses into the desired format.

The service is designed to be reusable and can be injected into any Angular component or service that needs to interact with the API. It provides a consistent and easy-to-use interface for working with the API, abstracting away the details of making HTTP requests and handling responses.

Overall, this code demonstrates the use of Angular services, dependency injection, HTTP requests, and RxJS operators to create a complex and reusable service for interacting with a RESTful API.