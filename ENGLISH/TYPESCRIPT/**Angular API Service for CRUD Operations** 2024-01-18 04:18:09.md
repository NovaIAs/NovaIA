```typescript
// Import the necessary libraries.
import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';

// Define the injectable service.
@Injectable({
  providedIn: 'root'
})
export class ApiService {

  // Define the base URL for the API.
  private baseUrl = 'https://api.example.com/';

  // Define the HTTP headers.
  private headers = new HttpHeaders({
    'Content-Type': 'application/json'
  });

  // Define the constructor for the service.
  constructor(private http: HttpClient) { }

  // Define a method to get all the data from the API.
  getAllData(): Observable<any> {
    return this.http.get(this.baseUrl + 'data', { headers: this.headers });
  }

  // Define a method to get a specific data item from the API.
  getDataById(id: number): Observable<any> {
    return this.http.get(this.baseUrl + 'data/' + id, { headers: this.headers });
  }

  // Define a method to create a new data item in the API.
  createData(data: any): Observable<any> {
    return this.http.post(this.baseUrl + 'data', data, { headers: this.headers });
  }

  // Define a method to update a data item in the API.
  updateData(id: number, data: any): Observable<any> {
    return this.http.put(this.baseUrl + 'data/' + id, data, { headers: this.headers });
  }

  // Define a method to delete a data item from the API.
  deleteData(id: number): Observable<any> {
    return this.http.delete(this.baseUrl + 'data/' + id, { headers: this.headers });
  }
}
```

**Explanation:**

* The above code is written in TypeScript, which is a superset of JavaScript.
* The `ApiService` class is decorated with the `@Injectable()` decorator, which indicates that it is a service that can be injected into other components.
* The `getAllData()`, `getDataById()`, `createData()`, `updateData()`, and `deleteData()` methods are all public methods of the `ApiService` class.
* The `getAllData()` method returns an `Observable` of type `any`, which means that it can emit any type of data.
* The `getDataById()` method takes a number as an argument and returns an `Observable` of type `any`.
* The `createData()` method takes a data object as an argument and returns an `Observable` of type `any`.
* The `updateData()` method takes a number and a data object as arguments and returns an `Observable` of type `any`.
* The `deleteData()` method takes a number as an argument and returns an `Observable` of type `any`.
* The `headers` constant defines the HTTP headers that will be used for all requests made by the `ApiService`.
* The `baseUrl` constant defines the base URL for the API.

This code can be used to create a simple CRUD (Create, Read, Update, Delete) application in Angular.