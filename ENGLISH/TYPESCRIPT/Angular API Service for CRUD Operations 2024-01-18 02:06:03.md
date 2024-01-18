```typescript
// Import the necessary modules.
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

// Create an Injectable decorator for the service.
@Injectable({
  providedIn: 'root'
})

// Create the class for the service.
export class ApiService {

  // Define the base URL for the API.
  private baseUrl = 'https://api.example.com';

  // Inject the HttpClient service into the constructor.
  constructor(private http: HttpClient) { }

  // Function to get all the data from the API.
  getAllData(): Observable<any> {
    return this.http.get(`${this.baseUrl}/data`);
  }

  // Function to get a single data item from the API by its ID.
  getDataById(id: number): Observable<any> {
    return this.http.get(`${this.baseUrl}/data/${id}`);
  }

  // Function to create a new data item in the API.
  createData(data: any): Observable<any> {
    return this.http.post(`${this.baseUrl}/data`, data);
  }

  // Function to update an existing data item in the API.
  updateData(id: number, data: any): Observable<any> {
    return this.http.put(`${this.baseUrl}/data/${id}`, data);
  }

  // Function to delete a data item from the API by its ID.
  deleteData(id: number): Observable<any> {
    return this.http.delete(`${this.baseUrl}/data/${id}`);
  }
}
```

**Explanation:**

* This code creates an Angular service called `ApiService` that provides a set of functions to interact with a remote API.
* The `getAllData()` function makes a GET request to the API's `/data` endpoint and returns an `Observable` that emits the API's response.
* The `getDataById()` function takes an ID as a parameter and makes a GET request to the API's `/data/:id` endpoint. It returns an `Observable` that emits the API's response for the data item with the specified ID.
* The `createData()` function takes a data object as a parameter and makes a POST request to the API's `/data` endpoint. It returns an `Observable` that emits the API's response for the newly created data item.
* The `updateData()` function takes an ID and a data object as parameters and makes a PUT request to the API's `/data/:id` endpoint. It returns an `Observable` that emits the API's response for the updated data item.
* The `deleteData()` function takes an ID as a parameter and makes a DELETE request to the API's `/data/:id` endpoint. It returns an `Observable` that emits the API's response for the deleted data item.