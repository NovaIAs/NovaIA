```typescript
// Import necessary modules
import { Injectable, OnInit, OnDestroy } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable, Subscription } from 'rxjs';

// Define the service class
@Injectable({
  providedIn: 'root'
})
export class ComplexService implements OnInit, OnDestroy {

  // Define private variables
  private apiUrl: string = 'https://example.com/api';
  private subscription: Subscription;

  // Constructor to initialize the service
  constructor(private http: HttpClient) { }

  // Lifecycle method to initialize the service
  ngOnInit(): void {
    // Subscribe to an observable using the 'interval' operator
    this.subscription = Observable.interval(1000).subscribe(() => {
      console.log('Emitted value from observable');
    });
  }

  // Lifecycle method to destroy the service
  ngOnDestroy(): void {
    // Unsubscribe from the observable to prevent memory leaks
    this.subscription.unsubscribe();
  }

  // Method to fetch data from an API
  getData(): Observable<any> {
    // Define the HTTP headers
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + localStorage.getItem('token')
    });

    // Make a GET request to the API
    return this.http.get(this.apiUrl + '/data', { headers });
  }

  // Method to post data to an API
  postData(data: any): Observable<any> {
    // Define the HTTP headers
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + localStorage.getItem('token')
    });

    // Make a POST request to the API
    return this.http.post(this.apiUrl + '/data', data, { headers });
  }

  // Method to update data in an API
  updateData(id: string, data: any): Observable<any> {
    // Define the HTTP headers
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + localStorage.getItem('token')
    });

    // Make a PUT request to the API
    return this.http.put(this.apiUrl + '/data/' + id, data, { headers });
  }

  // Method to delete data from an API
  deleteData(id: string): Observable<any> {
    // Define the HTTP headers
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + localStorage.getItem('token')
    });

    // Make a DELETE request to the API
    return this.http.delete(this.apiUrl + '/data/' + id, { headers });
  }
}
```

**Explanation:**

This is a complex TypeScript class that implements the `OnInit` and `OnDestroy` lifecycle hooks. It uses the `interval` operator from RxJS to emit values at regular intervals, and it defines several methods for interacting with an API. These methods include `getData`, `postData`, `updateData`, and `deleteData`, which allow the service to perform CRUD operations on a remote API.

The class also defines private variables for the API URL and a subscription to the observable. The `ngOnInit` method subscribes to the observable, and the `ngOnDestroy` method unsubscribes from it to prevent memory leaks.

The `getData`, `postData`, `updateData`, and `deleteData` methods all use the `HttpClient` service to make HTTP requests to the API. They define the appropriate HTTP headers and make the necessary requests to the API.

This class is a good example of how to use TypeScript to create a complex and reusable service that can be used in a variety of applications.