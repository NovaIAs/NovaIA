```typescript
// Import the necessary libraries.
import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable, of } from 'rxjs';
import { catchError, map, tap } from 'rxjs/operators';

// Define the service.
@Injectable({
  providedIn: 'root'
})
export class DataService {

  // Define the base URL for the API.
  private apiUrl = 'https://api.example.com';

  // Define the HTTP headers.
  private httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json'
    })
  };

  // Define the constructor.
  constructor(private http: HttpClient) { }

  // Define the method to get all the data.
  getAllData(): Observable<any> {
    return this.http.get(this.apiUrl, this.httpOptions).pipe(
      map(res => res),
      catchError(this.handleError<any>('getAllData'))
    );
  }

  // Define the method to get a single data item.
  getDataItem(id: number): Observable<any> {
    return this.http.get(`${this.apiUrl}/${id}`, this.httpOptions).pipe(
      map(res => res),
      catchError(this.handleError<any>('getDataItem'))
    );
  }

  // Define the method to create a new data item.
  createDataItem(data: any): Observable<any> {
    return this.http.post(this.apiUrl, data, this.httpOptions).pipe(
      map(res => res),
      catchError(this.handleError<any>('createDataItem'))
    );
  }

  // Define the method to update a data item.
  updateDataItem(id: number, data: any): Observable<any> {
    return this.http.put(`${this.apiUrl}/${id}`, data, this.httpOptions).pipe(
      map(res => res),
      catchError(this.handleError<any>('updateDataItem'))
    );
  }

  // Define the method to delete a data item.
  deleteDataItem(id: number): Observable<any> {
    return this.http.delete(`${this.apiUrl}/${id}`, this.httpOptions).pipe(
      map(res => res),
      catchError(this.handleError<any>('deleteDataItem'))
    );
  }

  // Define the method to handle errors.
  private handleError<T>(operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {
      // TODO: send the error to a remote logging infrastructure
      console.error(error); // log to console instead

      // Let the app keep running by returning an empty result.
      return of(result as T);
    };
  }
}
```

**Explanation:**

* The code imports the necessary libraries.
* The service is defined with the `@Injectable()` decorator.
* The base URL for the API is defined.
* The HTTP headers are defined.
* The constructor is defined.
* The methods to get all the data, get a single data item, create a new data item, update a data item, and delete a data item are defined.
* The method to handle errors is defined.

This code is a complex and differentiated example of how to use TypeScript to create a service that can be used to interact with an API. The code is well-organized and easy to read, and it uses a variety of TypeScript features, such as generics, decorators, and arrow functions.