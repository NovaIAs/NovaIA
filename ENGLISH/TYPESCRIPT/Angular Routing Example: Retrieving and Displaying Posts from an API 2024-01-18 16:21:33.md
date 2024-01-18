```typescript
// Import necessary libraries
import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

// Define the AppComponent class
@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit {
  // Declare variables
  title = 'Angular Routing Example';
  posts: any;

  // Inject the HttpClient service into the constructor
  constructor(private http: HttpClient) { }

  // ngOnInit lifecycle method
  ngOnInit(): void {
    // Make an HTTP GET request to the API to get the posts
    this.getPosts().subscribe(data => {
      this.posts = data;
    });
  }

  // Function to get posts from the API
  getPosts(): Observable<any> {
    return this.http.get('https://jsonplaceholder.typicode.com/posts');
  }
}
```

Explanation:

1. We import necessary libraries, including the `Component` and `OnInit` decorators from `@angular/core`, the `HttpClient` from `@angular/common/http`, and `Observable` from `rxjs`.

2. The `AppComponent` class is defined with the `@Component` decorator, which specifies its template and stylesheet.

3. The `title` variable is used to set the title of the page.

4. The `posts` variable is used to store the posts fetched from the API.

5. In the constructor, the `HttpClient` service is injected.

6. The `ngOnInit` lifecycle method is implemented to fetch the posts from the API on component initialization.

7. The `getPosts()` method is defined to make an HTTP GET request to the API and return the posts as an `Observable`.

8. In the template (HTML), the `*ngFor` directive is used to iterate over the posts and display them in a list.

When you run this code in an Angular application, it will fetch posts from a mock API and display them in a list on the web page.