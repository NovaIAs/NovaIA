```typescript
// Import the necessary libraries
import { Component, OnInit, Input } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { UserService } from '../user.service';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {

  // Define the form group
  loginForm: FormGroup;

  // Define the error message
  errorMessage: string;

  // Inject the necessary services
  constructor(private formBuilder: FormBuilder, private router: Router, private userService: UserService) { }

  ngOnInit() {
    // Create the form group
    this.loginForm = this.formBuilder.group({
      username: ['', Validators.required],
      password: ['', Validators.required]
    });
  }

  // Handle the login button click
  onLogin() {
    // Check if the form is valid
    if (this.loginForm.invalid) {
      return;
    }

    // Get the username and password from the form
    const username = this.loginForm.value.username;
    const password = this.loginForm.value.password;

    // Call the user service to login
    this.userService.login(username, password).subscribe(
      (data) => {
        // Store the token in local storage
        localStorage.setItem('token', data.token);

        // Redirect to the home page
        this.router.navigate(['/home']);
      },
      (error) => {
        // Display the error message
        this.errorMessage = error.error.message;
      }
    );
  }
}
```

This code is a component in an Angular application that handles the user login functionality. The component uses the FormBuilder service to create a form group, which contains the username and password fields. The form group is then validated using the Validators service to ensure that the fields are not empty.

When the user clicks the login button, the onLogin() method is called. This method checks if the form is valid and, if so, gets the username and password from the form. The method then calls the login() method of the UserService to authenticate the user. If the authentication is successful, the token is stored in local storage and the user is redirected to the home page. If the authentication fails, an error message is displayed.

This code demonstrates the use of Angular's form builder, validators, and routing services. It also shows how to interact with a user service to authenticate a user.