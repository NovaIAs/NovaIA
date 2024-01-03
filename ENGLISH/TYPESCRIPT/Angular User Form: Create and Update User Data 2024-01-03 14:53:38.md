```typescript
// Import necessary libraries
import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { UserService } from '../user.service';

@Component({
  selector: 'app-user-form',
  templateUrl: './user-form.component.html',
  styleUrls: ['./user-form.component.css']
})
export class UserFormComponent implements OnInit {

  // Define component properties
  @Input() user: any; // Input property to receive user data from parent component
  @Output() submitFormEvent = new EventEmitter(); // Output event to emit when form is submitted

  userForm: FormGroup; // FormGroup instance for user form
  submitted = false; // Keep track of form submission status
  errorMessage: string = ''; // Error message to display if there is an error

  constructor(private formBuilder: FormBuilder, private userService: UserService) { }

  ngOnInit(): void {
    // Initialize user form with FormBuilder
    this.userForm = this.formBuilder.group({
      name: ['', Validators.required],
      email: ['', [Validators.required, Validators.email]],
      phone: ['', [Validators.required, Validators.minLength(10)]],
      address: ['', Validators.required]
    });

    // Prepopulate form fields with user data if editing an existing user
    if (this.user) {
      this.userForm.patchValue(this.user);
    }
  }

  // Submit user form
  onSubmit(): void {
    this.submitted = true;

    // Stop if form is invalid
    if (this.userForm.invalid) {
      return;
    }

    // Prepare user data to be sent to the server
    const user = this.userForm.value;

    // Call user service to create or update user
    if (this.user) {
      // Update existing user
      this.userService.updateUser(user).subscribe(
        (data: any) => {
          this.submitFormEvent.emit(data); // Emit event with updated user data
        },
        (error: any) => {
          this.errorMessage = error.message; // Display error message
        }
      );
    } else {
      // Create new user
      this.userService.createUser(user).subscribe(
        (data: any) => {
          this.submitFormEvent.emit(data); // Emit event with new user data
        },
        (error: any) => {
          this.errorMessage = error.message; // Display error message
        }
      );
    }
  }

  // Reset user form
  resetForm(): void {
    this.userForm.reset();
    this.submitted = false;
  }
}
```

Explanation:

This TypeScript code defines a component called `UserFormComponent` for creating and updating user data in an Angular application. It utilizes Angular's form handling capabilities and integrates with a user service to communicate with a server for user data operations. Here's a breakdown of the code:

1. **Component Definition**:
   - `@Component` decorator defines the component with its selector (`app-user-form`), template URL (`./user-form.component.html`), and stylesheet URL (`./user-form.component.css`).

2. **Properties**:
   - `@Input() user`: Input property to receive user data from the parent component.
   - `@Output() submitFormEvent`: Output event to emit when the form is submitted.

3. **Constructor**:
   - Initializes necessary Angular services and dependencies.

4. **ngOnInit()**:
   - Initializes the user form using `FormBuilder` and sets its initial values based on the `user` input property.

5. **onSubmit()**:
   - Handles user form submission:
     - Checks for form validity.
     - Prepares user data from the form.
     - Calls the user service to create or update a user based on the existence of `this.user`.
     - Emits the `submitFormEvent` with updated or new user data.

6. **resetForm()**:
   - Resets the user form and clears the submitted status.

7. **HTML Template (user-form.component.html)**:
   - Defines the HTML template for the user form, including form fields and buttons.

8. **CSS Styles (user-form.component.css)**:
   - Contains styling rules for the user form component.