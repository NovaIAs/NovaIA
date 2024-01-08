```typescript
// Importing the necessary modules
import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

// Defining the component
@Component({
  selector: 'app-complex-component',
  templateUrl: './complex-component.html',
  styleUrls: ['./complex-component.css']
})
export class ComplexComponent implements OnInit {

  // Component properties
  @Input() data: any; // Input property to receive data from parent component
  @Output() outputEvent = new EventEmitter<any>(); // Output event to send data to parent component

  // Form-related properties
  form: FormGroup; // Reactive form
  submitted = false; // Flag to check if the form has been submitted

  // Constructor
  constructor(private formBuilder: FormBuilder) { }

  // Initializing the component
  ngOnInit(): void {
    // Creating the reactive form
    this.form = this.formBuilder.group({
      name: ['', [Validators.required, Validators.minLength(3)]],
      email: ['', [Validators.required, Validators.email]],
      message: ['', [Validators.required, Validators.maxLength(500)]]
    });
  }

  // Submitting the form
  onSubmit(): void {
    this.submitted = true;

    // Checking if the form is valid
    if (this.form.invalid) {
      return;
    }

    // Sending the form data to the parent component
    this.outputEvent.emit(this.form.value);

    // Resetting the form
    this.form.reset();
    this.submitted = false;
  }
}
```

This code defines a complex Angular component with various features:

1. **Input and Output Properties:**
   - It receives data from the parent component through the `@Input()` property.
   - It sends data back to the parent component through the `@Output()` event emitter.

2. **Reactive Form:**
   - It uses Angular's reactive forms module to create a form with validation.

3. **Validation:**
   - It applies validation rules to the form fields using Angular's built-in validators.

4. **Form Submission:**
   - It handles form submission and emits the form data to the parent component.

5. **Resetting the Form:**
   - It resets the form after successful submission.

This code demonstrates the use of advanced Angular concepts like input/output properties, reactive forms, validation, and event handling. It provides a reusable and customizable component that can be used in various scenarios.