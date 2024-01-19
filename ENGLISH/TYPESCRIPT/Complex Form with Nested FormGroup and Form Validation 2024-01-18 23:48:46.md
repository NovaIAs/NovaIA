```typescript
// Import required libraries
import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';

// Define the component
@Component({
  selector: 'app-complex-form',
  templateUrl: './complex-form.component.html',
  styleUrls: ['./complex-form.component.css']
})

// Implement the component class
export class ComplexFormComponent implements OnInit {

  // Define component properties
  @Input() formValues: any;
  @Output() formSubmitted = new EventEmitter<any>();
  complexForm: FormGroup;

  // Inject FormBuilder
  constructor(private fb: FormBuilder) { }

  // Initialize the form on component initialization
  ngOnInit(): void {
    this.complexForm = this.fb.group({
      name: ['', Validators.required],
      email: ['', [Validators.required, Validators.email]],
      phone: ['', [Validators.required, Validators.pattern(/^[\+]?[(]?[0-9]{3}[)]?[-\s\.]?[0-9]{3}[-\s\.]?[0-9]{4,6}$/gm)]],
      address: this.fb.group({
        street: ['', Validators.required],
        city: ['', Validators.required],
        state: ['', Validators.required],
        zip: ['', Validators.required]
      })
    });

    // Pre-fill the form with data if available
    if (this.formValues) {
      this.complexForm.patchValue(this.formValues);
    }
  }

  // Submit the form
  onSubmit() {
    if (this.complexForm.valid) {
      this.formSubmitted.emit(this.complexForm.value);
    } else {
      alert('Please correct the errors in the form.');
    }
  }

}
```

Explanation:

This TypeScript code defines a complex form using Angular's reactive forms module. Here's a breakdown of the code:

1. **Component Definition**:
   - `@Component` decorator defines the component with its selector, template, and stylesheet.

2. **Component Class**:
   - `ComplexFormComponent` implements the `OnInit` interface to initialize the component.
   - `@Input() formValues` and `@Output() formSubmitted` are used for passing data to and from the parent component.

3. **Form Initialization**:
   - `ngOnInit()` method initializes the form using `FormBuilder`.
   - Nested FormGroup is used for the address fields.
   - Form validation is also implemented using `Validators`.

4. **Form Submission**:
   - `onSubmit()` method handles form submission.
   - It emits the form values if the form is valid, otherwise displays an alert message.