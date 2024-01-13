```typescript
// Import necessary modules.
import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { HttpClient } from '@angular/common/http';

// Define the component.
@Component({
  selector: 'app-complex-form',
  templateUrl: './complex-form.component.html',
  styleUrls: ['./complex-form.component.css']
})

// Implement the component class.
export class ComplexFormComponent implements OnInit {

  // Define input and output properties.
  @Input() initialData: any;
  @Output() formData = new EventEmitter<any>();

  // Define form group and its fields.
  form: FormGroup;
  fields = {
    name: ['', [Validators.required, Validators.minLength(3)]],
    email: ['', [Validators.required, Validators.email]],
    phone: ['', [Validators.required, Validators.pattern('^[0-9]{10}$')]],
    address: ['', [Validators.required, Validators.minLength(10)]],
    city: ['', [Validators.required]],
    state: ['', [Validators.required]],
    zip: ['', [Validators.required, Validators.pattern('^[0-9]{5}$')]],
    country: ['', [Validators.required]],
    dob: ['', [Validators.required]],
    gender: ['', [Validators.required]],
    education: ['', [Validators.required]],
    experience: ['', [Validators.required]],
    skills: ['', [Validators.required]],
    hobbies: ['', [Validators.required]],
    about: ['', [Validators.required, Validators.minLength(20)]],
    terms: [false, [Validators.requiredTrue]]
  };

  // Define constructor.
  constructor(private fb: FormBuilder, private http: HttpClient) { }

  // Initialize the component.
  ngOnInit() {
    // Build the form group.
    this.form = this.fb.group(this.fields);

    // Check if initial data is provided.
    if (this.initialData) {
      // Patch the form with initial data.
      this.form.patchValue(this.initialData);
    }
  }

  // Handle form submission.
  onSubmit() {
    // Check if the form is valid.
    if (this.form.invalid) {
      // Mark all fields as touched to show errors.
      this.form.markAllAsTouched();
      return;
    }

    // Emit the form data.
    this.formData.emit(this.form.value);
  }

  // Handle reset button click.
  onReset() {
    // Reset the form.
    this.form.reset();
  }
}
```

Explanation:

1. We import necessary Angular modules.

2. We define the component with `@Component` decorator and specify its selector, template, and styles.

3. In the component class, we define input and output properties using `@Input` and `@Output` decorators.

4. We define the form group and its fields using FormBuilder.

5. In the `ngOnInit` method, we build the form group and optionally patch it with initial data if provided.

6. The `onSubmit` method handles form submission and emits the form data if the form is valid.

7. The `onReset` method handles the reset button click and resets the form.

This code creates a complex form with various fields and validation rules. It allows the user to enter and submit data, and it provides a way to reset the form. The form data is emitted as an event when the form is submitted.