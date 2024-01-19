```typescript
// Import the necessary modules.
import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

// Define the component.
@Component({
  selector: 'app-complex-form',
  templateUrl: './complex-form.component.html',
  styleUrls: ['./complex-form.component.css']
})
export class ComplexFormComponent implements OnInit {

  // Define the input and output properties.
  @Input() formData: any;
  @Output() onSubmit = new EventEmitter<any>();

  // Define the form group.
  formGroup: FormGroup;

  // Define the constructor.
  constructor(private formBuilder: FormBuilder) { }

  // Define the ngOnInit method.
  ngOnInit() {
    // Initialize the form group.
    this.formGroup = this.formBuilder.group({
      name: ['', Validators.required],
      email: ['', Validators.email],
      password: ['', Validators.required],
      confirmPassword: ['', Validators.required],
      address: this.formBuilder.group({
        street: ['', Validators.required],
        city: ['', Validators.required],
        state: ['', Validators.required],
        zip: ['', Validators.required]
      }),
      hobbies: [[]]
    });

    // Set the form data.
    this.formGroup.patchValue(this.formData);
  }

  // Define the onSubmit method.
  onSubmitClick() {
    // Emit the form data.
    this.onSubmit.emit(this.formGroup.value);
  }
}
```

This code is a complex form component written in TypeScript. It uses Angular's reactive forms module to create a form with nested form groups and validation. The form has fields for name, email, password, confirm password, address, and hobbies.

The component has an input property called `formData` that is used to set the initial values of the form. It also has an output property called `onSubmit` that emits the form data when the form is submitted.

The `ngOnInit` method is used to initialize the form group and set the form data. The `onSubmitClick` method is used to emit the form data when the form is submitted.

This code is complex because it uses nested form groups and validation. It also has a lot of different fields and input validation. This code would be difficult to maintain and update, so it is important to use a tool like a linter to help keep the code clean and organized.