```typescript
// Import the necessary libraries.
import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, FormControl } from '@angular/forms';
import { Subscription } from 'rxjs';

// Define the component.
@Component({
  selector: 'app-dynamic-form',
  templateUrl: './dynamic-form.component.html',
  styleUrls: ['./dynamic-form.component.scss']
})
export class DynamicFormComponent implements OnInit {

  // Define the input and output properties.
  @Input() fields: Array<any>;
  @Input() submitText: string;
  @Output() onSubmit = new EventEmitter();

  // Define the form group.
  form: FormGroup;

  // Define the subscription.
  subscription: Subscription;

  // Define the constructor.
  constructor(private fb: FormBuilder) { }

  // Define the ngOnInit method.
  ngOnInit() {
    // Create the form group.
    this.form = this.fb.group({});

    // Add the fields to the form group.
    this.fields.forEach((field) => {
      this.form.addControl(field.name, new FormControl(field.value));
    });

    // Subscribe to the form group value changes.
    this.subscription = this.form.valueChanges.subscribe((value) => {
      // Emit the value of the form group.
      this.onSubmit.emit(value);
    });
  }

  // Define the ngOnDestroy method.
  ngOnDestroy() {
    // Unsubscribe from the subscription.
    this.subscription.unsubscribe();
  }

  // Define the submit method.
  submit() {
    // Emit the value of the form group.
    this.onSubmit.emit(this.form.value);
  }

  // Define the addField method.
  addField() {
    // Get the last field.
    const lastField = this.fields[this.fields.length - 1];

    // Create a new field.
    const newField = {
      name: `field${this.fields.length + 1}`,
      label: `Field ${this.fields.length + 1}`,
      type: 'text',
      value: ''
    };

    // Add the new field to the fields array.
    this.fields.push(newField);

    // Add the new field to the form group.
    this.form.addControl(newField.name, new FormControl(newField.value));
  }

  // Define the removeField method.
  removeField(index: number) {
    // Remove the field from the fields array.
    this.fields.splice(index, 1);

    // Remove the field from the form group.
    this.form.removeControl(`field${index + 1}`);
  }
}
```

// Explain the code.

// The code is a component that dynamically creates a form based on an array of fields.
// The component has an input property called fields, which is an array of objects that define the fields in the form.
// The component also has an input property called submitText, which is the text that will be displayed on the submit button.
// The component has an output property called onSubmit, which is an event emitter that will be emitted when the form is submitted.

// The component uses the FormBuilder service to create the form group.
// The FormBuilder service is used to create form groups, which are objects that contain form controls.
// The component adds the fields to the form group using the addControl method.
// The addControl method takes two arguments: the name of the field and the form control.

// The component subscribes to the form group value changes using the valueChanges property.
// The valueChanges property is an observable that emits the value of the form group whenever it changes.
// The component uses the subscribe method to subscribe to the observable.
// The subscribe method takes a callback function as an argument.
// The callback function is called whenever the value of the form group changes.
// The callback function emits the value of the form group using the onSubmit event emitter.

// The component has a submit method that is called when the form is submitted.
// The submit method emits the value of the form group using the onSubmit event emitter.

// The component also has an addField method and a removeField method.
// The addField method adds a new field to the form group.
// The removeField method removes a field from the form group.

// The component is used in a template to create a dynamic form.
// The template uses the fields array to generate the fields in the form.
// The template also uses the submitText property to display the text on the submit button.
// The template uses the onSubmit event emitter to submit the form.