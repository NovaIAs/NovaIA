```typescript
// Import the necessary libraries.
import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Task, TaskStatus } from '../task';

// Define the component.
@Component({
  selector: 'app-task-form',
  templateUrl: './task-form.component.html',
  styleUrls: ['./task-form.component.css']
})
export class TaskFormComponent implements OnInit {

  // Define the input and output properties.
  @Input() task: Task;
  @Output() saveTask = new EventEmitter<Task>();

  // Define the form group.
  taskForm: FormGroup;

  // Define the constructor.
  constructor(private fb: FormBuilder) { }

  // Initialize the component.
  ngOnInit() {
    // Initialize the form group.
    this.taskForm = this.fb.group({
      id: [this.task.id],
      title: [this.task.title, Validators.required],
      description: [this.task.description],
      status: [this.task.status, Validators.required]
    });
  }

  // Save the task.
  onSaveTask() {
    // Get the task from the form.
    const task = this.taskForm.value;

    // Emit the save task event.
    this.saveTask.emit(task);
  }
}
```

This code defines a component that allows the user to create and edit tasks. The component uses a form group to manage the task data. The form group is initialized with the data from the task that is passed to the component as an input property. The component also has an output property that emits an event when the user saves the task.

The component's template uses the form group to create a form with fields for the task's title, description, and status. The form also has a save button that calls the onSaveTask() method when clicked.

The onSaveTask() method gets the task data from the form group and emits the save task event. The event is handled by the parent component, which can then save the task to the database.

This code is complex because it uses a number of different Angular features, including form groups, input and output properties, and event emitters. However, it is also a very flexible and powerful component that can be used to create and edit tasks in a variety of different applications.