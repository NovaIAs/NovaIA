```typescript
// Import necessary modules
import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { Task } from '../task';

// Define the component
@Component({
  selector: 'task-list',
  templateUrl: './task-list.component.html',
  styleUrls: ['./task-list.component.css']
})
export class TaskListComponent implements OnInit {

  // Define component properties
  @Input() tasks: Task[];
  @Output() onTaskSelected: EventEmitter<Task> = new EventEmitter();

  // Define component methods
  ngOnInit() { }

  selectTask(task: Task) {
    this.onTaskSelected.emit(task);
  }

}
```

**Explanation:**

* The `@Component` decorator is used to define the component. It specifies the component's selector, template URL, and style URL.
* The `tasks` property is an input property, which means that it can be passed data from the parent component.
* The `onTaskSelected` property is an output property, which means that it can emit events to the parent component.
* The `ngOnInit()` method is a lifecycle hook that is called when the component is initialized.
* The `selectTask()` method is a custom method that is called when a task is clicked. It emits an event to the parent component, passing the selected task as an argument.

This code creates a reusable component that can be used to display a list of tasks. The component accepts an array of tasks as an input property, and it emits an event when a task is clicked. This allows the parent component to handle the task selection and perform any necessary actions.