```typescript
// Interface for defining a task
interface Task {
  id: string;
  title: string;
  description?: string;
  priority: TaskPriority;
  status: TaskStatus;
  assignedTo?: string;
  dueDate?: Date;
  completedDate?: Date;
  comments?: string[];
  attachments?: string[];
}

// Enum for defining task priorities
enum TaskPriority {
  LOW = 'low',
  MEDIUM = 'medium',
  HIGH = 'high'
}

// Enum for defining task statuses
enum TaskStatus {
  NEW = 'new',
  IN_PROGRESS = 'in_progress',
  COMPLETED = 'completed'
}

// Class for managing tasks
class TaskManager {
  private tasks: Task[] = [];

  // Method for adding a new task
  addTask(task: Task) {
    this.tasks.push(task);
  }

  // Method for getting all tasks
  getTasks(): Task[] {
    return this.tasks;
  }

  // Method for getting a task by its ID
  getTaskById(id: string): Task | undefined {
    return this.tasks.find((task) => task.id === id);
  }

  // Method for updating a task
  updateTask(id: string, updatedTask: Task) {
    const task = this.tasks.find((task) => task.id === id);
    if (task) {
      Object.assign(task, updatedTask);
    }
  }

  // Method for deleting a task
  deleteTask(id: string) {
    this.tasks = this.tasks.filter((task) => task.id !== id);
  }
}

// Creating an instance of the task manager
const taskManager = new TaskManager();

// Adding some sample tasks
taskManager.addTask({
  id: 'task-1',
  title: 'Write a blog post',
  priority: TaskPriority.HIGH,
  status: TaskStatus.NEW
});

taskManager.addTask({
  id: 'task-2',
  title: 'Design a website',
  priority: TaskPriority.MEDIUM,
  status: TaskStatus.IN_PROGRESS,
  assignedTo: 'John Doe'
});

taskManager.addTask({
  id: 'task-3',
  title: 'Complete a training module',
  priority: TaskPriority.LOW,
  status: TaskStatus.COMPLETED,
  completedDate: new Date()
});

// Getting all tasks
const allTasks = taskManager.getTasks();

// Getting a task by its ID
const taskById = taskManager.getTaskById('task-2');

// Updating a task
taskManager.updateTask('task-2', {
  title: 'Redesign a website',
  status: TaskStatus.NEW
});

// Deleting a task
taskManager.deleteTask('task-1');

// Displaying the updated task list
console.log(taskManager.getTasks());
```

Explanation:

1. We define an `interface Task` that represents the structure of a task. It includes properties like `id`, `title`, `description`, `priority`, `status`, `assignedTo`, `dueDate`, `completedDate`, `comments`, and `attachments`.

2. We create enums `TaskPriority` and `TaskStatus` to define the possible values for task priority and status.

3. We create a `TaskManager` class to manage tasks. It includes methods for adding, getting, updating, and deleting tasks.

4. We instantiate a `TaskManager` object and add some sample tasks to it.

5. We demonstrate various operations like getting all tasks, getting a task by ID, updating a task, deleting a task, and displaying the updated task list.

This code demonstrates a more complex and comprehensive use of TypeScript, including interfaces, enums, classes, and methods. It provides a more robust and structured way to manage tasks in a task management application.