```typescript
// Define an interface for a task
interface Task {
  id: string;
  title: string;
  description: string;
  status: "new" | "in progress" | "completed";
  priority: "low" | "medium" | "high";
  dueDate: Date;
  assignedTo: string;
}

// Create an array of tasks
const tasks: Task[] = [
  {
    id: "1",
    title: "Task 1",
    description: "This is the first task.",
    status: "new",
    priority: "low",
    dueDate: new Date("2023-03-08"),
    assignedTo: "John Doe",
  },
  {
    id: "2",
    title: "Task 2",
    description: "This is the second task.",
    status: "in progress",
    priority: "medium",
    dueDate: new Date("2023-03-15"),
    assignedTo: "Jane Doe",
  },
  {
    id: "3",
    title: "Task 3",
    description: "This is the third task.",
    status: "completed",
    priority: "high",
    dueDate: new Date("2023-03-22"),
    assignedTo: "Michael Jones",
  },
];

// Define a function to filter tasks by status
const filterTasksByStatus = (tasks: Task[], status: string): Task[] => {
  return tasks.filter((task) => task.status === status);
};

// Define a function to filter tasks by priority
const filterTasksByPriority = (tasks: Task[], priority: string): Task[] => {
  return tasks.filter((task) => task.priority === priority);
};

// Define a function to filter tasks by due date
const filterTasksByDueDate = (tasks: Task[], dueDate: Date): Task[] => {
  return tasks.filter((task) => task.dueDate <= dueDate);
};

// Define a function to sort tasks by due date
const sortTasksByDueDate = (tasks: Task[]): Task[] => {
  return tasks.sort((a, b) => a.dueDate.getTime() - b.dueDate.getTime());
};

// Define a function to get the total number of tasks
const getTotalNumberOfTasks = (tasks: Task[]): number => {
  return tasks.length;
};

// Define a function to get the total number of completed tasks
const getTotalNumberOfCompletedTasks = (tasks: Task[]): number => {
  return tasks.filter((task) => task.status === "completed").length;
};

// Define a function to get the total number of tasks assigned to a specific person
const getTotalNumberOfTasksAssignedToPerson = (tasks: Task[], person: string): number => {
  return tasks.filter((task) => task.assignedTo === person).length;
};

// Define a function to get the average priority of tasks
const getAveragePriorityOfTasks = (tasks: Task[]): number => {
  const priorities = tasks.map((task) => task.priority);
  const priorityValues = {
    low: 1,
    medium: 2,
    high: 3,
  };
  const totalPriorityValue = priorities.reduce((acc, curr) => acc + priorityValues[curr], 0);
  return totalPriorityValue / priorities.length;
};

// Define a function to get the task with the highest priority
const getTaskWithHighestPriority = (tasks: Task[]): Task | undefined => {
  return tasks.reduce((acc, curr) => (acc.priority > curr.priority ? acc : curr), undefined);
};

// Define a function to get the task with the lowest priority
const getTaskWithLowestPriority = (tasks: Task[]): Task | undefined => {
  return tasks.reduce((acc, curr) => (acc.priority < curr.priority ? acc : curr), undefined);
};

// Define a function to get the task due the soonest
const getTaskDueTheSoonest = (tasks: Task[]): Task | undefined => {
  return tasks.reduce((acc, curr) => (acc.dueDate < curr.dueDate ? acc : curr), undefined);
};

// Define a function to get the task due the latest
const getTaskDueTheLatest = (tasks: Task[]): Task | undefined => {