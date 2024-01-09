```javascript
(function () {
  // Define a function to generate a random integer between two values (inclusive)
  const getRandomInt = (min, max) => Math.floor(Math.random() * (max - min + 1)) + min;

  // Define a function to shuffle an array
  const shuffleArray = (array) => {
    for (let i = array.length - 1; i > 0; i--) {
      const j = getRandomInt(0, i);
      [array[i], array[j]] = [array[j], array[i]];
    }
  };

  // Define a function to generate a unique ID
  const generateUniqueId = () => {
    const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    let id = '';
    for (let i = 0; i < 10; i++) {
      id += characters[getRandomInt(0, characters.length - 1)];
    }
    return id;
  };

  // Define a function to create a new todo item
  const createTodoItem = (title, description, dueDate, priority) => {
    return {
      id: generateUniqueId(),
      title,
      description,
      dueDate,
      priority,
      completed: false,
    };
  };

  // Define a function to add a todo item to the list
  const addTodoItem = (todoList, todoItem) => {
    todoList.push(todoItem);
    return todoList;
  };

  // Define a function to remove a todo item from the list
  const removeTodoItem = (todoList, todoId) => {
    const index = todoList.findIndex((todoItem) => todoItem.id === todoId);
    if (index !== -1) {
      todoList.splice(index, 1);
    }
    return todoList;
  };

  // Define a function to mark a todo item as completed
  const markTodoItemAsCompleted = (todoList, todoId) => {
    const todoItem = todoList.find((todoItem) => todoItem.id === todoId);
    if (todoItem) {
      todoItem.completed = true;
    }
    return todoList;
  };

  // Define a function to get all the todo items that are due today
  const getTodaysTodos = (todoList) => {
    const todaysDate = new Date();
    return todoList.filter((todoItem) => {
      const dueDate = new Date(todoItem.dueDate);
      return dueDate.getDate() === todaysDate.getDate() &&
        dueDate.getMonth() === todaysDate.getMonth() &&
        dueDate.getFullYear() === todaysDate.getFullYear();
    });
  };

  // Define a function to get all the todo items that are overdue
  const getOverdueTodos = (todoList) => {
    const todaysDate = new Date();
    return todoList.filter((todoItem) => {
      const dueDate = new Date(todoItem.dueDate);
      return dueDate < todaysDate;
    });
  };

  // Define a function to get all the todo items that are high priority
  const getHighPriorityTodos = (todoList) => {
    return todoList.filter((todoItem) => todoItem.priority === 'high');
  };

  // Define a function to sort the todo list by due date
  const sortByDueDate = (todoList) => {
    todoList.sort((a, b) => {
      const dateA = new Date(a.dueDate);
      const dateB = new Date(b.dueDate);
      return dateA - dateB;
    });
    return todoList;
  };

  // Define a function to sort the todo list by priority
  const sortByPriority = (todoList) => {
    todoList.sort((a, b) => {
      return a.priority.localeCompare(b.priority);
    });
    return todoList;
  };

  // Create a sample todo list
  const todoList = [
    createTodoItem('Learn JavaScript', 'Go through the JavaScript tutorial', '2023-03-08', 'high'),
    createTodoItem('Finish project', 'Complete the project by the end of the week', '2023-03-10', 'medium'),
    createTodoItem('Buy groceries', 'Get milk, eggs, and bread', '2023-03-12', 'low'),
    createTodoItem('Go to the gym', 'Do a full body workout', '2023-03-14', 'medium'),
    createTodoItem('Pay bills', 'Pay the electricity and water bills', '2023-03-16', 'high'),
  ];

  // Shuffle the todo list
  shuffleArray(todoList);

  // Print the todo list
  console.log('Todo List:');
  todoList.forEach((todoItem) => {
    console.log(`- ${todoItem.title} (${todoItem.priority}) - Due: ${todoItem.dueDate}`);
  });

  // Add a new todo item to the list
  const newTodoItem = createTodoItem('Clean the house', 'Sweep, mop, and vacuum the floors', '2023-03-20', 'low');
  addTodoItem(todoList, newTodoItem);

  // Print the updated todo list
  console.log('\nUpdated Todo List:');
  todoList.forEach((todoItem) => {
    console.log(`- ${todoItem.title} (${todoItem.priority}) - Due: ${todoItem.dueDate}`);
  });

  // Remove a todo item from the list
  removeTodoItem(todoList, '2');

  // Print the updated todo list
  console.log('\nUpdated Todo List:');
  todoList.forEach((todoItem) => {
    console.log(`- ${todoItem.title} (${todoItem.priority}) - Due: ${todoItem.dueDate}`);
  });

  // Mark a todo item as completed
  markTodoItemAsCompleted(todoList, '1');

  // Print the updated todo list
  console.log('\nUpdated Todo List:');
  todoList.forEach((todoItem) => {
    console.log(`- ${todoItem.title} (${todoItem.priority}) - Due: ${todoItem.dueDate} - Completed: ${todoItem.completed}`);
  });

  // Get all the todo items that are due today
  const todaysTodos = getTodaysTodos(todoList);

  // Print the todo items that are due today
  console.log('\nToday\'s Todos:');
  todaysTodos.forEach((todoItem) => {
    console.log(`- ${todoItem.title} (${todoItem.priority}) - Due: ${todoItem.dueDate}`);
  });

  // Get all the todo items that are overdue
  const overdueTodos = getOverdueTodos(todoList);

  // Print the todo items that are overdue
  console.log('\nOverdue Todos:');
  overdueTodos.forEach((todoItem) => {
    console.log(`- ${todoItem.title} (${todoItem.priority}) - Due: ${todoItem.dueDate}`);
  });

  // Get all the todo items that are high priority
  const highPriorityTodos = getHighPriorityTodos(todoList);

  // Print the todo items that are high priority
  console.log('\nHigh Priority Todos:');
  highPriorityTodos.forEach((todoItem) => {
    console.log(`- ${todoItem.title} (${todoItem.priority}) - Due: ${todoItem.dueDate}`);
  });

  // Sort the todo list by due date
  const sortedByDueDate = sortByDueDate(todoList);

  // Print the sorted todo list
  console.log('\nSorted Todo List by Due Date:');
  sortedByDueDate.forEach((todoItem) => {
    console.log(`- ${todoItem.title} (${todoItem.priority}) - Due: ${todoItem.dueDate}`);
  });

  // Sort the todo list by priority
  const sortedByPriority = sortByPriority(todoList);

  // Print the sorted todo list
  console.log('\nSorted Todo List by Priority:');
  sortedByPriority.forEach((todoItem) => {
    console.log(`- ${todoItem.title} (${todoItem.priority}) - Due: ${todoItem.dueDate}`);
  });
})();
```

**Explanation:**

This code is a comprehensive JavaScript program that demonstrates various operations on a todo list. It includes functions to generate random integers, shuffle arrays, generate unique IDs, create todo items, add, remove, and mark todo items as completed, get todo items based on due date, priority, and overdue status, and sort the todo list by due date and priority.

The code starts by defining helper functions like `getRandomInt`, `shuffleArray`, and `generateUniqueId`. These functions are used in other parts of the code.

Next, it defines a function `createTodoItem`