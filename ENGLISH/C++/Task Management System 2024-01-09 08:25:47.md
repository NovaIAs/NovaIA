```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <string>

using namespace std;

// Define a custom data structure to store information about each task
struct Task {
    int id;
    string name;
    int priority;
    int duration;
    bool completed;

    Task(int id, string name, int priority, int duration) {
        this->id = id;
        this->name = name;
        this->priority = priority;
        this->duration = duration;
        this->completed = false;
    }

    // Define a comparison function to sort tasks by priority and duration
    bool operator<(const Task& other) const {
        if (this->priority == other.priority) {
            return this->duration < other.duration;
        } else {
            return this->priority > other.priority;
        }
    }
};

// Define a function to print the details of a task
void printTask(const Task& task) {
    cout << "Task ID: " << task.id << endl;
    cout << "Task Name: " << task.name << endl;
    cout << "Task Priority: " << task.priority << endl;
    cout << "Task Duration: " << task.duration << endl;
    cout << "Task Completed: " << (task.completed ? "Yes" : "No") << endl;
    cout << endl;
}

// Define a function to find the task with the highest priority
Task findHighestPriorityTask(const vector<Task>& tasks) {
    return *max_element(tasks.begin(), tasks.end());
}

// Define a function to find all tasks with a specific priority
vector<Task> findTasksWithPriority(const vector<Task>& tasks, int priority) {
    vector<Task> result;
    for (const Task& task : tasks) {
        if (task.priority == priority) {
            result.push_back(task);
        }
    }
    return result;
}

// Define a function to mark a task as completed
void markTaskCompleted(Task& task) {
    task.completed = true;
}

// Define a function to sort tasks by duration
void sortByDuration(vector<Task>& tasks) {
    sort(tasks.begin(), tasks.end(), [](const Task& a, const Task& b) {
        return a.duration < b.duration;
    });
}

// Define a function to find the longest task
Task findLongestTask(const vector<Task>& tasks) {
    return *max_element(tasks.begin(), tasks.end(), [](const Task& a, const Task& b) {
        return a.duration < b.duration;
    });
}

// Define a function to find the shortest task
Task findShortestTask(const vector<Task>& tasks) {
    return *min_element(tasks.begin(), tasks.end(), [](const Task& a, const Task& b) {
        return a.duration < b.duration;
    });
}

// Define a function to find all completed tasks
vector<Task> findCompletedTasks(const vector<Task>& tasks) {
    vector<Task> result;
    for (const Task& task : tasks) {
        if (task.completed) {
            result.push_back(task);
        }
    }
    return result;
}

// Define a function to find all incomplete tasks
vector<Task> findIncompleteTasks(const vector<Task>& tasks) {
    vector<Task> result;
    for (const Task& task : tasks) {
        if (!task.completed) {
            result.push_back(task);
        }
    }
    return result;
}

// Define a function to group tasks by priority
map<int, vector<Task>> groupTasksByPriority(const vector<Task>& tasks) {
    map<int, vector<Task>> result;
    for (const Task& task : tasks) {
        result[task.priority].push_back(task);
    }
    return result;
}

// Define a function to group tasks by duration
map<int, vector<Task>> groupTasksByDuration(const vector<Task>& tasks) {
    map<int, vector<Task>> result;
    for (const Task& task : tasks) {
        result[task.duration].push_back(task);
    }
    return result;
}

// Define a function to print the details of all tasks
void printAllTasks(const vector<Task>& tasks) {
    for (const Task& task : tasks) {
        printTask(task);
    }
}

int main() {
    // Create a vector of tasks
    vector<Task> tasks = {
        Task(1, "Task 1", 5, 3),
        Task(2, "Task 2", 3, 2),
        Task(3, "Task 3", 4, 5),
        Task(4, "Task 4", 2, 4),
        Task(5, "Task 5", 1, 1)
    };

    // Print the details of all tasks
    cout << "All Tasks:" << endl;
    printAllTasks(tasks);

    // Find the task with the highest priority
    Task highestPriorityTask = findHighestPriorityTask(tasks);
    cout << "Task with Highest Priority:" << endl;
    printTask(highestPriorityTask);

    // Find all tasks with a specific priority
    vector<Task> tasksWithPriority3 = findTasksWithPriority(tasks, 3);
    cout << "Tasks with Priority 3:" << endl;
    printAllTasks(tasksWithPriority3);

    // Mark a task as completed
    markTaskCompleted(tasks[2]);
    cout << "Task 3 marked as completed." << endl;

    // Sort tasks by duration
    sortByDuration(tasks);
    cout << "Tasks sorted by Duration:" << endl;
    printAllTasks(tasks);

    // Find the longest task
    Task longestTask = findLongestTask(tasks);
    cout << "Longest Task:" << endl;
    printTask(longestTask);

    // Find the shortest task
    Task shortestTask = findShortestTask(tasks);
    cout << "Shortest Task:" << endl;
    printTask(shortestTask);

    // Find all completed tasks
    vector<Task> completedTasks = findCompletedTasks(tasks);
    cout << "Completed Tasks:" << endl;
    printAllTasks(completedTasks);

    // Find all incomplete tasks
    vector<Task> incompleteTasks = findIncompleteTasks(tasks);
    cout << "Incomplete Tasks:" << endl;
    printAllTasks(incompleteTasks);

    // Group tasks by priority
    map<int, vector<Task>> tasksByPriority = groupTasksByPriority(tasks);
    cout << "Tasks grouped by Priority:" << endl;
    for (const auto& [priority, tasks] : tasksByPriority) {
        cout << "Priority " << priority << ":" << endl;
        printAllTasks(tasks);
    }

    // Group tasks by duration
    map<int, vector<Task>> tasksByDuration = groupTasksByDuration(tasks);
    cout << "Tasks grouped by Duration:" << endl;
    for (const auto& [duration, tasks] : tasksByDuration) {
        cout << "Duration " << duration << ":" << endl;
        printAllTasks(tasks);
    }

    return 0;
}
```

This code demonstrates various operations on a collection of tasks, including finding the task with the highest priority, finding all tasks with a specific priority, marking a task as completed, sorting tasks by duration, finding the longest and shortest tasks, finding all completed and incomplete tasks, grouping tasks by priority and duration, and printing the details of all tasks. It showcases the use of STL containers, algorithms, and lambda expressions to manipulate and organize the task data.