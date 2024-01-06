```sql
WITH RecursiveCTE AS (
    -- Base case: Find the first level of employees reporting to the CEO
    SELECT
        E.EmployeeID,
        E.FirstName,
        E.LastName,
        E.ManagerID,
        1 AS Level
    FROM
        Employees AS E
    WHERE
        E.ManagerID IS NULL

    UNION ALL

    -- Recursive case: Find the employees reporting to employees in the current level
    SELECT
        E.EmployeeID,
        E.FirstName,
        E.LastName,
        E.ManagerID,
        Level + 1 AS Level
    FROM
        Employees AS E
    JOIN
        RecursiveCTE AS RC
    ON
        E.ManagerID = RC.EmployeeID
)

-- Select the EmployeeID, FirstName, LastName, ManagerID, and Level for all employees in the RecursiveCTE
SELECT
    EmployeeID,
    FirstName,
    LastName,
    ManagerID,
    Level
FROM
    RecursiveCTE;
```

This code is a recursive common table expression (CTE) that finds the employees in a company who report to each other, starting from the CEO and going down the organization chart. The CTE uses a recursive query to find all the employees who report to a given employee, and then adds them to the CTE. The query then selects the EmployeeID, FirstName, LastName, ManagerID, and Level for all the employees in the CTE.

Here is a step-by-step explanation of the code:

* **Base case:** The base case of the recursive query is the first line of the WITH clause. This line selects the employees who report directly to the CEO (i.e., those with a ManagerID of NULL). The Level column is set to 1 for these employees because they are one level below the CEO.
* **Recursive case:** The recursive case of the recursive query is the second line of the WITH clause. This line selects the employees who report to employees in the current level of the CTE. The Level column is incremented by 1 for these employees because they are one level below the employees in the current level.
* **SELECT statement:** The SELECT statement at the end of the query selects the EmployeeID, FirstName, LastName, ManagerID, and Level for all the employees in the CTE.

This code can be used to generate an organization chart, find the employees who report to a given employee, or find the longest chain of command in a company.