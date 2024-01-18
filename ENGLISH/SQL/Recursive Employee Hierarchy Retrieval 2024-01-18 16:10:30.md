```sql
WITH RecursiveEmployeeHierarchy AS (
    -- Base case: Select employees with no managers.
    SELECT EmployeeID, EmployeeName, ManagerID, 0 AS Level
    FROM Employees
    WHERE ManagerID IS NULL

    -- Recursive case: Select employees and their managers recursively.
    UNION ALL
    SELECT E.EmployeeID, E.EmployeeName, E.ManagerID, REH.Level + 1 AS Level
    FROM Employees AS E
    JOIN RecursiveEmployeeHierarchy AS REH
    ON E.ManagerID = REH.EmployeeID
)

-- Select employee information with hierarchical level.
SELECT REH.EmployeeID, REH.EmployeeName, REH.ManagerID, REH.Level
FROM RecursiveEmployeeHierarchy AS REH;
```

Explanation:

This SQL code retrieves employee information from a database and calculates the hierarchical level of each employee based on their manager-subordinate relationships. Here's how it works:

1. RecursiveEmployeeHierarchy Common Table Expression (CTE):
   - The CTE named RecursiveEmployeeHierarchy is defined to calculate the hierarchical level of employees recursively.

2. Base Case:
   - The base case of the CTE selects employees who have no managers. These employees are at the top level of the hierarchy, so their level is 0.

3. Recursive Case:
   - The recursive case of the CTE selects employees and their managers recursively. It uses a LEFT JOIN between the Employees table (aliased as E) and the RecursiveEmployeeHierarchy CTE (aliased as REH).
   - The join condition E.ManagerID = REH.EmployeeID ensures that each employee is joined with their manager.
   - The expression REH.Level + 1 AS Level calculates the hierarchical level of the employee by adding 1 to the level of their manager. This accumulates the levels as we move down the hierarchy.

4. Final Result:
   - The final result is obtained by selecting specific columns from the RecursiveEmployeeHierarchy CTE:
     - EmployeeID: Unique identifier for each employee.
     - EmployeeName: Name of the employee.
     - ManagerID: Unique identifier of the employee's manager. If an employee has no manager, this column will be NULL.
     - Level: The hierarchical level of the employee in the organization.

This code allows you to efficiently calculate the hierarchical structure of employees in a database and retrieve their information along with their hierarchical levels. It uses a recursive CTE to traverse the employee hierarchy and determine the level of each employee relative to the top-level managers.