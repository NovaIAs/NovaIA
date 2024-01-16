```sql
-- Create a common table expression (CTE) called EmployeeHierarchy
WITH EmployeeHierarchy AS (
    SELECT
        e.EmployeeID,
        e.FirstName,
        e.LastName,
        e.ManagerID,
        e.DepartmentID,
        d.DepartmentName,
        -- Calculate the level of each employee in the hierarchy
        COALESCE(
            (
                SELECT COUNT(*)
                FROM EmployeeHierarchy AS eh
                WHERE eh.EmployeeID = e.ManagerID
            ),
            0
        ) AS Level
    FROM
        Employee AS e
    JOIN
        Department AS d ON e.DepartmentID = d.DepartmentID
)

-- Select employee information and hierarchy details
SELECT
    eh.EmployeeID,
    eh.FirstName,
    eh.LastName,
    eh.ManagerID,
    eh.DepartmentID,
    eh.DepartmentName,
    eh.Level

-- From the EmployeeHierarchy CTE
FROM
    EmployeeHierarchy AS eh

-- Filter the results to include only employees at or below a specified level
WHERE
    eh.Level <= 3

-- Order the results by the employee's level and then by their employee ID
ORDER BY
    eh.Level, eh.EmployeeID;
```

Explanation:

1. **Common Table Expression (CTE) EmployeeHierarchy:**
   - This CTE retrieves employee information, including employee ID, first name, last name, manager ID, department ID, department name, and calculates the level of each employee in the hierarchy.

2. **Main Query:**
   - Selects the employee information and hierarchy details from the EmployeeHierarchy CTE.

3. **WHERE Clause:**
   - Filters the results to include only employees at or below a specified level. In this example, we are selecting employees at Level 3 or below.

4. **ORDER BY Clause:**
   - Orders the results by the employee's level (ascending order) and then by their employee ID (ascending order).

This query provides a hierarchically organized list of employees, displaying their information and their position in the company's organizational structure. It allows you to easily identify employees at specific levels and view their details along with their department information.