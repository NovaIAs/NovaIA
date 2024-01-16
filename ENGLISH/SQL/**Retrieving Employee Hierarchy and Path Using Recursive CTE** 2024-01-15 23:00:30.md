```sql
-- Create a Recursive Common Table Expression (CTE) called EmployeeHierarchy
WITH EmployeeHierarchy(EmployeeID, ManagerID, Level, Path) AS (
    -- Base case: Select employees who have no manager (Level 1)
    SELECT EmployeeID, ManagerID, 1 AS Level, EmployeeID AS Path
    FROM Employees
    WHERE ManagerID IS NULL
    UNION ALL
    -- Recursive case: Select employees and their managers, incrementing the Level and appending ManagerID to the Path
    SELECT e.EmployeeID, e.ManagerID, eh.Level + 1, eh.Path || '/' || e.EmployeeID
    FROM Employees e
    JOIN EmployeeHierarchy eh ON e.ManagerID = eh.EmployeeID
)

-- Select EmployeeID, Name, Level, and Path from the EmployeeHierarchy CTE
SELECT EmployeeID, Name, Level, Path
FROM Employees
JOIN EmployeeHierarchy ON Employees.EmployeeID = EmployeeHierarchy.EmployeeID;
```

Explanation:

1. We create a CTE named EmployeeHierarchy using the WITH clause. This CTE will be used to calculate the employee hierarchy.

2. The base case of the CTE is a subquery that selects employees who have no manager (Level 1). The EmployeeID, ManagerID, Level, and Path columns are specified for each employee.

3. The recursive case of the CTE is another subquery that selects employees and their managers. The Level is incremented by 1, and the ManagerID is appended to the Path using the '||' operator.

4. We use a JOIN operation to combine the Employees table with the EmployeeHierarchy CTE on the EmployeeID column. This allows us to retrieve employee information along with their hierarchy data.

5. Finally, we use a SELECT statement to retrieve the EmployeeID, Name, Level, and Path columns from the joined result set.