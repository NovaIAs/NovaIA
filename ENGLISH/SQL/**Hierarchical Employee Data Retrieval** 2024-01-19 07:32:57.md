```sql
-- Create a common table expression (CTE) called EmployeeHierarchy
WITH EmployeeHierarchy AS (
    -- Select the employee's ID, manager ID, and name
    SELECT EmployeeID, ManagerID, EmployeeName
    FROM Employees
),

-- Create a CTE called EmployeeLevels
EmployeeLevels AS (
    -- Select the employee's ID, manager ID, name, and level
    SELECT EmployeeHierarchy.EmployeeID, EmployeeHierarchy.ManagerID, EmployeeHierarchy.EmployeeName, 0 AS Level
    FROM EmployeeHierarchy
    WHERE EmployeeHierarchy.ManagerID IS NULL
    
    UNION ALL
    
    -- Recursive query to find employees at lower levels
    SELECT EmployeeHierarchy.EmployeeID, EmployeeHierarchy.ManagerID, EmployeeHierarchy.EmployeeName, EmployeeLevels.Level + 1 AS Level
    FROM EmployeeHierarchy
    JOIN EmployeeLevels ON EmployeeHierarchy.ManagerID = EmployeeLevels.EmployeeID
),

-- Create a CTE called EmployeeDetails
EmployeeDetails AS (
    -- Select the employee's ID, name, level, and department name
    SELECT EmployeeLevels.EmployeeID, EmployeeLevels.EmployeeName, EmployeeLevels.Level, Departments.DepartmentName
    FROM EmployeeLevels
    JOIN Departments ON EmployeeLevels.EmployeeID = Departments.DepartmentID
)

-- Select the employee's ID, name, level, and department name from the EmployeeDetails CTE
SELECT EmployeeDetails.EmployeeID, EmployeeDetails.EmployeeName, EmployeeDetails.Level, EmployeeDetails.DepartmentName
FROM EmployeeDetails;
```

Explanation:

1. **EmployeeHierarchy CTE:** This CTE retrieves the employee ID, manager ID, and name from the Employees table.

2. **EmployeeLevels CTE:** This CTE uses a recursive query to assign levels to employees based on their management hierarchy. Employees with no manager are assigned level 0. For employees with managers, their level is one more than their manager's level.

3. **EmployeeDetails CTE:** This CTE joins the EmployeeLevels CTE with the Departments table to retrieve the employee's department name.

4. The final query selects the employee's ID, name, level, and department name from the EmployeeDetails CTE.

This complex SQL code demonstrates the use of CTEs and recursive queries to efficiently retrieve hierarchical data from multiple tables. It can be used to generate organizational charts, calculate reporting structures, or perform other analyses based on employee relationships.