```sql
WITH RecursiveHierarchy AS (
    SELECT
        EmployeeID,
        EmployeeName,
        ManagerID,
        CAST(EmployeeID AS VARCHAR(MAX)) AS Path
    FROM Employees
    WHERE ManagerID IS NULL

    UNION ALL

    SELECT
        Employees.EmployeeID,
        Employees.EmployeeName,
        Employees.ManagerID,
        Path || '/' || CAST(Employees.EmployeeID AS VARCHAR(MAX))
    FROM Employees
    JOIN RecursiveHierarchy ON Employees.ManagerID = RecursiveHierarchy.EmployeeID
)

SELECT
    EmployeeID,
    EmployeeName,
    ManagerID,
    Path,

    -- Calculate the employee's level in the hierarchy
    CAST(LENGTH(Path) - LENGTH(REPLACE(Path, '/', '')) AS INTEGER) AS Level,

    -- Calculate the employee's descendants count
    (SELECT COUNT(*) FROM RecursiveHierarchy WHERE Path LIKE Path || '/%') AS DescendantsCount

FROM RecursiveHierarchy;
```

Explanation:

1. **Recursive CTE (RecursiveHierarchy)**:
   - We create a recursive common table expression (CTE) called RecursiveHierarchy to traverse the employee hierarchy.
   - The CTE starts with employees who have no managers (i.e., they are at the top of the hierarchy).
   - It then recursively adds employees who report to the employees already included in the CTE.
   - The Path column keeps track of the path from the root employee to the current employee, separated by forward slashes '/'.

2. **Main Query**:
   - We select various columns from the RecursiveHierarchy CTE, including EmployeeID, EmployeeName, ManagerID, and Path.
   - We also calculate the employee's level in the hierarchy (Level) by counting the number of forward slashes in the Path column.
   - Finally, we calculate the employee's descendants count (DescendantsCount) by finding all employees whose Path starts with the current employee's Path followed by a forward slash '/'.

This code effectively calculates the employee hierarchy, including each employee's level and descendants count, in a single query. It demonstrates the power of recursive CTEs in expressing complex hierarchical relationships in SQL.