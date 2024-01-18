```sql
WITH RecursiveEmployeeHierarchy AS (
    SELECT
        e.EmployeeID,
        e.FirstName,
        e.LastName,
        e.ManagerID,
        1 AS Level
    FROM
        Employees e
    WHERE
        e.ManagerID IS NULL
    UNION ALL
    SELECT
        e.EmployeeID,
        e.FirstName,
        e.LastName,
        e.ManagerID,
        reh.Level + 1 AS Level
    FROM
        Employees e
    JOIN
        RecursiveEmployeeHierarchy reh ON e.ManagerID = reh.EmployeeID
)

SELECT
    reh.EmployeeID,
    reh.FirstName,
    reh.LastName,
    reh.Level,
    GROUP_CONCAT(DISTINCT dep.DepartmentName) AS DepartmentNames
FROM
    RecursiveEmployeeHierarchy reh
JOIN
    EmployeeDepartments ed ON reh.EmployeeID = ed.EmployeeID
JOIN
    Departments dep ON ed.DepartmentID = dep.DepartmentID
GROUP BY
    reh.EmployeeID,
    reh.FirstName,
    reh.LastName,
    reh.Level
ORDER BY
    reh.Level,
    reh.EmployeeID;
```

Explanation:

1. Recursive CTE (Common Table Expression):
   - We start with a recursive CTE called RecursiveEmployeeHierarchy.
   - It initially selects employees who have no manager (i.e., they are at the top of the hierarchy).
   - It then recursively adds employees at each level by joining with itself and selecting employees whose ManagerID matches the EmployeeID of the previously selected employees.
   - The Level column keeps track of the level of each employee in the hierarchy.

2. Main Query:
   - The main query selects columns from the RecursiveEmployeeHierarchy CTE, including EmployeeID, FirstName, LastName, Level, and DepartmentNames.
   - It joins the RecursiveEmployeeHierarchy with EmployeeDepartments (ed) and Departments (dep) tables to retrieve the department names associated with each employee.
   - It then groups the results by EmployeeID, FirstName, LastName, and Level to aggregate the department names for each employee.
   - Finally, it orders the results by Level and EmployeeID to display the employee hierarchy with department names.

This comprehensive SQL code efficiently retrieves and displays employee information, including their names, hierarchy levels, and associated department names. It utilizes a recursive CTE to traverse the employee hierarchy and joins multiple tables to gather the necessary data. The resulting output provides a structured view of the employee hierarchy, allowing for analysis and reporting on employee-related information.