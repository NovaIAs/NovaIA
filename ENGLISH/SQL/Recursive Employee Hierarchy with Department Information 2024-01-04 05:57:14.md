```sql
WITH RecursiveEmployeeHierarchy AS (
    SELECT
        e.EmployeeID,
        e.FirstName,
        e.LastName,
        e.JobTitle,
        e.ManagerID,
        e.DepartmentID,
        NULL AS ParentEmployeeID
    FROM
        Employees e
    WHERE
        e.ManagerID IS NULL
    UNION ALL
    SELECT
        e.EmployeeID,
        e.FirstName,
        e.LastName,
        e.JobTitle,
        e.ManagerID,
        e.DepartmentID,
        reh.EmployeeID AS ParentEmployeeID
    FROM
        Employees e
    JOIN
        RecursiveEmployeeHierarchy reh ON e.ManagerID = reh.EmployeeID
)

SELECT
    reh.EmployeeID,
    reh.FirstName,
    reh.LastName,
    reh.JobTitle,
    reh.ManagerID,
    reh.DepartmentID,
    reh.ParentEmployeeID,
    (
        SELECT
            GROUP_CONCAT(d.DepartmentName, ' > ')
        FROM
            Departments d
        WHERE
            d.DepartmentID IN (
                SELECT
                    DepartmentID
                FROM
                    RecursiveEmployeeHierarchy
                WHERE
                    EmployeeID = reh.EmployeeID
            )
    ) AS DepartmentHierarchy
FROM
    RecursiveEmployeeHierarchy reh
ORDER BY
    reh.EmployeeID;
```

Explanation:

This SQL code creates a recursive CTE (Common Table Expression) called RecursiveEmployeeHierarchy to generate a hierarchical representation of employees in an organization based on their manager-subordinate relationships. Here's how it works:

1. Base Case:

   - The CTE starts with a base case that selects employees who have no managers (i.e., they are at the top level of the hierarchy). These employees are identified by having a NULL value in the ManagerID column.
   - For these top-level employees, the ParentEmployeeID column is set to NULL.

2. Recursive Step:

   - The recursive part of the CTE uses a UNION ALL operation to combine the results of the base case with the results of the recursive step itself. This allows us to traverse down the hierarchy levels.
   - It selects employees and their information, including their ManagerID and DepartmentID.
   - It joins the current employee's ManagerID with the EmployeeID of the parent employee from the previous level of the hierarchy.
   - The ParentEmployeeID column is set to the EmployeeID of the parent employee.

3. Recursive Calls:

   - The recursive step continues to call itself until all levels of the hierarchy have been traversed. This results in a hierarchical representation of employees with their managers and subordinates.

4. Final Result:

   - The final SELECT statement retrieves various employee details along with their hierarchical information, including the EmployeeID, name, job title, manager ID, department ID, parent employee ID, and the department hierarchy (a list of all departments the employee belongs to).

5. Ordering:

   - The results are ordered by the EmployeeID to ensure a consistent ordering of the employees.

This code provides a comprehensive and detailed hierarchical representation of employees in the organization, allowing you to analyze and visualize employee relationships, organizational structure, and departmental information.