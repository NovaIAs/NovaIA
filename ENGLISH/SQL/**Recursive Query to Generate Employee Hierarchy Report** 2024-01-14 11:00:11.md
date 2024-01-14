```sql
WITH RecursiveCTE AS (
    SELECT *, 0 AS Level
    FROM Employees
    WHERE ManagerID IS NULL

    UNION ALL

    SELECT e.*, rc.Level + 1
    FROM Employees e
    JOIN RecursiveCTE rc ON e.ManagerID = rc.EmployeeID
)

SELECT
    e.EmployeeID,
    e.FirstName,
    e.LastName,
    e.JobTitle,
    rc.Level AS ManagementLevel
FROM
    RecursiveCTE rc
JOIN
    Employees e ON rc.EmployeeID = e.EmployeeID
ORDER BY
    rc.Level DESC, e.LastName ASC;
```

Explanation:

* The RecursiveCTE common table expression (CTE) is used to perform a recursive query that traverses the employee hierarchy. It starts with the top-level employees (those with no manager) and then recursively includes their subordinates at each level. The Level column keeps track of the level of each employee in the hierarchy.
* The main query then joins the RecursiveCTE with the Employees table to retrieve the employee details and their management level.
* The results are ordered by management level in descending order (with top-level managers first) and then by last name in ascending order.

This code can be used to generate a hierarchical report of employees, showing their management level and other details. It can be useful for analyzing the organizational structure of a company or for identifying employees at different levels for various purposes such as promotions, training, or performance reviews.