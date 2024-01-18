```sql
WITH RecursiveHierarchy AS (
    SELECT
        employee_id,
        employee_name,
        manager_id,
        0 AS level
    FROM
        employees
    WHERE
        manager_id IS NULL

    UNION ALL

    SELECT
        e.employee_id,
        e.employee_name,
        e.manager_id,
        rh.level + 1 AS level
    FROM
        employees e
    JOIN
        RecursiveHierarchy rh ON e.manager_id = rh.employee_id
)

SELECT
    employee_id,
    employee_name,
    level
FROM
    RecursiveHierarchy
ORDER BY
    level ASC,
    employee_id ASC;
```

Explanation:

* The RecursiveHierarchy CTE (Common Table Expression) is used to calculate the hierarchical level of each employee in the organization.
* The base case of the recursion is to select all employees who have a NULL manager_id, which means they are the top-level employees.
* The recursive step of the recursion is to join the employees table with the RecursiveHierarchy CTE on the manager_id column. This allows us to find the level of each employee based on the level of their manager.
* The final SELECT statement retrieves the employee_id, employee_name, and level for all employees, ordered by level and employee_id.