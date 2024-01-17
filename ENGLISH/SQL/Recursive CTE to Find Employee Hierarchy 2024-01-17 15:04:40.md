```sql
WITH RecursiveCTE AS (
    SELECT
        employee_id,
        manager_id,
        employee_name,
        level
    FROM
        employees
    WHERE
        level = 1
    UNION ALL
    SELECT
        e.employee_id,
        e.manager_id,
        e.employee_name,
        rc.level + 1
    FROM
        employees e
    JOIN
        RecursiveCTE rc ON e.manager_id = rc.employee_id
)

SELECT
    employee_id,
    manager_id,
    employee_name,
    level
FROM
    RecursiveCTE
ORDER BY
    level, employee_id;
```

Explanation:

- The above code is a recursive common table expression (CTE) that finds the hierarchy of employees in a company based on their manager-subordinate relationships stored in the `employees` table. The CTE is named `RecursiveCTE`.

- The `employees` table is assumed to have the following columns: `employee_id`, `manager_id`, `employee_name`, and `level`. `level` represents the level of an employee in the hierarchy, with level 1 being the top level (e.g., CEO).

- The CTE starts with a base case query that selects employees who are at level 1 (i.e., those without managers). These employees are the root nodes of the hierarchy.

- The recursive part of the CTE uses a `UNION ALL` to join the results of the base case query with the results of a recursive query. The recursive query finds all employees who have a manager in the `employees` table. It increments the `level` column by 1 for these employees, indicating their level in the hierarchy.

- The `ORDER BY` clause in the final `SELECT` statement sorts the results by `level` and `employee_id`. This ordering ensures that the employees are displayed in a hierarchical format, with the top-level employees appearing first, followed by their direct reports, and so on.

- The final result is a hierarchical representation of the employee data, where each employee's level in the hierarchy is correctly displayed.