```sql
WITH CTE_Hierarchy AS (
    SELECT
        id,
        name,
        parent_id,
        LEVEL AS level
    FROM
        tbl_Hierarchy
    WHERE
        parent_id IS NULL

    UNION ALL

    SELECT
        h.id,
        h.name,
        h.parent_id,
        CTE_Hierarchy.level + 1 AS level
    FROM
        tbl_Hierarchy AS h
    JOIN
        CTE_Hierarchy ON h.parent_id = CTE_Hierarchy.id
),

CTE_Employees AS (
    SELECT
        e.id,
        e.first_name,
        e.last_name,
        CTE_Hierarchy.name AS department
    FROM
        tbl_Employees AS e
    JOIN
        CTE_Hierarchy ON e.department_id = CTE_Hierarchy.id
)

SELECT
    e.id AS EmployeeID,
    e.first_name AS FirstName,
    e.last_name AS LastName,
    d.name AS Department,
    d.level AS DepartmentLevel

FROM
    CTE_Employees AS e
JOIN
    CTE_Hierarchy AS d ON e.department = d.name

ORDER BY
    d.level, e.id;
```

Explanation:

This SQL code consists of two Common Table Expressions (CTEs) and a final SELECT statement to retrieve employee information along with their department details. Let's break down each part:

CTE_Hierarchy:

SELECT:
- id: Unique identifier for each record in the tbl_Hierarchy table.
- name: Name of the department.
- parent_id: Id of the parent department.
- LEVEL: Introduced by Oracle's hierarchical query extension, LEVEL represents the level of each node in the hierarchy. It starts from 1 for the root node.

WHERE:
- parent_id IS NULL: This condition selects only the root nodes (departments with no parent).

UNION ALL:

- This is a recursive part of the CTE. It joins CTE_Hierarchy with tbl_Hierarchy using parent_id to find child departments.
- It increments the LEVEL column by 1 for each child department, effectively depicting the hierarchy levels.

CTE_Employees:

SELECT:
- e.id: Unique identifier for each employee.
- e.first_name: First name of the employee.
- e.last_name: Last name of the employee.
- CTE_Hierarchy.name: Name of the department where the employee works.

FROM:
- tbl_Employees AS e: Alias for the tbl_Employees table.
- CTE_Hierarchy: Joined with tbl_Employees using the department_id column to link employees to their respective departments.

Final SELECT Statement:

SELECT:
- e.id AS EmployeeID: Employee's unique identifier.
- e.first_name AS FirstName: Employee's first name.
- e.last_name AS LastName: Employee's last name.
- d.name AS Department: Name of the employee's department.
- d.level AS DepartmentLevel: Level of the department in the hierarchy.

FROM:
- CTE_Employees AS e: Alias for the CTE created earlier.
- CTE_Hierarchy AS d: Alias for the CTE created earlier.

JOIN:
- e.department = d.name: Joining employees with their respective departments.

ORDER BY:
- d.level: Sorting the results by department level, starting from the highest level.
- e.id: Sorting employees within each department by their IDs.

This complex SQL code demonstrates hierarchical data retrieval and manipulation. It showcases how to extract employee information along with department details, including the department level within the hierarchy. The result will display employee data organized by department hierarchy, making it easy to visualize the organizational structure and employee details within each department.