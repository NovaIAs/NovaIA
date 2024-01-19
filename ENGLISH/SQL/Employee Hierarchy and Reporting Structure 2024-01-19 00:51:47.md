```sql
WITH RecursiveOrganizationHierarchy AS (
    SELECT
        o.OrganizationID,
        o.OrganizationName,
        o.ParentOrganizationID,
        RANK() OVER (ORDER BY o.OrganizationID) AS TreeLevel,
        SUM(o.NumberOfEmployees) OVER (ORDER BY o.OrganizationID) AS TotalEmployeesBelow
    FROM
        Organization o
),
EmployeeDetails AS (
    SELECT
        e.EmployeeID,
        e.FirstName,
        e.LastName,
        e.Email,
        e.Department,
        e.OrganizationID,
        e.ReportsToEmployeeID
    FROM
        Employee e
),
EmployeeWithHierarchy AS (
    SELECT
        ed.EmployeeID,
        ed.FirstName,
        ed.LastName,
        ed.Email,
        ed.Department,
        roh.OrganizationName,
        roh.TreeLevel,
        roh.TotalEmployeesBelow
    FROM
        EmployeeDetails ed
    JOIN
        RecursiveOrganizationHierarchy roh ON ed.OrganizationID = roh.OrganizationID
)
SELECT DISTINCT
    eh.EmployeeID,
    eh.FirstName,
    eh.LastName,
    eh.Email,
    eh.Department,
    eh.OrganizationName,
    eh.TotalEmployeesBelow,
    (
        SELECT GROUP_CONCAT(ed2.FirstName || ' ' || ed2.LastName, ', ')
        FROM EmployeeWithHierarchy ed2
        WHERE
            ed2.OrganizationName = eh.OrganizationName AND ed2.TreeLevel > eh.TreeLevel
    ) AS ReportingManagerNames
FROM
    EmployeeWithHierarchy eh;
```

Explanation:

This SQL code is a complex and differentiated query that retrieves employee details along with organization hierarchy and reporting manager information. It uses a series of Common Table Expressions (CTEs) to achieve this:

1. `RecursiveOrganizationHierarchy` CTE:
   - Selects organization data from the `Organization` table and calculates the tree level and total employees below each organization.
   - Uses a recursive ranking function to assign tree levels to organizations based on their parent-child relationships.

2. `EmployeeDetails` CTE:
   - Selects employee data from the `Employee` table, including their organization ID and reporting manager ID.

3. `EmployeeWithHierarchy` CTE:
   - Joins the `EmployeeDetails` CTE with the `RecursiveOrganizationHierarchy` CTE to add organization hierarchy information to each employee.

The final SELECT statement retrieves distinct employee records and includes columns for employee ID, name, email, department, organization name, total employees below, and a list of reporting manager names.

The subquery in the `ReportingManagerNames` column uses the `EmployeeWithHierarchy` CTE to find and concatenate the names of all reporting managers within the same organization and at higher tree levels.

Overall, this code efficiently combines employee and organization data to provide a comprehensive view of employee details within their respective organizational structures and reporting relationships.