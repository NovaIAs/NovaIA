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
),

EmployeeDetails AS (
    SELECT
        reh.EmployeeID,
        reh.FirstName,
        reh.LastName,
        reh.Level,
        e.PhoneNumber,
        e.Email,
        e.HireDate
    FROM
        RecursiveEmployeeHierarchy reh
    JOIN
        Employees e ON reh.EmployeeID = e.EmployeeID
),

EmployeeSalaryHistory AS (
    SELECT
        esh.EmployeeID,
        esh.Salary,
        esh.StartDate,
        esh.EndDate
    FROM
        EmployeeSalaryHistory esh
),

EmployeeCurrentSalary AS (
    SELECT
        esh.EmployeeID,
        esh.Salary
    FROM
        EmployeeSalaryHistory esh
    WHERE
        esh.EndDate IS NULL
)

SELECT
    ed.EmployeeID,
    ed.FirstName,
    ed.LastName,
    ed.Level,
    ed.PhoneNumber,
    ed.Email,
    ed.HireDate,
    ecs.Salary AS CurrentSalary,
    (
        SELECT
            esh.Salary
        FROM
            EmployeeSalaryHistory esh
        WHERE
            esh.EmployeeID = ed.EmployeeID
        ORDER BY
            esh.StartDate DESC
        LIMIT 1
    ) AS PreviousSalary
FROM
    EmployeeDetails ed
JOIN
    EmployeeCurrentSalary ecs ON ed.EmployeeID = ecs.EmployeeID;
```

Explanation:

1. RecursiveEmployeeHierarchy CTE:
   - This CTE uses a recursive query to create a hierarchical structure of employees based on their manager-subordinate relationships.
   - It starts by selecting employees with no manager (Level 1) and then recursively adds employees based on their ManagerID.

2. EmployeeDetails CTE:
   - This CTE joins the RecursiveEmployeeHierarchy CTE with the Employees table to retrieve additional employee details, such as phone number, email, and hire date.

3. EmployeeSalaryHistory CTE:
   - This CTE selects salary history records for employees.

4. EmployeeCurrentSalary CTE:
   - This CTE selects the current salary for each employee based on the latest salary history record with a NULL EndDate.

5. Final SELECT statement:
   - This statement combines data from the EmployeeDetails, EmployeeCurrentSalary, and EmployeeSalaryHistory CTEs to retrieve detailed employee information, including their current salary and previous salary (last salary before the current one).