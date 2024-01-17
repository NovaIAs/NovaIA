```sql
-- Create a common table expression (CTE) called EmployeeDetails
WITH EmployeeDetails AS (
    SELECT
        e.id AS EmployeeID,
        e.first_name AS FirstName,
        e.last_name AS LastName,
        e.email AS Email,
        e.phone AS Phone,
        d.department_name AS Department,
        r.role_name AS Role,
        m.manager_id AS ManagerID
    FROM
        employees AS e
    LEFT JOIN
        departments AS d ON e.department_id = d.department_id
    LEFT JOIN
        roles AS r ON e.role_id = r.role_id
    LEFT JOIN
        managers AS m ON e.manager_id = m.manager_id
),

-- Create another CTE called ManagerDetails
ManagerDetails AS (
    SELECT
        m.manager_id AS ManagerID,
        m.first_name AS ManagerFirstName,
        m.last_name AS ManagerLastName,
        m.email AS ManagerEmail,
        d.department_name AS ManagerDepartment
    FROM
        managers AS m
    LEFT JOIN
        departments AS d ON m.department_id = d.department_id
),

-- Create a final CTE called EmployeeHierarchy
EmployeeHierarchy AS (
    SELECT
        ed.EmployeeID,
        ed.FirstName,
        ed.LastName,
        ed.Email,
        ed.Phone,
        ed.Department,
        ed.Role,
        md.ManagerID,
        md.ManagerFirstName,
        md.ManagerLastName,
        md.ManagerEmail,
        md.ManagerDepartment
    FROM
        EmployeeDetails AS ed
    LEFT JOIN
        ManagerDetails AS md ON ed.ManagerID = md.ManagerID
)

-- Select and display the employee and manager details in a hierarchical format
SELECT
    eh.EmployeeID,
    eh.FirstName,
    eh.LastName,
    eh.Email,
    eh.Phone,
    eh.Department,
    eh.Role,
    eh.ManagerID,
    eh.ManagerFirstName,
    eh.ManagerLastName,
    eh.ManagerEmail,
    eh.ManagerDepartment
FROM
    EmployeeHierarchy AS eh;
```

**Explanation:**

1. We start by creating a CTE called `EmployeeDetails`, which retrieves information about employees, departments, roles, and managers. This CTE includes columns like `EmployeeID`, `FirstName`, `LastName`, `Email`, `Phone`, `Department`, `Role`, and `ManagerID`.


2. Next, we create another CTE called `ManagerDetails`, which retrieves information about managers, including their manager ID, first name, last name, email, and department.


3. We then create a final CTE called `EmployeeHierarchy`, which combines information from the `EmployeeDetails` and `ManagerDetails` CTEs, providing a hierarchical view of employees and their managers.


4. Finally, we select and display the employee and manager details from the `EmployeeHierarchy` CTE in a hierarchical format, showing each employee's information along with their manager's details.