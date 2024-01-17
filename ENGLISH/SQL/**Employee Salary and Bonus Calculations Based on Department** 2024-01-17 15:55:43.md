```sql
-- Create a temporary table to store employee and department data
WITH EmployeeDepartment AS (
    SELECT
        e.employee_id,
        e.first_name,
        e.last_name,
        e.department_id,
        d.department_name
    FROM
        employees e
    JOIN
        departments d ON e.department_id = d.department_id
),

-- Create a temporary table to calculate employee salaries based on department
EmployeeSalaries AS (
    SELECT
        ed.employee_id,
        ed.first_name,
        ed.last_name,
        ed.department_name,
        CASE
            WHEN ed.department_name = 'Sales' THEN e.salary * 1.10
            WHEN ed.department_name = 'Marketing' THEN e.salary * 1.05
            ELSE e.salary
        END AS adjusted_salary
    FROM
        EmployeeDepartment ed
    JOIN
        salaries s ON ed.employee_id = s.employee_id
),

-- Create a temporary table to calculate employee bonuses
EmployeeBonuses AS (
    SELECT
        es.employee_id,
        es.first_name,
        es.last_name,
        es.department_name,
        es.adjusted_salary,
        CASE
            WHEN es.department_name = 'Sales' THEN es.adjusted_salary * 0.15
            WHEN es.department_name = 'Marketing' THEN es.adjusted_salary * 0.10
            ELSE es.adjusted_salary * 0.05
        END AS bonus
    FROM
        EmployeeSalaries es
)

-- Select employee information along with calculated salaries and bonuses
SELECT
    eb.employee_id,
    eb.first_name,
    eb.last_name,
    eb.department_name,
    eb.adjusted_salary,
    eb.bonus
FROM
    EmployeeBonuses eb;
```

**Explanation:**

1. The first CTE, `EmployeeDepartment`, joins the `employees` and `departments` tables to create a temporary table containing employee and department information.

2. The second CTE, `EmployeeSalaries`, calculates adjusted salaries for employees based on their department. It applies a 10% bonus to sales employees, a 5% bonus to marketing employees, and no bonus to other employees.

3. The third CTE, `EmployeeBonuses`, calculates bonuses for employees based on their adjusted salaries. It applies a 15% bonus to sales employees, a 10% bonus to marketing employees, and a 5% bonus to other employees.

4. The final query selects employee information along with calculated salaries and bonuses from the `EmployeeBonuses` CTE.