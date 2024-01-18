```sql
-- Create a temporary table to store the Cartesian product of two tables
WITH CartesianProduct AS (
    SELECT A.column1, A.column2, B.column1, B.column2
    FROM TableA AS A
    CROSS JOIN TableB AS B
)
-- Select distinct values from the Cartesian product
SELECT DISTINCT column1, column2, column3, column4
FROM CartesianProduct;

-- Create a common table expression (CTE) to calculate the running total of sales
WITH RunningTotal AS (
    SELECT 
        product_id,
        product_name,
        SUM(sales) OVER (PARTITION BY product_id ORDER BY date) AS running_total
    FROM Sales
)
-- Select the product name and running total for each product
SELECT product_name, running_total
FROM RunningTotal
ORDER BY running_total DESC;

-- Create a recursive CTE to calculate the hierarchical structure of an organization
WITH RecursiveCTE AS (
    -- Base case: Select the top-level nodes of the organization
    SELECT employee_id, employee_name, manager_id
    FROM Employees
    WHERE manager_id IS NULL

    -- Recursive case: Select employees and their managers
    UNION ALL
    SELECT e.employee_id, e.employee_name, e.manager_id
    FROM Employees AS e
    JOIN RecursiveCTE AS r ON e.manager_id = r.employee_id
)
-- Select the employee name, employee ID, and manager name for each employee
SELECT employee_name, employee_id, manager_name
FROM RecursiveCTE
JOIN Employees AS m ON RecursiveCTE.manager_id = m.employee_id;

-- Create aCTE to calculate the moving average of a time series
WITH MovingAverage AS (
    SELECT 
        date,
        value,
        AVG(value) OVER (ORDER BY date ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_average
    FROM TimeSeries
)
-- Select the date, value, and moving average for each data point
SELECT date, value, moving_average
FROM MovingAverage;

-- Create aCTE to find the nth highest salary for each department
WITH RankedSalaries AS (
    SELECT 
        employee_id,
        salary,
        RANK() OVER (PARTITION BY department_id ORDER BY salary DESC) AS rank
    FROM Employees
)
-- Select the employee name, salary, and rank for the nth highest salary in each department
SELECT employee_name, salary, rank
FROM RankedSalaries
WHERE rank = 3;
```

This code performs various complex SQL operations, including Cartesian product, running total calculation, recursive CTE for hierarchical data, moving average calculation, and ranking of salaries within departments.