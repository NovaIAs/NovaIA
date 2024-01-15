```sql
-- Create a temporary table to store the employee data
CREATE TEMP TABLE EmployeeData (
    EmployeeID INT NOT NULL,
    FirstName VARCHAR(50) NOT NULL,
    LastName VARCHAR(50) NOT NULL,
    Department VARCHAR(50) NOT NULL,
    Salary DECIMAL(18, 2) NOT NULL,
    HireDate DATE NOT NULL,
    ManagerID INT,
    CONSTRAINT PK_EmployeeData PRIMARY KEY (EmployeeID)
);

-- Insert data into the temporary table
INSERT INTO EmployeeData (EmployeeID, FirstName, LastName, Department, Salary, HireDate, ManagerID) VALUES
(1, 'John', 'Smith', 'Sales', 8000.00, '2023-03-08', NULL),
(2, 'Mary', 'Johnson', 'Marketing', 9000.00, '2022-06-15', 1),
(3, 'Robert', 'Jones', 'Engineering', 10000.00, '2021-09-22', 1),
(4, 'Susan', 'Miller', 'Customer Service', 7000.00, '2020-12-29', 2),
(5, 'William', 'Davis', 'Sales', 8500.00, '2019-03-12', 1),
(6, 'Sarah', 'Wilson', 'Marketing', 9500.00, '2018-06-19', 2),
(7, 'David', 'Brown', 'Engineering', 11000.00, '2017-09-26', 3),
(8, 'Linda', 'Green', 'Customer Service', 7500.00, '2016-12-31', 4),
(9, 'James', 'White', 'Sales', 9000.00, '2015-03-14', 5),
(10, 'Patricia', 'Harris', 'Marketing', 10000.00, '2014-06-21', 6);

-- Create a common table expression (CTE) to calculate the total sales for each department
WITH DepartmentSales AS (
    SELECT
        Department,
        SUM(Salary) AS TotalSales
    FROM EmployeeData
    WHERE Department IN ('Sales', 'Marketing')
    GROUP BY Department
)

-- Create a CTE to calculate the average salary for each department
WITH DepartmentAverageSalary AS (
    SELECT
        Department,
        AVG(Salary) AS AverageSalary
    FROM EmployeeData
    GROUP BY Department
)

-- Create a CTE to calculate the employee details along with the department sales and average salary
WITH EmployeeDetails AS (
    SELECT
        EmployeeData.EmployeeID,
        EmployeeData.FirstName,
        EmployeeData.LastName,
        EmployeeData.Department,
        EmployeeData.Salary,
        DepartmentSales.TotalSales,
        DepartmentAverageSalary.AverageSalary
    FROM EmployeeData
    LEFT JOIN DepartmentSales ON EmployeeData.Department = DepartmentSales.Department
    LEFT JOIN DepartmentAverageSalary ON EmployeeData.Department = DepartmentAverageSalary.Department
)

-- Select and display the employee details along with the department sales and average salary
SELECT
    EmployeeDetails.EmployeeID,
    EmployeeDetails.FirstName,
    EmployeeDetails.LastName,
    EmployeeDetails.Department,
    EmployeeDetails.Salary,
    EmployeeDetails.TotalSales,
    EmployeeDetails.AverageSalary
FROM EmployeeDetails;
```

Explanation:

1. Create a temporary table called `EmployeeData` to store employee information, including `EmployeeID`, `FirstName`, `LastName`, `Department`, `Salary`, `HireDate`, and `ManagerID`.

2. Insert sample data into the `EmployeeData` table.

3. Create a common table expression (CTE) called `DepartmentSales` to calculate the total sales for each department in the 'Sales' or 'Marketing' departments.

4. Create another CTE called `DepartmentAverageSalary` to calculate the average salary for each department.

5. Create a third CTE called `EmployeeDetails` to join the `EmployeeData` table with the `DepartmentSales` and `DepartmentAverageSalary` CTEs, allowing us to retrieve employee details along with the department sales and average salary.

6. Finally, select and display the employee details, including their `EmployeeID`, `FirstName`, `LastName`, `Department`, `Salary`, `TotalSales`, and `AverageSalary`.