```sql
WITH CTE_Sales AS (
    SELECT
        s.SalesID,
        s.ProductID,
        s.QuantitySold,
        s.UnitPrice,
        s.SalesDate,
        RANK() OVER (PARTITION BY s.ProductID ORDER BY s.SalesDate) AS SalesRank
    FROM Sales s
),

CTE_BestSellingProducts AS (
    SELECT
        ProductID,
        SUM(QuantitySold) AS TotalQuantitySold
    FROM CTE_Sales
    WHERE SalesRank = 1
    GROUP BY ProductID
),

CTE_ProductCategories AS (
    SELECT
        p.ProductID,
        c.CategoryName
    FROM Products p
    JOIN Categories c ON p.CategoryID = c.CategoryID
),

CTE_CustomerInfo AS (
    SELECT
        c.CustomerID,
        c.CustomerName,
        c.ContactName,
        c.Country
    FROM Customers c
),

CTE_OrderSummary AS (
    SELECT
        o.OrderID,
        o.OrderDate,
        o.CustomerID,
        o.TotalAmount,
        (SELECT SUM(QuantitySold) FROM OrderDetails od WHERE od.OrderID = o.OrderID) AS TotalQuantityOrdered
    FROM Orders o
),

CTE_TopCustomers AS (
    SELECT
        CustomerID,
        CustomerName,
        SUM(TotalAmount) AS TotalRevenue,
        COUNT(DISTINCT OrderID) AS NumberOfOrders
    FROM CTE_CustomerInfo ci
    JOIN CTE_OrderSummary os ON ci.CustomerID = os.CustomerID
    GROUP BY CustomerID, CustomerName
    ORDER BY TotalRevenue DESC
    LIMIT 5
),

CTE_ProductsWithLowStock AS (
    SELECT
        ProductID,
        ProductName,
        UnitsInStock,
        UnitsOnOrder
    FROM Products
    WHERE UnitsInStock < 10 AND UnitsOnOrder = 0
),

CTE_EmployeesWithHighSales AS (
    SELECT
        e.EmployeeID,
        e.FirstName,
        e.LastName,
        SUM(od.QuantityOrdered * od.UnitPrice) AS TotalSalesAmount
    FROM Employees e
    JOIN Orders o ON e.EmployeeID = o.EmployeeID
    JOIN OrderDetails od ON o.OrderID = od.OrderID
    GROUP BY e.EmployeeID, e.FirstName, e.LastName
    ORDER BY TotalSalesAmount DESC
    LIMIT 10
)

SELECT
    CTE_Sales.SalesID,
    CTE_Sales.ProductID,
    CTE_Sales.QuantitySold,
    CTE_Sales.UnitPrice,
    CTE_Sales.SalesDate,
    CTE_BestSellingProducts.TotalQuantitySold AS BestSellingProductQuantity,
    CTE_ProductCategories.CategoryName,
    CTE_CustomerInfo.CustomerID,
    CTE_CustomerInfo.CustomerName,
    CTE_CustomerInfo.ContactName,
    CTE_CustomerInfo.Country,
    CTE_OrderSummary.OrderID,
    CTE_OrderSummary.OrderDate,
    CTE_OrderSummary.TotalAmount,
    CTE_OrderSummary.TotalQuantityOrdered,
    CTE_TopCustomers.CustomerID AS TopCustomerID,
    CTE_TopCustomers.CustomerName AS TopCustomerName,
    CTE_TopCustomers.TotalRevenue AS TopCustomerRevenue,
    CTE_TopCustomers.NumberOfOrders AS TopCustomerNumberOfOrders,
    CTE_ProductsWithLowStock.ProductID AS LowStockProductID,
    CTE_ProductsWithLowStock.ProductName AS LowStockProductName,
    CTE_ProductsWithLowStock.UnitsInStock AS LowStockUnitsInStock,
    CTE_ProductsWithLowStock.UnitsOnOrder AS LowStockUnitsOnOrder,
    CTE_EmployeesWithHighSales.EmployeeID AS HighSalesEmployeeID,
    CTE_EmployeesWithHighSales.FirstName AS HighSalesFirstName,
    CTE_EmployeesWithHighSales.LastName AS HighSalesLastName,
    CTE_EmployeesWithHighSales.TotalSalesAmount AS HighSalesTotalAmount
FROM CTE_Sales
JOIN CTE_BestSellingProducts ON CTE_Sales.ProductID = CTE_BestSellingProducts.ProductID
JOIN CTE_ProductCategories ON CTE_Sales.ProductID = CTE_ProductCategories.ProductID
JOIN CTE_CustomerInfo ON CTE_Sales.CustomerID = CTE_CustomerInfo.CustomerID
JOIN CTE_OrderSummary ON CTE_Sales.SalesID = CTE_OrderSummary.OrderID
LEFT JOIN CTE_TopCustomers ON CTE_CustomerInfo.CustomerID = CTE_TopCustomers.CustomerID
LEFT JOIN CTE_ProductsWithLowStock ON CTE_Sales.ProductID = CTE_ProductsWithLowStock.ProductID
LEFT JOIN CTE_EmployeesWithHighSales ON CTE_Sales.EmployeeID = CTE_EmployeesWithHighSales.EmployeeID;
```

**Explanation:**

This complex SQL code combines data from multiple tables and performs various calculations and aggregations to provide a comprehensive overview of sales, products, customers, and employees.

- **CTE_Sales:** This CTE (Common Table Expression) ranks sales records for each product based on the sales date.

- **CTE_BestSellingProducts:** This CTE identifies the best-selling products based on the total quantity sold.

- **CTE_ProductCategories:** This CTE joins products with their respective categories.

- **CTE_CustomerInfo:** This CTE retrieves customer information such as name, contact name, and country.

- **CTE_OrderSummary:** This CTE summarizes order data, including the total amount and total quantity ordered.

- **CTE_TopCustomers:** This CTE identifies the top customers based on total revenue and number of orders.

- **CTE_ProductsWithLowStock:** This CTE identifies products with low stock levels and no units on order.

- **CTE_EmployeesWithHighSales:** This CTE identifies employees with the highest total sales amounts.

- **Final Query:** The final query joins all the CTEs and retrieves various information about sales, products, customers, and employees.