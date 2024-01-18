```sql
-- Create a temporary table (#CustomersWithHighSales) to identify customers with total sales above a certain threshold.
WITH CustomersWithHighSales AS (
    SELECT
        c.CustomerID,
        c.CompanyName,
        c.ContactName,
        SUM(od.Quantity * od.UnitPrice) AS TotalSales
    FROM
        Customers c
    JOIN
        Orders o ON c.CustomerID = o.CustomerID
    JOIN
        "Order Details" od ON o.OrderID = od.OrderID
    GROUP BY
        c.CustomerID, c.CompanyName, c.ContactName
    HAVING
        TotalSales > 50000 -- Set the sales threshold here.
),

-- Create a temporary table (#ProductsInHighDemand) to identify products with a high number of units sold.
ProductsInHighDemand AS (
    SELECT
        p.ProductID,
        p.ProductName,
        SUM(od.Quantity) AS TotalUnitsSold
    FROM
        Products p
    JOIN
        "Order Details" od ON p.ProductID = od.ProductID
    GROUP BY
        p.ProductID, p.ProductName
    HAVING
        TotalUnitsSold > 100 -- Set the units sold threshold here.
)

-- Select customer information along with product details for those customers who have high total sales and have purchased products with high demand.
SELECT
    c.CustomerID,
    c.CompanyName,
    c.ContactName,
    cws.TotalSales,
    p.ProductID,
    p.ProductName,
    pih.TotalUnitsSold
FROM
    Customers c
JOIN
    CustomersWithHighSales cws ON c.CustomerID = cws.CustomerID
JOIN
    Orders o ON c.CustomerID = o.CustomerID
JOIN
    "Order Details" od ON o.OrderID = od.OrderID
JOIN
    Products p ON od.ProductID = p.ProductID
JOIN
    ProductsInHighDemand pih ON p.ProductID = pih.ProductID;
```

Explanation:

1. Create Temporary Tables:
   - CustomersWithHighSales: This temporary table identifies customers with a total sales amount exceeding a specified threshold.
   - ProductsInHighDemand: This temporary table identifies products with a total number of units sold exceeding a specified threshold.

2. Join Tables:
   - Join Customers, CustomersWithHighSales, Orders, Order Details, Products, and ProductsInHighDemand tables to retrieve customer information, sales data, and product details.

3. Filter Data:
   - Filter customers based on their total sales using the CustomersWithHighSales temporary table.
   - Filter products based on their total units sold using the ProductsInHighDemand temporary table.

4. Select and Display Results:
   - Select customer information, total sales, product details, and total units sold for products purchased by those customers.