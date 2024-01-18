```sql

-- Create a temporary table to store the current date and time.
DECLARE @CurrentDateTime DATETIME;
SET @CurrentDateTime = GETDATE();

-- Create a temporary table to store the sales data.
DECLARE @SalesData TABLE (
    ProductID INT,
    ProductName VARCHAR(50),
    UnitPrice MONEY,
    Quantity INT,
    TotalSales MONEY
);

-- Insert data into the SalesData table.
INSERT INTO @SalesData (ProductID, ProductName, UnitPrice, Quantity, TotalSales)
VALUES
    (1, 'Product 1', 10.00, 5, 50.00),
    (2, 'Product 2', 20.00, 10, 200.00),
    (3, 'Product 3', 30.00, 15, 450.00),
    (4, 'Product 4', 40.00, 20, 800.00),
    (5, 'Product 5', 50.00, 25, 1250.00);

-- Create a temporary table to store the product categories.
DECLARE @ProductCategories TABLE (
    ProductCategoryID INT,
    ProductCategoryName VARCHAR(50)
);

-- Insert data into the ProductCategories table.
INSERT INTO @ProductCategories (ProductCategoryID, ProductCategoryName)
VALUES
    (1, 'Category 1'),
    (2, 'Category 2'),
    (3, 'Category 3'),
    (4, 'Category 4'),
    (5, 'Category 5');

-- Create a temporary table to store the order details.
DECLARE @OrderDetails TABLE (
    OrderID INT,
    ProductID INT,
    Quantity INT,
    UnitPrice MONEY,
    TotalSales MONEY
);

-- Insert data into the OrderDetails table.
INSERT INTO @OrderDetails (OrderID, ProductID, Quantity, UnitPrice, TotalSales)
VALUES
    (1, 1, 5, 10.00, 50.00),
    (1, 2, 10, 20.00, 200.00),
    (2, 3, 15, 30.00, 450.00),
    (2, 4, 20, 40.00, 800.00),
    (3, 5, 25, 50.00, 1250.00);

-- Create a temporary table to store the sales summary data.
DECLARE @SalesSummary TABLE (
    ProductCategoryID INT,
    ProductCategoryName VARCHAR(50),
    TotalSales MONEY
);

-- Calculate the total sales for each product category.
INSERT INTO @SalesSummary (ProductCategoryID, ProductCategoryName, TotalSales)
SELECT
    P.ProductCategoryID,
    P.ProductCategoryName,
    SUM(S.TotalSales) AS TotalSales
FROM
    @ProductCategories P
JOIN
    @SalesData S ON P.ProductCategoryID = S.ProductID
GROUP BY
    P.ProductCategoryID, P.ProductCategoryName;

-- Create a temporary table to store the top 5 selling products.
DECLARE @TopSellingProducts TABLE (
    ProductID INT,
    ProductName VARCHAR(50),
    TotalSales MONEY,
    Rank INT
);

-- Calculate the top 5 selling products based on total sales.
INSERT INTO @TopSellingProducts (ProductID, ProductName, TotalSales, Rank)
SELECT
    S.ProductID,
    S.ProductName,
    S.TotalSales,
    ROW_NUMBER() OVER (ORDER BY S.TotalSales DESC) AS Rank
FROM
    @SalesData S
WHERE
    S.TotalSales > (SELECT MIN(TotalSales) FROM @SalesData)
ORDER BY
    S.TotalSales DESC;

-- Create a temporary table to store the order details for the top 5 selling products.
DECLARE @TopSellingProductsOrderDetails TABLE (
    OrderID INT,
    ProductID INT,
    Quantity INT,
    UnitPrice MONEY,
    TotalSales MONEY
);

-- Insert data into the TopSellingProductsOrderDetails table.
INSERT INTO @TopSellingProductsOrderDetails (OrderID, ProductID, Quantity, UnitPrice, TotalSales)
SELECT
    O.OrderID,
    O.ProductID,
    O.Quantity,
    O.UnitPrice,
    O.TotalSales
FROM
    @OrderDetails O
WHERE
    O.ProductID IN (SELECT ProductID FROM @TopSellingProducts);

-- Select the data from the temporary tables and format the output.
SELECT
    SD.ProductCategoryID,
    SD.ProductCategoryName,
    SD.TotalSales AS CategoryTotalSales,
    TSP.ProductID,
    TSP.ProductName AS TopSellingProductName,
    TSP.TotalSales AS TopSellingProductTotalSales,
    TSP.Rank,
    TSO.OrderID,
    TSO.Quantity AS TopSellingProductQuantity,
    TSO.UnitPrice AS TopSellingProductUnitPrice,
    TSO.TotalSales AS TopSellingProductTotalSales
FROM
    @SalesSummary SD
JOIN
    @TopSellingProducts TSP ON SD.ProductCategoryID = TSP.ProductCategoryID
JOIN
    @TopSellingProductsOrderDetails TSO ON TSP.ProductID = TSO.ProductID
ORDER BY
    SD.ProductCategoryID, TSP.Rank, TSO.OrderID;

```

This code performs the following tasks:

1. It creates several temporary tables to store data for the sales, product categories, order details, sales summary, top selling products, and top selling products order details.
2. It inserts data into these temporary tables using the `INSERT` statement.
3. It calculates the total sales for each product category and stores the results in the `SalesSummary` table.
4. It calculates the top 5 selling products based on total sales and stores the results in the `TopSellingProducts` table.
5. It inserts the order details for the top 5 selling products into the `TopSellingProductsOrderDetails` table.
6. Finally, it selects data from the temporary tables and formats the output using the `SELECT` statement.

The output of the code is a detailed report that shows the total sales for each product category, the top 5 selling products within each category, and the order details for those products. This report can be used to identify trends and patterns in sales data and to make informed decisions about product placement, marketing strategies, and inventory management.