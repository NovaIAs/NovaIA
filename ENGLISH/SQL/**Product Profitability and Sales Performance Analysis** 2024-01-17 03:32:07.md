```sql
-- This query retrieves data from multiple tables in the AdventureWorks2019 database to provide insights into sales performance and product profitability.

-- Common Table Expression (CTE) to calculate product-level profitability
WITH ProductProfitability AS (
    SELECT
        p.ProductID,
        p.Name AS ProductName,
        SUM(s.OrderQty) AS TotalSoldQuantity,
        SUM(s.OrderQty * s.UnitPrice * (1 - s.UnitPriceDiscount)) AS TotalSalesAmount,
        SUM(s.OrderQty * s.UnitPrice * (1 - s.UnitPriceDiscount) - (s.UnitPrice * p.StandardCost)) AS TotalProfitAmount
    FROM
        Sales.SalesOrderHeader AS soh
    JOIN
        Sales.SalesOrderDetail AS sod ON soh.SalesOrderID = sod.SalesOrderID
    JOIN
        Product.Product AS p ON sod.ProductID = p.ProductID
    GROUP BY
        p.ProductID, p.Name
),

-- CTE to calculate sales performance by product category
ProductCategorySales AS (
    SELECT
        pc.ProductCategoryID,
        pc.Name AS ProductCategoryName,
        SUM(s.OrderQty) AS TotalCategorySalesQuantity,
        SUM(s.OrderQty * s.UnitPrice * (1 - s.UnitPriceDiscount)) AS TotalCategorySalesAmount
    FROM
        Sales.SalesOrderHeader AS soh
    JOIN
        Sales.SalesOrderDetail AS sod ON soh.SalesOrderID = sod.SalesOrderID
    JOIN
        Product.Product AS p ON sod.ProductID = p.ProductID
    JOIN
        Product.ProductCategory AS pc ON p.ProductCategoryID = pc.ProductCategoryID
    GROUP BY
        pc.ProductCategoryID, pc.Name
)

-- Final query to combine product profitability and sales performance data
SELECT
    pp.ProductID,
    pp.ProductName,
    pp.TotalSoldQuantity,
    pp.TotalSalesAmount,
    pp.TotalProfitAmount,
    pcs.ProductCategoryName,
    pcs.TotalCategorySalesQuantity,
    pcs.TotalCategorySalesAmount
FROM
    ProductProfitability AS pp
JOIN
    ProductCategorySales AS pcs ON pp.ProductID = pcs.ProductCategoryID;
```

**Explanation:**

1. **ProductProfitability CTE:**
   - Calculates the profitability of each product.
   - It sums up the total quantity sold, total sales amount, and total profit amount for each product.
   - It subtracts the standard cost of the product from the sales amount to calculate the profit.

2. **ProductCategorySales CTE:**
   - Calculates the sales performance for each product category.
   - It sums up the total quantity sold and total sales amount for each product category.

3. **Final Query:**
   - Combines the data from the two CTEs to provide insights into both product profitability and sales performance.
   - It joins the two CTEs on the ProductID and ProductCategoryName columns.
   - Displays the product ID, product name, total quantity sold, total sales amount, total profit amount, product category name, total category sales quantity, and total category sales amount.