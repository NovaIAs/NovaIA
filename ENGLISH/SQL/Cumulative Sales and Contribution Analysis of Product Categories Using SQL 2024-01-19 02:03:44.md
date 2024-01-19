```sql
--WITH clause to calculate cumulative sum of sales for each product category
WITH ProductSales AS (
    SELECT
        product_category,
        SUM(sales) OVER (PARTITION BY product_category ORDER BY date ASC) AS cumulative_sales
    FROM sales_data
)

--WITH clause to calculate the percentage contribution of each product category to the total sales
WITH ProductContribution AS (
    SELECT
        product_category,
        cumulative_sales,
        (cumulative_sales / SUM(cumulative_sales) OVER () * 100) AS contribution_percentage
    FROM ProductSales
)

--Main query to retrieve product categories, their cumulative sales, and contribution percentage
SELECT
    product_category,
    cumulative_sales,
    contribution_percentage
FROM ProductContribution
ORDER BY cumulative_sales DESC;
```

This complex SQL code performs the following operations:

1. **WITH ProductSales:**
   - Calculates the cumulative sum of sales for each product category using the OVER() function.
   - The cumulative sales are calculated by summing up the sales for each product category in ascending order of date.

2. **WITH ProductContribution:**
   - Calculates the contribution percentage of each product category to the total sales.
   - The contribution percentage is calculated by dividing the cumulative sales of each category by the total cumulative sales and multiplying by 100.

3. **Main Query:**
   - Selects the product category, cumulative sales, and contribution percentage from the ProductContribution common table expression (CTE).
   - Orders the results by cumulative sales in descending order.

This code efficiently combines multiple CTEs and window functions to perform complex calculations and provide insights into the cumulative sales and contribution of each product category.