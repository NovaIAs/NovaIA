```sql
-- Common Table Expression (CTE) to calculate the monthly revenue for each product category
WITH MonthlyRevenue AS (
    SELECT
        p.category,
        DATE_FORMAT(o.order_date, '%Y-%m') AS order_month,
        SUM(oi.quantity * oi.unit_price) AS monthly_revenue
    FROM
        products p
    JOIN
        order_items oi ON p.product_id = oi.product_id
    JOIN
        orders o ON oi.order_id = o.order_id
    GROUP BY
        p.category,
        order_month
),

-- CTE to calculate the year-to-date (YTD) revenue for each product category
YearToDateRevenue AS (
    SELECT
        category,
        SUM(monthly_revenue) OVER (PARTITION BY category ORDER BY order_month) AS ytd_revenue
    FROM
        MonthlyRevenue
),

-- CTE to calculate the percentage change in revenue for each product category compared to the previous month
MonthlyRevenueChange AS (
    SELECT
        category,
        order_month,
        monthly_revenue,
        (monthly_revenue - LAG(monthly_revenue, 1, 0) OVER (PARTITION BY category ORDER BY order_month)) * 100.0 / LAG(monthly_revenue, 1, 0) OVER (PARTITION BY category ORDER BY order_month) AS monthly_revenue_change
    FROM
        MonthlyRevenue
),

-- CTE to calculate the average monthly revenue for each product category
AvgMonthlyRevenue AS (
    SELECT
        category,
        AVG(monthly_revenue) AS avg_monthly_revenue
    FROM
        MonthlyRevenue
    GROUP BY
        category
)

-- Final query to retrieve the product category, YTD revenue, monthly revenue, monthly revenue change, and average monthly revenue
SELECT
    mr.category,
    ytd.ytd_revenue,
    mr.monthly_revenue,
    mr.monthly_revenue_change,
    amr.avg_monthly_revenue
FROM
    YearToDateRevenue ytd
JOIN
    MonthlyRevenue mr ON ytd.category = mr.category
JOIN
    MonthlyRevenueChange mrc ON ytd.category = mrc.category AND ytd.order_month = mrc.order_month
JOIN
    AvgMonthlyRevenue amr ON ytd.category = amr.category;
```

**Explanation:**

* The `MonthlyRevenue` CTE calculates the monthly revenue for each product category by summing the quantity multiplied by the unit price for each order item within a given month.


* The `YearToDateRevenue` CTE calculates the YTD revenue for each product category by summing the monthly revenue for each month up to the current month.


* The `MonthlyRevenueChange` CTE calculates the percentage change in revenue for each product category compared to the previous month.


* The `AvgMonthlyRevenue` CTE calculates the average monthly revenue for each product category by averaging the monthly revenue over the entire time period.


* The final query joins these CTEs to retrieve the product category, YTD revenue, monthly revenue, monthly revenue change, and average monthly revenue for each product category.