```sql
-- Create a temporary table to store the results of a complex query
WITH RankedCustomers AS (
    SELECT
        c.customer_id,
        c.customer_name,
        c.total_purchases,
        RANK() OVER (PARTITION BY c.customer_type ORDER BY c.total_purchases DESC) AS customer_rank
    FROM
        customers c
),

-- Create a second temporary table to calculate customer segments based on their rank
CustomerSegments AS (
    SELECT
        rc.customer_id,
        rc.customer_name,
        rc.total_purchases,
        CASE
            WHEN rc.customer_rank <= 10 THEN 'Top 10%'
            WHEN rc.customer_rank BETWEEN 11 AND 50 THEN 'Middle 40%'
            ELSE 'Bottom 50%'
        END AS customer_segment
    FROM
        RankedCustomers rc
)

-- Select and display the customer information along with their segment
SELECT
    cs.customer_id,
    cs.customer_name,
    cs.total_purchases,
    cs.customer_segment
FROM
    CustomerSegments cs;
```

**Explanation:**

1. **RankedCustomers CTE:**
   - This Common Table Expression (CTE) calculates the total purchases for each customer and ranks them within their customer type based on their total purchases in descending order.

2. **CustomerSegments CTE:**
   - This CTE uses the results from the RankedCustomers CTE to assign customer segments based on their rank.

3. **Final SELECT:**
   - The final SELECT statement retrieves and displays the customer information along with their assigned customer segments.