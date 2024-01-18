```sql
WITH ModelMetrics AS (
    SELECT 
        model_name, 
        metric_name, 
        metric_value, 
        metric_timestamp
    FROM 
        model_metrics
    WHERE 
        metric_timestamp BETWEEN '2023-01-01' AND '2023-12-31'
),
ModelRanks AS (
    SELECT 
        model_name, 
        RANK() OVER(PARTITION BY metric_name ORDER BY metric_value DESC) AS rank_metric
    FROM 
        ModelMetrics
    GROUP BY 
        model_name, 
        metric_name
)
SELECT 
    mm.model_name, 
    mm.metric_name, 
    mm.metric_value, 
    mm.metric_timestamp, 
    mr.rank_metric
FROM 
    ModelMetrics mm
JOIN 
    ModelRanks mr ON mm.model_name = mr.model_name AND mm.metric_name = mr.metric_name
WHERE 
    mr.rank_metric <= 3
ORDER BY 
    mm.model_name, 
    mm.metric_name, 
    mm.metric_timestamp;
```

Explanation:

1. `ModelMetrics`: This Common Table Expression (CTE) selects and filters model metrics data from the `'model_metrics'` table for a specified time range in `'2023'`. This CTE aliased as `'mm'`.

2. `ModelRanks`: This CTE calculates the rank of each model for each metric. It ranks models in descending order based on their metric values. Models with higher metric values have lower ranks. This CTE aliased as `'mr'`.

3. The outer `SELECT` statement retrieves the following information:
   - `'mm.model_name'`: The name of the model.
   - `'mm.metric_name'`: The name of the metric.
   - `'mm.metric_value'`: The value of the metric.
   - `'mm.metric_timestamp'`: The timestamp when the metric was recorded.
   - `'mr.rank_metric'`: The rank of the model for the given metric.

4. The `JOIN` clause joins the `'mm'` and `'mr'` CTEs on the `'model_name'` and `'metric_name'` columns to associate model metrics with their corresponding ranks.

5. The `WHERE` clause filters the results to include only models with ranks of 1, 2, or 3. This ensures that only the top-performing models are included in the final result set.

6. The `ORDER BY` clause sorts the results first by `'model_name'`, then by `'metric_name'`, and finally by `'metric_timestamp'`.

The result of this query is a list of model metrics for the top-performing models, along with their ranks for each metric. This information can be useful for identifying the best-performing models and monitoring their performance over time.