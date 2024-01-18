```sql
WITH CTE_Nested_Data_Extraction AS (
    SELECT
        ID,
        [Value],
        [Group]
    FROM
        [dbo].[Nested_Data]
),
CTE_Flatten_Nested_Data AS (
    SELECT
        ID,
        [Value],
        [Group],
        [Level]
    FROM
        CTE_Nested_Data_Extraction
    WHERE
        [Group] IS NOT NULL
    UNION ALL
    SELECT
        ID,
        [Value],
        [Group],
        [Level] + 1
    FROM
        CTE_Nested_Data_Extraction t1
    INNER JOIN
        CTE_Flatten_Nested_Data t2
    ON
        t1.ID = t2.ID
    WHERE
        t1.[Group] = t2.[Value]
)
SELECT
    ID,
    [Value],
    [Group],
    [Level]
FROM
    CTE_Flatten_Nested_Data
ORDER BY
    ID,
    [Level];
```

Explanation:

1. **CTE_Nested_Data_Extraction:**

   - This CTE (Common Table Expression) extracts the necessary columns (`ID`, `Value`, and `Group`) from the `[dbo].[Nested_Data]` table.
   - It serves as a starting point for further processing.

2. **CTE_Flatten_Nested_Data:**

   - This CTE is responsible for flattening the hierarchical nested data.

   - It starts with the records that have a non-NULL `Group` value (i.e., they are not leaf nodes). These records represent parent groups.

   - For each parent group, it recursively retrieves all its child values and groups through a self-join.

   - It assigns the `Level` column values using `[Level] + 1` to indicate the depth of each child value within the nested structure.

3. **Final SELECT Statement:**

   - The final SELECT statement retrieves the desired columns (`ID`, `Value`, `Group`, and `Level`) from the flattened data.

   - It applies an `ORDER BY` clause to sort the results by `ID` and `Level`, ensuring a hierarchical order.

This complex SQL code allows you to extract and flatten nested data hierarchically, providing a comprehensive view of the relationships and levels within the data structure.