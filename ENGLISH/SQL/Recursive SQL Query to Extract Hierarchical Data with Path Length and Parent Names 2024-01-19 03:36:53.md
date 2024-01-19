```sql
WITH RecursiveCTE AS (
    SELECT
        *,
        0 AS Level
    FROM
        YourTable
    WHERE
        [Condition]

    UNION ALL

    SELECT
        t2.*,
        Level + 1 AS Level
    FROM
        YourTable t2
    INNER JOIN
        RecursiveCTE t1 ON t1.[PrimaryKey] = t2.[ForeignKey]
),

CTE_WITH_HIERARCHY AS (
    SELECT
        Id,
        [Name],
        Level,
        COALESCE(CONCAT(t1.Name, ' > ', t2.Name), t1.Name) AS HierarchyPath
    FROM
        RecursiveCTE t1
    LEFT JOIN
        RecursiveCTE t2 ON t1.Id = t2.Parent_Id
),

CTE_WITH_PATH_LENGTH AS (
    SELECT
        Id,
        [Name],
        Level,
        HierarchyPath,
        LENGTH(HierarchyPath) AS PathLength
    FROM
        CTE_WITH_HIERARCHY
),

CTE_WITH_PARENTS_AT_EACH_LEVEL AS (
    SELECT
        Id,
        [Name],
        Level,
        HierarchyPath,
        PathLength,
        LAG(HierarchyPath, 1, '') OVER (PARTITION BY Level ORDER BY HierarchyPath) AS ParentPath
    FROM
        CTE_WITH_PATH_LENGTH
),

CTE_WITH_PARENTS_INFO AS (
    SELECT
        Id,
        [Name],
        Level,
        HierarchyPath,
        PathLength,
        ParentPath,
        SUBSTRING(ParentPath, LENGTH(ParentPath) - INSTR(REVERSE(ParentPath), '>') + 1) AS ParentName
    FROM
        CTE_WITH_PARENTS_AT_EACH_LEVEL
)

SELECT
    Id,
    [Name],
    Level,
    HierarchyPath,
    PathLength,
    ParentName
FROM
    CTE_WITH_PARENTS_INFO;
```

Explanation:

1. RecursiveCTE: This CTE recursively retrieves all records from the 'YourTable' table and adds a 'Level' column, which indicates the level of each record in the hierarchy.


2. CTE_WITH_HIERARCHY: This CTE calculates the hierarchy path for each record, starting from the root element. It uses a LEFT JOIN to combine records from different levels in the hierarchy.


3. CTE_WITH_PATH_LENGTH: This CTE adds a 'PathLength' column, which indicates the length of the hierarchy path for each record.


4. CTE_WITH_PARENTS_AT_EACH_LEVEL: This CTE uses the LAG window function to determine the parent path for each record at each level in the hierarchy.


5. CTE_WITH_PARENTS_INFO: This CTE extracts the name of the parent record from the parent path column and adds it as a new column named 'ParentName'.


The final SELECT statement retrieves all columns from the 'CTE_WITH_PARENTS_INFO' CTE, which now contains the hierarchical information, path length, and parent name for each record. This code allows you to effectively traverse and analyze hierarchical data in your database.