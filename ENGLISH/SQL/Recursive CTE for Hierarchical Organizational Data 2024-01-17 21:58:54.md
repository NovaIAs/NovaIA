```sql
WITH RecursiveChilds AS (
    SELECT 
        ORGANIZATION_ID,
        ORGANIZATION_PARENT_ID,
        ORGANIZATION_NAME,
        1 AS LEVEL
    FROM 
        ORGANIZATIONS
    WHERE 
        ORGANIZATION_PARENT_ID IS NULL
    UNION ALL
    SELECT 
        o.ORGANIZATION_ID,
        o.ORGANIZATION_PARENT_ID,
        o.ORGANIZATION_NAME,
        rc.LEVEL + 1 AS LEVEL
    FROM 
        ORGANIZATIONS AS o
    INNER JOIN 
        RecursiveChilds AS rc ON o.ORGANIZATION_PARENT_ID = rc.ORGANIZATION_ID
),

OrganizationHierarchy AS (
    SELECT 
        ORGANIZATION_ID,
        ORGANIZATION_PARENT_ID,
        ORGANIZATION_NAME,
        LEVEL
    FROM 
        RecursiveChilds
    WHERE 
        LEVEL <= (
            SELECT MAX(LEVEL)
            FROM RecursiveChilds
        )
)

SELECT 
    OH.ORGANIZATION_ID,
    OH.ORGANIZATION_PARENT_ID,
    OH.ORGANIZATION_NAME,
    OH.LEVEL
FROM 
    OrganizationHierarchy AS OH
ORDER BY 
    OH.ORGANIZATION_ID;
```

Explanation:

This code is a recursive Common Table Expression (CTE) query used to generate a hierarchical representation of organizational data, including each organization's name, parent organization, and level within the hierarchy. The query involves two CTEs, RecursiveChilds and OrganizationHierarchy:

1. **RecursiveChilds**:
   - This recursive CTE finds all child organizations at different levels by repeatedly joining the ORGANIZATIONS table with itself based on the ORGANIZATION_PARENT_ID column.
   - It starts by selecting organizations with no parent (i.e., the root organizations) and assigns them a LEVEL of 1.
   - Subsequent recursive joins add child organizations to the result set, incrementing the LEVEL for each level of the hierarchy.

2. **OrganizationHierarchy**:
   - This CTE filters the RecursiveChilds CTE to include only organizations at or below a specified level (determined by the maximum LEVEL in the RecursiveChilds CTE).
   - This ensures that the final result contains only the desired depth of the hierarchy.

The query then selects and orders the columns from the OrganizationHierarchy CTE, providing a hierarchical representation of organizations, including their IDs, parent IDs, names, and levels. This result can be used to display organizational structures, perform hierarchical analysis, or generate nested reports.