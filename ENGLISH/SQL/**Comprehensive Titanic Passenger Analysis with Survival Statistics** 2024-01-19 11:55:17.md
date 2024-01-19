```sql
-- Create a temporary table (#TempTable) to store the original data
WITH #TempTable AS (
    SELECT
        PassengerId,  -- Passenger ID
        Survived,      -- Survival status (0: No, 1: Yes)
        Pclass,         -- Passenger class (1: First, 2: Second, 3: Third)
        Name,          -- Passenger name
        Sex,           -- Passenger sex
        Age,           -- Passenger age
        SibSp,         -- Number of siblings and spouses aboard
        Parch,         -- Number of parents and children aboard
        Ticket,        -- Ticket number
        Fare,          -- Fare paid
        Cabin,         -- Cabin number
        Embarked       -- Port of embarkation (C: Cherbourg, Q: Queenstown, S: Southampton)
    FROM
        Titanic_Data  -- Your original Titanic data table
)

-- Create a CTE (#SurvivalStats) to calculate survival statistics for different passenger groups
WITH #SurvivalStats AS (
    SELECT
        Pclass,         -- Passenger class (1: First, 2: Second, 3: Third)
        Sex,           -- Passenger sex
        Age_Group,     -- Age group (0-17, 18-30, 31-45, 46-60, 61+)
        Survived,      -- Survival status (0: No, 1: Yes)
        COUNT(*) AS TotalPassengers,  -- Total number of passengers in the group
        SUM(CASE WHEN Survived = 1 THEN 1 ELSE 0 END) AS SurvivedPassengers,  -- Number of survived passengers in the group
        (SUM(CASE WHEN Survived = 1 THEN 1 ELSE 0 END) * 1.0 / COUNT(*)) AS SurvivalRate  -- Survival rate for the group
    FROM
        #TempTable
    WHERE
        Age IS NOT NULL  -- Exclude passengers with missing age
    GROUP BY
        Pclass, Sex, Age_Group, Survived
),

-- Create a CTE (#AgeGroups) to categorize passengers into age groups
#AgeGroups AS (
    SELECT
        PassengerId,  -- Passenger ID
        CASE
            WHEN Age <= 17 THEN '0-17'
            WHEN Age BETWEEN 18 AND 30 THEN '18-30'
            WHEN Age BETWEEN 31 AND 45 THEN '31-45'
            WHEN Age BETWEEN 46 AND 60 THEN '46-60'
            ELSE '61+'
        END AS Age_Group  -- Age group (0-17, 18-30, 31-45, 46-60, 61+)
    FROM
        #TempTable
    WHERE
        Age IS NOT NULL  -- Exclude passengers with missing age
)

-- Select and display the final result
SELECT
    #TempTable.PassengerId,  -- Passenger ID
    #TempTable.Survived,      -- Survival status (0: No, 1: Yes)
    #TempTable.Pclass,         -- Passenger class (1: First, 2: Second, 3: Third)
    #TempTable.Name,          -- Passenger name
    #TempTable.Sex,           -- Passenger sex
    #AgeGroups.Age_Group,     -- Age group (0-17, 18-30, 31-45, 46-60, 61+)
    #TempTable.SibSp,         -- Number of siblings and spouses aboard
    #TempTable.Parch,         -- Number of parents and children aboard
    #TempTable.Ticket,        -- Ticket number
    #TempTable.Fare,          -- Fare paid
    #TempTable.Cabin,         -- Cabin number
    #TempTable.Embarked,       -- Port of embarkation (C: Cherbourg, Q: Queenstown, S: Southampton)
    #SurvivalStats.SurvivalRate  -- Survival rate for the passenger's group
FROM
    #TempTable
JOIN
    #AgeGroups ON #TempTable.PassengerId = #AgeGroups.PassengerId
LEFT JOIN
    #SurvivalStats ON #AgeGroups.Age_Group = #SurvivalStats.Age_Group
    AND #TempTable.Pclass = #SurvivalStats.Pclass
    AND #TempTable.Sex = #SurvivalStats.Sex;
```

**Explanation:**

This SQL code performs a comprehensive analysis of the Titanic passenger data, providing detailed information about each passenger along with survival statistics for different passenger groups based on their class, sex, and age.

- **Temporary Table (#TempTable):**
  - This temporary table is created to store the original Titanic data with some additional calculated columns.

- **CTE #SurvivalStats:**
  - This CTE calculates survival statistics for different passenger groups based on their class, sex, and age group. It includes the total number of passengers, the number of survived passengers, and the survival rate for each group.

- **CTE #AgeGroups:**
  - This CTE categorizes passengers into different age groups based on their age. It assigns each passenger to an age group: '0-17', '18-30', '31-45', '46-60', or '61+'.

- **Final Result:**
  - The final result is obtained by joining the original data (#TempTable) with the age groups (#AgeGroups) and survival statistics (#SurvivalStats). This provides a comprehensive view of each passenger's information, including their survival status, class, sex, age group, and survival rate for their group.

This code offers a detailed analysis of the Titanic passenger data, allowing you to explore patterns, trends, and relationships among different passenger groups and their survival chances.