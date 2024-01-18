```sql
-- Create a temporary table to store the results of the subquery
WITH RankedColleges AS (
    SELECT
        c.college_id,
        c.college_name,
        c.state,
        c.tuition,
        c.acceptance_rate,
        c.graduation_rate,
        -- Calculate the rank of each college based on their acceptance rate
        RANK() OVER (ORDER BY c.acceptance_rate) AS acceptance_rank,
        -- Calculate the rank of each college based on their graduation rate
        RANK() OVER (ORDER BY c.graduation_rate) AS graduation_rank,
        -- Calculate the rank of each college based on their tuition
        RANK() OVER (ORDER BY c.tuition) AS tuition_rank
    FROM
        colleges AS c
)

-- Select colleges that meet the following criteria:
-- 1. Acceptance rate in the top 25%
-- 2. Graduation rate in the top 50%
-- 3. Tuition in the bottom 50%
SELECT
    rc.college_id,
    rc.college_name,
    rc.state,
    rc.tuition,
    rc.acceptance_rate,
    rc.graduation_rate
FROM
    RankedColleges AS rc
WHERE
    rc.acceptance_rank <= 0.25 AND
    rc.graduation_rank <= 0.50 AND
    rc.tuition_rank >= 0.50;
```

Explanation:

1. **Create a Temporary Table (RankedColleges):**
   - We use a CTE (Common Table Expression) to create a temporary table called RankedColleges.
   - This table contains all the columns from the colleges table, along with three additional columns:
     - `acceptance_rank`: Rank of each college based on acceptance rate (lower is better).
     - `graduation_rank`: Rank of each college based on graduation rate (higher is better).
     - `tuition_rank`: Rank of each college based on tuition (lower is better).

2. **Calculate Ranks:**
   - We use the `RANK()` function to calculate the ranks for each college based on acceptance rate, graduation rate, and tuition.
   - The `RANK()` function assigns a rank to each row within a group, with 1 being the highest rank and larger numbers indicating lower ranks.

3. **Select Colleges Meeting the Criteria:**
   - We use a `SELECT` statement to retrieve data from the RankedColleges table.
   - We include columns for college ID, name, state, tuition, acceptance rate, and graduation rate.

4. **Filter Colleges:**
   - We use a `WHERE` clause to filter the colleges based on the following criteria:
     - `acceptance_rank <= 0.25`: Acceptance rate must be in the top 25%.
     - `graduation_rank <= 0.50`: Graduation rate must be in the top 50%.
     - `tuition_rank >= 0.50`: Tuition must be in the bottom 50%.

This query retrieves a list of colleges that have a low acceptance rate, a high graduation rate, and affordable tuition, satisfying all the given requirements.