```sql
-- Common Table Expression (CTE) to calculate the running balance for each account
WITH RunningBalance AS (
    SELECT
        account_number,
        transaction_date,
        amount,
        SUM(amount) OVER (PARTITION BY account_number ORDER BY transaction_date) AS running_balance
    FROM transactions
),

-- CTE to calculate the average daily balance for each account
AverageDailyBalance AS (
    SELECT
        account_number,
        AVG(running_balance) OVER (PARTITION BY account_number) AS avg_daily_balance
    FROM RunningBalance
),

-- CTE to calculate the interest earned for each account
InterestEarned AS (
    SELECT
        account_number,
        avg_daily_balance * interest_rate AS interest_earned
    FROM AverageDailyBalance
    JOIN accounts ON AverageDailyBalance.account_number = accounts.account_number
),

-- Final query to retrieve the account information along with the calculated values
SELECT
    accounts.account_number,
    accounts.account_holder_name,
    accounts.account_type,
    accounts.interest_rate,
    InterestEarned.interest_earned
FROM accounts
JOIN InterestEarned ON accounts.account_number = InterestEarned.account_number;
```

Explanation:

1. **RunningBalance CTE:**
   - This CTE calculates the running balance for each account. It uses the `SUM()` function with the `OVER()` clause to calculate the cumulative sum of the transaction amounts for each account.


2. **AverageDailyBalance CTE:**
   - This CTE calculates the average daily balance for each account. It uses the `AVG()` function with the `OVER()` clause to calculate the average of the running balances for each account.


3. **InterestEarned CTE:**
   - This CTE calculates the interest earned for each account. It multiplies the average daily balance by the interest rate to calculate the interest earned.


4. **Final Query:**
   - The final query retrieves the account information along with the calculated values from the previous CTEs. It uses a `JOIN` operation to link the accounts table with the InterestEarned CTE based on the account number.

This code demonstrates the use of CTEs to perform complex calculations and retrieve the desired information from multiple tables. It calculates the running balance, average daily balance, and interest earned for each account in a single query.