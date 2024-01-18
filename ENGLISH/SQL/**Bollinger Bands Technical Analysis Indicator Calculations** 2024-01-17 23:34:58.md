```sql
WITH StockData AS (
    SELECT
        Symbol,
        Date,
        Open,
        High,
        Low,
        Close,
        Volume
    FROM
        stock_prices
    WHERE
        Date BETWEEN '2020-01-01' AND '2022-12-31'
),

DailyReturns AS (
    SELECT
        Symbol,
        Date,
        (Close - Open) / Open AS DailyReturn
    FROM
        StockData
),

MovingAverages AS (
    SELECT
        Symbol,
        Date,
        AVG(DailyReturn) OVER (PARTITION BY Symbol ORDER BY Date ASC ROWS BETWEEN 49 AND 200) AS MA50,
        AVG(DailyReturn) OVER (PARTITION BY Symbol ORDER BY Date ASC ROWS BETWEEN 199 AND 250) AS MA200
    FROM
        DailyReturns
),

BollingerBands AS (
    SELECT
        Symbol,
        Date,
        MA50,
        MA200,
        STDDEV(DailyReturn) OVER (PARTITION BY Symbol ORDER BY Date ASC ROWS BETWEEN 49 AND 200) AS StdDev50,
        STDDEV(DailyReturn) OVER (PARTITION BY Symbol ORDER BY Date ASC ROWS BETWEEN 199 AND 250) AS StdDev200
    FROM
        MovingAverages
)

SELECT
    Symbol,
    Date,
    Close,
    MA50,
    MA200,
    StdDev50,
    StdDev200,
    (Close - MA50) / StdDev50 AS BollingerBands50,
    (Close - MA200) / StdDev200 AS BollingerBands200
FROM
    BollingerBands;
```

Explanation:

1. **StockData:** This CTE selects stock price data for a specified date range from the 'stock_prices' table.

2. **DailyReturns:** This CTE calculates the daily returns for each stock by subtracting the open price from the close price and dividing by the open price.

3. **MovingAverages:** This CTE calculates moving averages (MA) of the daily returns for two different periods: 50 days (MA50) and 200 days (MA200).

4. **BollingerBands:** This CTE calculates the Bollinger Bands for each stock using the MA50, MA200, and the standard deviation of the daily returns for both periods.

5. The final SELECT statement retrieves the symbol, date, closing price, moving averages, standard deviations, and Bollinger Bands values for each stock.