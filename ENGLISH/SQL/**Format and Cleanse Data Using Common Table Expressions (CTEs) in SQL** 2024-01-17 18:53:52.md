```sql
-- Create a common table expression (CTE) called 'SourceData' to hold the input data
WITH SourceData AS (
    SELECT
        'John Doe' AS FullName,
        '123 Main Street' AS Address,
        '1234567890' AS PhoneNumber,
        'johndoe@example.com' AS EmailAddress,
        25 AS Age,
        'Programmer' AS Occupation
    UNION ALL
    SELECT
        'Jane Smith',
        '456 Elm Street',
        '2345678901',
        'janesmith@example.com',
        30,
        'Accountant'
    UNION ALL
    SELECT
        'Michael Jones',
        '789 Oak Street',
        '3456789012',
        'michaeljones@example.com',
        35,
        'Engineer'
)

-- Create a CTE called 'CleansedData' to remove any leading or trailing spaces from the data
WITH CleansedData AS (
    SELECT
        TRIM(FullName) AS FullName,
        TRIM(Address) AS Address,
        TRIM(PhoneNumber) AS PhoneNumber,
        TRIM(EmailAddress) AS EmailAddress,
        Age,
        TRIM(Occupation) AS Occupation
    FROM SourceData
)

-- Create a CTE called 'UniqueData' to remove any duplicate records based on FullName and EmailAddress
WITH UniqueData AS (
    SELECT DISTINCT FullName, Address, PhoneNumber, EmailAddress, Age, Occupation
    FROM CleansedData
)

-- Create a CTE called 'FormattedData' to format the data in a consistent manner
WITH FormattedData AS (
    SELECT
        FullName,
        Address,
        PhoneNumber,
        EmailAddress,
        CASE
            WHEN Age >= 18 AND Age < 65 THEN 'Adult'
            WHEN Age >= 65 THEN 'Senior'
            ELSE 'Minor'
        END AS AgeCategory,
        Occupation
    FROM UniqueData
)

-- Retrieve the formatted data from the 'FormattedData' CTE
SELECT * FROM FormattedData;
```

Explanation:

1. **SourceData CTE:** This CTE creates a temporary table called 'SourceData' to hold the input data. It includes six columns: 'FullName', 'Address', 'PhoneNumber', 'EmailAddress', 'Age', and 'Occupation'.

2. **CleansedData CTE:** This CTE removes any leading or trailing spaces from the data in the 'SourceData' CTE. This ensures that the data is consistent and easier to work with.

3. **UniqueData CTE:** This CTE removes any duplicate records from the 'CleansedData' CTE based on the 'FullName' and 'EmailAddress' columns. This ensures that each record represents a unique individual.

4. **FormattedData CTE:** This CTE formats the data in the 'UniqueData' CTE in a consistent manner. It includes the following columns:
   - 'FullName': The full name of the individual.
   - 'Address': The address of the individual.
   - 'PhoneNumber': The phone number of the individual.
   - 'EmailAddress': The email address of the individual.
   - 'AgeCategory': The age category of the individual ('Adult', 'Senior', or 'Minor').
   - 'Occupation': The occupation of the individual.

5. **Final Query:** The final query retrieves all the formatted data from the 'FormattedData' CTE. This query returns a result set that contains the formatted data for each individual in the input data.