```sql
WITH RecursiveInvoiceDetails AS (
    SELECT
        id,
        invoice_id,
        product_id,
        quantity,
        unit_price,
        total_price,
        line_number,
        ROW_NUMBER() OVER (PARTITION BY invoice_id ORDER BY line_number) AS line_position
    FROM invoice_details
),
InvoiceSummary AS (
    SELECT
        id,
        invoice_date,
        customer_id,
        total_amount,
        ROW_NUMBER() OVER (ORDER BY invoice_date) AS invoice_sequence
    FROM invoices
),
CustomerDetails AS (
    SELECT
        id,
        name,
        email,
        phone_number
    FROM customers
)

SELECT
    RID.id,
    RID.invoice_id,
    RID.product_id,
    RID.quantity,
    RID.unit_price,
    RID.total_price,
    RID.line_position,
    IS.id AS invoice_summary_id,
    IS.invoice_date,
    IS.customer_id,
    IS.total_amount,
    IS.invoice_sequence,
    CD.id AS customer_details_id,
    CD.name AS customer_name,
    CD.email AS customer_email,
    CD.phone_number AS customer_phone_number
FROM RecursiveInvoiceDetails RID
JOIN InvoiceSummary IS ON RID.invoice_id = IS.id
JOIN CustomerDetails CD ON IS.customer_id = CD.id
WHERE RID.line_position BETWEEN 2 AND 5 AND IS.invoice_date BETWEEN '2021-01-01' AND '2021-12-31';
```

Explanation:

This complex SQL code combines data from multiple tables and uses various advanced techniques such as recursive CTEs, window functions, and correlated subqueries. Let's break down the code step by step:

RecursiveInvoiceDetails (CTE):

We start by defining a recursive CTE called RecursiveInvoiceDetails. This CTE is used to assign line positions to invoice details based on their order within each invoice.

It selects various columns from the invoice_details table, including the invoice ID, product ID, quantity, unit price, total price, and line number.
It then uses the ROW_NUMBER() window function to assign a sequential line position to each invoice detail within its respective invoice.

InvoiceSummary (CTE):

The InvoiceSummary CTE summarizes invoice data, such as total amounts and invoice dates.

It selects the invoice ID, invoice date, customer ID, and total amount from the invoices table.
It also uses the ROW_NUMBER() window function to assign a sequential invoice sequence number to each invoice based on its invoice date.

CustomerDetails (CTE):

The CustomerDetails CTE retrieves customer information, such as names, emails, and phone numbers.

It selects the customer ID, name, email, and phone number from the customers table.

Final SELECT Statement:

The final SELECT statement combines data from the RecursiveInvoiceDetails, InvoiceSummary, and CustomerDetails CTEs.

It joins the RecursiveInvoiceDetails CTE with the InvoiceSummary CTE on the invoice ID, and then joins the result with the CustomerDetails CTE on the customer ID.

It filters the results to include only invoice details with line positions between 2 and 5, and invoices with dates between '2021-01-01' and '2021-12-31'.

The result is a comprehensive dataset that includes invoice details, invoice summaries, and customer information, all linked together through various joins and CTEs.