Certainly! Here's a complex and differentiated SQL code that tackles a fictitious e-commerce scenario. The code includes tables for customers, products, orders, and reviews, and demonstrates various SQL concepts such as data modeling, data manipulation, and data retrieval.

```sql
-- Create the table for customers
CREATE TABLE customers (
  customer_id INT PRIMARY KEY,
  first_name VARCHAR(50),
  last_name VARCHAR(50),
  email VARCHAR(100),
  phone VARCHAR(20)
);

-- Create the table for products
CREATE TABLE products (
  product_id INT PRIMARY KEY,
  name VARCHAR(100),
  description TEXT,
  price DECIMAL(10, 2),
  stock_quantity INT
);

-- Create the table for orders
CREATE TABLE orders (
  order_id INT PRIMARY KEY,
  customer_id INT,
  order_date DATE,
  FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);

-- Create the table for reviews
CREATE TABLE reviews (
  review_id INT PRIMARY KEY,
  product_id INT,
  customer_id INT,
  rating INT,
  comment TEXT,
  FOREIGN KEY (product_id) REFERENCES products(product_id),
  FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);

-- Insert sample data into customers table
INSERT INTO customers (customer_id, first_name, last_name, email, phone)
VALUES
  (1, 'John', 'Doe', 'john.doe@example.com', '1234567890'),
  (2, 'Jane', 'Smith', 'jane.smith@example.com', '0987654321');

-- Insert sample data into products table
INSERT INTO products (product_id, name, description, price, stock_quantity)
VALUES
  (1, 'Product 1', 'Description of Product 1', 19.99, 100),
  (2, 'Product 2', 'Description of Product 2', 29.99, 50),
  (3, 'Product 3', 'Description of Product 3', 9.99, 200);

-- Insert sample data into orders table
INSERT INTO orders (order_id, customer_id, order_date)
VALUES
  (1, 1, '2022-01-01'),
  (2, 1, '2022-02-15'),
  (3, 2, '2022-03-10');

-- Insert sample data into reviews table
INSERT INTO reviews (review_id, product_id, customer_id, rating, comment)
VALUES
  (1, 1, 1, 5, 'Great product!'),
  (2, 1, 2, 4, 'Good product, but a bit expensive.'),
  (3, 2, 1, 3, 'Average product.');

-- Retrieve all customers who placed an order
SELECT c.first_name, c.last_name, o.order_date
FROM customers c
INNER JOIN orders o ON c.customer_id = o.customer_id;

-- Retrieve products with average rating higher than 3
SELECT p.name, AVG(r.rating) AS average_rating
FROM products p
LEFT JOIN reviews r ON p.product_id = r.product_id
GROUP BY p.name
HAVING AVG(r.rating) > 3;
```

This SQL code creates four tables: `customers`, `products`, `orders`, and `reviews`. The tables represent customers, products, customer orders, and product reviews, respectively.

The code defines the structure of each table using `CREATE TABLE` statements. It specifies the columns, data types, primary keys, and foreign keys for the tables.

Sample data is then inserted into each table using `INSERT INTO` statements.

Finally, two example queries are provided:

1. Retrieve all customers who placed an order: This query uses an inner join between the `customers` and `orders` tables to fetch the customer's first name, last name, and order date.

2. Retrieve products with an average rating higher than 3: This query utilizes a left join between the `products` and `reviews` tables to calculate the average rating for each product. The result includes the product name and its average rating, but only for products with an average rating greater than 3.

These examples demonstrate how data can be modeled, inserted, and queried in a relational database using SQL.