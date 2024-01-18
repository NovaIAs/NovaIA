```shell
#!/bin/bash

# Define variables
USERNAME="username"
PASSWORD="password"
HOSTNAME="hostname"
PORT="port"
DATABASE="database"

# Create a function to connect to the database
connect_to_database() {
  # Connect to the database using the provided credentials
  mysql -u $USERNAME -p$PASSWORD -h $HOSTNAME -P $PORT $DATABASE
}

# Create a function to create a table
create_table() {
  # Create a table named 'users' with three columns: 'id', 'name', and 'email'
  connect_to_database <<EOF
CREATE TABLE IF NOT EXISTS users (
  id INT NOT NULL AUTO_INCREMENT,
  name VARCHAR(255) NOT NULL,
  email VARCHAR(255) NOT NULL,
  PRIMARY KEY (id)
);
EOF
}

# Create a function to insert data into the table
insert_data() {
  # Insert three rows of data into the 'users' table
  connect_to_database <<EOF
INSERT INTO users (name, email) VALUES
  ('John Doe', 'john.doe@example.com'),
  ('Jane Smith', 'jane.smith@example.com'),
  ('Michael Jones', 'michael.jones@example.com');
EOF
}

# Create a function to select data from the table
select_data() {
  # Select all rows from the 'users' table
  connect_to_database <<EOF
SELECT * FROM users;
EOF
}

# Create a function to update data in the table
update_data() {
  # Update the email address of the user with the id of 1
  connect_to_database <<EOF
UPDATE users SET email = 'new_email@example.com' WHERE id = 1;
EOF
}

# Create a function to delete data from the table
delete_data() {
  # Delete the user with the id of 2
  connect_to_database <<EOF
DELETE FROM users WHERE id = 2;
EOF
}

# Create a function to drop the table
drop_table() {
  # Drop the 'users' table
  connect_to_database <<EOF
DROP TABLE IF EXISTS users;
EOF
}

# Call the functions to perform the desired operations
create_table
insert_data
select_data
update_data
delete_data
drop_table
```

This script connects to a MySQL database using the provided credentials, creates a table named 'users', inserts three rows of data into the table, selects all rows from the table, updates the email address of the user with the id of 1, deletes the user with the id of 2, and finally drops the 'users' table.