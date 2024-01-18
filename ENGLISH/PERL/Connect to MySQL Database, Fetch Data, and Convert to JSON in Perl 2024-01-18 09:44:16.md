```perl
# Import necessary modules
use strict;
use warnings;
use DBI;
use JSON;

# Database connection parameters
my $host = 'localhost';
my $database = 'mydb';
my $username = 'root';
my $password = 'password';

# Connect to the database
my $dbh = DBI->connect("DBI:mysql:$database:$host", $username, $password);

# SQL query to fetch data from the database
my $sql = 'SELECT * FROM users';

# Prepare the SQL statement
my $sth = $dbh->prepare($sql);

# Execute the prepared SQL statement
$sth->execute();

# Fetch all rows from the result set
my @rows = $sth->fetchall_arrayref();

# Convert the array of rows to JSON format
my $json = encode_json \@rows;

# Print the JSON string
print $json;

# Close the database connection
$dbh->disconnect();
```

This is a Perl script that connects to a MySQL database, fetches all rows from the 'users' table, converts the rows to JSON format, and prints the JSON string.

Here's a breakdown of the code:

1. **Import necessary modules:** The `use` statements at the beginning of the script import the necessary Perl modules that will be used in the script.

2. **Database connection parameters:** These variables are used to specify the parameters for connecting to the MySQL database.

3. **Connect to the database:** This code connects to the MySQL database using the DBI module.

4. **SQL query to fetch data from the database:** This line defines the SQL query that will be used to fetch all rows from the 'users' table in the database.

5. **Prepare the SQL statement:** This code prepares the SQL statement for execution. Preparing a statement is a good practice for securing the database from SQL injection attacks.

6. **Execute the prepared SQL statement:** This code executes the prepared SQL statement and fetches all rows from the result set.

7. **Fetch all rows from the result set:** This line fetches all rows from the result set and stores them in an array of arrays.

8. **Convert the array of rows to JSON format:** This code converts the array of rows to JSON format using the JSON module.

9. **Print the JSON string:** This code prints the JSON string to the console.

10. **Close the database connection:** This code closes the connection to the MySQL database.