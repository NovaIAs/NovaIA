```tcl
package require Tcl 8.6

proc randstring {length} {
    set chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    return [lmap i [range $length] {string map {$chars} [expr {[rand() * 62]} % [llength $chars]]}]]
}

proc randint {low high} {
    expr {int(rand() * [expr {$high - $low + 1}] + $low)}
}

proc generate_data {num_records} {
    set data {}
    for {set i 0} {$i < $num_records} {incr i} {
        lappend data [format {
            "%s,%s,%s,%d,%d,%d"
            [randstring {randint 5 15}] [randstring {randint 5 15}]
            [randstring {randint 5 15}] [randint 18 70]
            [randint 1000 9999] [randint 100000 999999]}]]
    }
    return $data
}

# Create a SQLite3 database and table
sqlite3 data.db {
    create table records (
        first_name text, last_name text, email text,
        age integer, zip integer, phone integer
    );
    .separator ;
    .import data.csv records
    .separator \n
}

# Retrieve and display the data from the database
sqlite3 data.db {
    select * from records;
}
```

This code generates a large dataset of random records and inserts them into a SQLite3 database. It then retrieves and displays the data from the database.

Here's a breakdown of the code:

1. **Generating the Data:**
   - The `generate_data` procedure generates a specified number of random records.
   - Each record consists of six fields: First name, last name, email, age, zip code, and phone number.
   - The fields are generated using the `randstring` and `randint` procedures.

2. **Creating the Database and Table:**
   - The `sqlite3` command is used to create a SQLite3 database named `data.db`.
   - Inside the `sqlite3` block, the `create table` statement is used to create a table named `records` with the appropriate columns.

3. **Importing the Data:**
   - The `.import` statement is used to import the generated data from the `data.csv` file into the `records` table.

4. **Retrieving and Displaying the Data:**
   - Another `sqlite3` block is used to retrieve all the records from the `records` table using the `select * from records` statement.
   - The results are displayed on the console.

This code demonstrates how to generate a large dataset, store it in a SQLite3 database, and retrieve and display the data using Tcl.