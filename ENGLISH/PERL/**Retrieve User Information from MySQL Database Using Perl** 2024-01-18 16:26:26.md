```PERL
use strict;
use warnings;

my $database = 'mydb';
my $host = 'localhost';
my $username = 'root';
my $password = 'password';

my $dbh = DBI->connect("DBI:mysql:database=$database;host=$host", $username, $password, {AutoCommit => 0}) or die $DBI::errstr;

my $sth = $dbh->prepare('SELECT * FROM users WHERE username = ?') or die $dbh->errstr;
$sth->execute('john') or die $sth->errstr;

while (my @row = $sth->fetchrow_array) {
    print "@row\n";
}

$sth->finish;
$dbh->disconnect;
```

This code connects to a MySQL database using the DBI module and retrieves all rows from the `users` table where the `username` column is equal to `john`.

The code first loads the strict and warnings modules, which help to catch errors and typos.

Next, it defines the database connection parameters, including the database name, host, username, and password.

The code then connects to the database using the DBI module and prepares a SQL statement to select all rows from the `users` table where the `username` column is equal to `john`.

The prepared statement is then executed, and a loop is used to fetch each row from the result set and print it to the console.

Finally, the prepared statement and database connection are closed.