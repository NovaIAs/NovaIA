```perl
use strict;
use warnings;

# Define a hash to store the book data
my %books = (
    'title' => 'The Hitchhiker's Guide to the Galaxy',
    'author' => 'Douglas Adams',
    'isbn' => '0-330-25864-8',
    'price' => 12.99,
    'stock' => 50
);

# Define a subroutine to print the book data
sub print_book_data {
    my $book = shift;

    print "Title: $book->{title}\n";
    print "Author: $book->{author}\n";
    print "ISBN: $book->{isbn}\n";
    print "Price: $book->{price}\n";
    print "Stock: $book->{stock}\n\n";
}

# Print the book data
print_book_data(\%books);

# Define a subroutine to update the book data
sub update_book_data {
    my $book = shift;
    my $field = shift;
    my $value = shift;

    $book->{$field} = $value;
}

# Update the book data
update_book_data(\%books, 'price', 14.99);

# Print the book data again
print_book_data(\%books);

# Define a subroutine to add a new book to the hash
sub add_book {
    my $title = shift;
    my $author = shift;
    my $isbn = shift;
    my $price = shift;
    my $stock = shift;

    my %new_book = (
        'title' => $title,
        'author' => $author,
        'isbn' => $isbn,
        'price' => $price,
        'stock' => $stock
    );

    $books{$isbn} = \%new_book;
}

# Add a new book to the hash
add_book('The Lord of the Rings', 'J.R.R. Tolkien', '0-395-08256-0', 19.99, 30);

# Print the book data for the new book
print_book_data($books{'0-395-08256-0'});

# Define a subroutine to remove a book from the hash
sub remove_book {
    my $isbn = shift;

    delete $books{$isbn};
}

# Remove a book from the hash
remove_book('0-330-25864-8');

# Print the book data for the remaining books
foreach my $isbn (keys %books) {
    print_book_data($books{$isbn});
}
```

This code defines a hash to store book data, including the title, author, ISBN, price, and stock. It then defines subroutines to print the book data, update the book data, add a new book to the hash, and remove a book from the hash. The code then uses these subroutines to add, update, and remove books from the hash, and to print the book data for the remaining books.

Here is a more detailed explanation of the code:

* The `use strict` and `use warnings` statements are used to enable strict variable checking and warnings.
* The `%books` hash is defined to store the book data.
* The `print_book_data()` subroutine is defined to print the book data.
* The `update_book_data()` subroutine is defined to update the book data.
* The `add_book()` subroutine is defined to add a new book to the hash.
* The `remove_book()` subroutine is defined to remove a book from the hash.
* The code then uses the `add_book()` subroutine to add a new book to the hash.
* The code then uses the `print_book_data()` subroutine to print the book data for the new book.
* The code then uses the `remove_book()` subroutine to remove a book from the hash.
* The code then uses the `print_book_data()` subroutine to print the book data for the remaining books.