```haskell
import Data.List (sortBy, group, sort)
import Data.Char (toUpper)

-- Define a custom type to represent a book, with fields for title, author, and year of publication.
data Book = Book {
    title :: String,
    author :: String,
    year :: Int
} deriving (Eq, Ord, Show)

-- Define a function to compare two books by their publication year.
compareBooks :: Book -> Book -> Ordering
compareBooks b1 b2 = compare (year b1) (year b2)

-- Define a function to group books by their author.
groupByAuthor :: [Book] -> [[Book]]
groupByAuthor books = groupBy (\b1 b2 -> author b1 == author b2) books

-- Define a function to sort books by their title.
sortByTitle :: [Book] -> [Book]
sortByTitle books = sortBy (\b1 b2 -> compare (title b1) (title b2)) books

-- Define a function to convert a list of books to a string, with each book on a separate line.
booksToString :: [Book] -> String
booksToString books = unlines (map (\b -> show b) books)

-- Define a function to convert a list of books to a string, with each book's title in uppercase on a separate line.
booksTitlesToUppercase :: [Book] -> String
booksTitlesToUppercase books = unlines (map (\b -> toUpper (title b)) books)

-- Define a function to find the most recent book by a given author.
mostRecentBookByAuthor :: String -> [Book] -> Book
mostRecentBookByAuthor author books = head (sortBy compareBooks (filter (\b -> author b == author) books))

-- Define a function to find the most popular author, based on the number of books they have written.
mostPopularAuthor :: [Book] -> String
mostPopularAuthor books = author (head (sortBy (\(a1, b1) (a2, b2) -> compare (length b1) (length b2)) (groupByAuthor books)))

-- Define a function to find the most popular book, based on the number of times it has been checked out of a library.
mostPopularBook :: [Book] -> Book
mostPopularBook books = head (sortBy (\b1 b2 -> compare (length (checkouts b1)) (length (checkouts b2))) books)

-- Define a function to find the most popular book by a given author.
mostPopularBookByAuthor :: String -> [Book] -> Book
mostPopularBookByAuthor author books = head (sortBy (\b1 b2 -> compare (length (checkouts b1)) (length (checkouts b2))) (filter (\b -> author b == author) books))

-- Define a function to find the most popular genre, based on the number of books in that genre.
mostPopularGenre :: [Book] -> String
mostPopularGenre books = head (sortBy (\(g1, b1) (g2, b2) -> compare (length b1) (length b2)) (groupBy (\b1 b2 -> genre b1 == genre b2) books))

-- Define a function to find the most popular book in a given genre.
mostPopularBookInGenre :: String -> [Book] -> Book
mostPopularBookInGenre genre books = head (sortBy (\b1 b2 -> compare (length (checkouts b1)) (length (checkouts b2))) (filter (\b -> genre b == genre) books))

-- Define a function to find the most popular author in a given genre.
mostPopularAuthorInGenre :: String -> [Book] -> String
mostPopularAuthorInGenre genre books = author (head (sortBy (\(a1, b1) (a2, b2) -> compare (length b1) (length b2)) (groupByAuthor (filter (\b -> genre b == genre) books))))

-- Define a function to find the most popular book by a given author in a given genre.
mostPopularBookByAuthorInGenre :: String -> String -> [Book] -> Book
mostPopularBookByAuthorInGenre author genre books = head (sortBy (\b1 b2 -> compare (length (checkouts b1)) (length (checkouts b2))) (filter (\b -> author b == author && genre b == genre) books))

-- Define a function to generate a report on the library's collection, including the most popular book, author, and genre.
generateReport :: [Book] -> String
generateReport books =
    "Most Popular Book: " ++ show (mostPopularBook books) ++ "\n" ++
    "Most Popular Author: " ++ mostPopularAuthor books ++ "\n" ++
    "Most Popular Genre: " ++ mostPopularGenre books ++ "\n"

-- Define a function to run the program.
main :: IO ()
main = do
    -- Create a list of books.
    let books = [
        Book "The Lord of the Rings" "J.R.R. Tolkien" 1954,
        Book "The Hobbit" "J.R.R. Tolkien" 1937,
        Book "Harry Potter and the Sorcerer's Stone" "J.K. Rowling" 1997,
        Book "Harry Potter and the Chamber of Secrets" "J.K. Rowling" 1998,
        Book "Harry Potter and the Prisoner of Azkaban" "J.K. Rowling" 1999,
        Book "The Hunger Games" "Suzanne Collins" 2008,
        Book "Catching Fire" "Suzanne Collins" 2009,
        Book "Mockingjay" "Suzanne Collins" 2010,
        Book "The Fault in Our Stars" "John Green" 2012,
        Book "Paper Towns" "John Green" 2008
    ]

    -- Print the list of books.
    putStrLn "Books:"
    putStrLn (booksToString books)

    -- Print the list of books with their titles in uppercase.
    putStrLn "Books with Titles in Uppercase:"
    putStrLn (booksTitlesToUppercase books)

    -- Find the most recent book by J.R.R. Tolkien.
    let mostRecentTolkienBook = mostRecentBookByAuthor "J.R.R. Tolkien" books
    putStrLn "Most Recent Book by J.R.R. Tolkien:"
    putStrLn (show mostRecentTolkienBook)

    -- Find the most popular author.
    let mostPopularAuthorName = mostPopularAuthor books
    putStrLn "Most Popular Author:"
    putStrLn mostPopularAuthorName

    -- Find the most popular book.
    let mostPopularBookName = mostPopularBook books
    putStrLn "Most Popular Book:"
    putStrLn (show mostPopularBookName)

    -- Find the most popular book by J.K. Rowling.
    let mostPopularRowlingBook = mostPopularBookByAuthor "J.K. Rowling" books
    putStrLn "Most Popular Book by J.K. Rowling:"
    putStrLn (show mostPopularRowlingBook)

    -- Find the most popular genre.
    let mostPopularGenreName = mostPopularGenre books
    putStrLn "Most Popular Genre:"
    putStrLn mostPopularGenreName

    -- Find the most popular book in the Fantasy genre.
    let mostPopularFantasyBook = mostPopularBookInGenre "Fantasy" books
    putStrLn "Most Popular Book in the Fantasy Genre:"
    putStrLn (show mostPopularFantasyBook)

    -- Find the most popular author in the Fantasy genre.
    let mostPopularFantasyAuthor = mostPopularAuthorInGenre "Fantasy" books
    putStrLn "Most Popular Author in the Fantasy Genre:"
    putStrLn mostPopularFantasyAuthor

    -- Find the most popular book by J.R.R. Tolkien in the Fantasy genre.
    let mostPopularTolkienFantasyBook = mostPopularBookByAuthorInGenre "J.R.R. Tolkien" "Fantasy" books
    putStrLn "Most Popular Book by J.R.R. Tolkien in the Fantasy Genre:"
    putStrLn (show mostPopularTolkienFantasyBook)

    -- Generate a report on the library's collection.
    let report = generateReport books
    putStrLn "Library Collection Report:"
    putStrLn report
```

This code is a library management system that allows users to perform various operations on a list of books. The code includes functions to:

* Compare books by their publication year.
* Group books by their author.
* Sort books by their title.
* Convert a list of books to a string.
* Convert a list of books to a string with their titles in uppercase.
* Find the most recent book by a given author.
* Find the most popular author.
* Find the most popular book.
* Find the most popular book by a given author.
* Find the most popular genre.
* Find the most popular book in a given genre.
* Find the most popular author in a given genre.
* Find the most popular book by a given author in a given genre.
* Generate a report on the library's collection, including the most popular book, author, and genre.

The code