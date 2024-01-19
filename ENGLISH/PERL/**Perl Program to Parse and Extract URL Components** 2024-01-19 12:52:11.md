```perl
use strict;
use warnings;
use feature 'say';

# Define a complex regular expression to match various parts of a string
my $regex = qr/(?<protocol>\w+):\/\/(?<domain>[a-zA-Z0-9\-\.]+)(?<path>\/[a-zA-Z0-9\-\._~%\/]+)(?<query>\?[a-zA-Z0-9\-_.~%\/=&]+)?(?<fragment>\#[a-zA-Z0-9\-\._~%\/]+)?/;

# Define a subroutine to extract and display the parts of a URL
sub parse_url {
    my $url = shift;
    say "URL: $url";

    # Check if the URL matches the regular expression
    if ($url =~ $regex) {
        # If it matches, extract the parts of the URL using named captures
        my $protocol = $+{protocol};
        my $domain = $+{domain};
        my $path = $+{path};
        my $query = $+{query};
        my $fragment = $+{fragment};

        # Display the extracted parts
        say "Protocol: $protocol";
        say "Domain: $domain";
        say "Path: $path";
        say "Query: $query";
        say "Fragment: $fragment";
    } else {
        # If it doesn't match, indicate that the URL is invalid
        say "Invalid URL";
    }
}

# Test the subroutine with different URLs
parse_url("https://www.example.com/path/to/resource?query=string#fragment");
parse_url("ftp://ftp.example.com/pub/file.txt");
parse_url("mailto:user@example.com");
parse_url("invalid URL");

```

**Explanation:**

* The code uses the `strict` and `warnings` modules to ensure that the code is written in a strict and consistent manner, and to catch potential errors early.

* The `feature 'say'` module enables the use of the `say` function, which is a more concise and readable way to print output.

* A complex regular expression is defined using the `qr//` operator. The regular expression is designed to match various parts of a URL, including the protocol, domain, path, query, and fragment.

* A subroutine named `parse_url()` is defined. This subroutine takes a URL as an argument and extracts the different parts of the URL using named captures.

* The `parse_url()` subroutine is tested with different URLs, and the extracted parts of the URLs are displayed.

* If the URL is invalid and does not match the regular expression, the subroutine indicates that the URL is invalid.