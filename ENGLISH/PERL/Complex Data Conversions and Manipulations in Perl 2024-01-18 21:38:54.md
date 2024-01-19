```perl
use strict;
use warnings;
use feature 'say';

my %data = (
    'name' => 'John Doe',
    'age' => 30,
    'city' => 'New York',
    'state' => 'NY',
    'zip' => '10001',
    'phone' => '212-555-1212',
    'email' => 'john.doe@example.com',
);

my $csv_string = join(', ', map { "\"$data{$_}\"" } keys %data);
say "CSV String: $csv_string";

my @csv_array = split(/, /, $csv_string);
say "CSV Array: @csv_array";

my %csv_hash = map { m/\"(.*?)\"/ ? $1 : undef } @csv_array;
say "CSV Hash: %csv_hash";

my $json_string = encode_json \%data;
say "JSON String: $json_string";

my $json_hash = decode_json $json_string;
say "JSON Hash: %json_hash";

sub encode_json {
    my $hash_ref = shift;
    my $json_string = '';
    foreach my $key (keys %$hash_ref) {
        my $value = $hash_ref->{$key};
        if (ref $value eq 'HASH') {
            $json_string .= "\"$key\": " . encode_json($value) . ',';
        } elsif (ref $value eq 'ARRAY') {
            $json_string .= "\"$key\": [" . join(', ', map { encode_json($_) } @$value) . '],';
        } else {
            $json_string .= "\"$key\": \"$value\",";
        }
    }
    $json_string =~ s/, $//;
    return "{$json_string}";
}

sub decode_json {
    my $json_string = shift;
    my $json_hash = {};
    my @json_pairs = split(/,/, $json_string);
    foreach my $json_pair (@json_pairs) {
        my ($key, $value) = $json_pair =~ m/^\"(.*?)\": (.*)$/;
        $value =~ s/^\"(.*?)\"$//;
        if ($value =~ m/^\[(.*)\]$/) {
            $value = [split(/, /, $1)];
        } elsif ($value eq 'true') {
            $value = 1;
        } elsif ($value eq 'false') {
            $value = 0;
        } elsif ($value =~ m/^-?\d+$/) {
            $value = int($value);
        } elsif ($value =~ m/^-?\d+\.\d+$/) {
            $value = float($value);
        }
        $json_hash->{$key} = $value;
    }
    return $json_hash;
}
```

This code demonstrates a variety of complex data processing tasks in Perl. It takes a hash of data, converts it to a CSV string, an array, and a hash back. Then it converts the hash to a JSON string and back to a hash again. The code uses custom encode_json and decode_json subroutines to perform the JSON conversions.

Here's a breakdown of the code:

1. **Data Initialization:**

    ```perl
    my %data = (
        'name' => 'John Doe',
        'age' => 30,
        'city' => 'New York',
        'state' => 'NY',
        'zip' => '10001',
        'phone' => '212-555-1212',
        'email' => 'john.doe@example.com',
    );
    ```

    This creates a hash of data representing a person's information.

2. **CSV Conversion:**

    ```perl
    my $csv_string = join(', ', map { "\"$data{$_}\"" } keys %data);
    my @csv_array = split(/, /, $csv_string);
    my %csv_hash = map { m/\"(.*?)\"/ ? $1 : undef } @csv_array;
    ```

    This converts the hash to a CSV string by joining the values of the hash keys separated by commas and double quotes. Then it splits the CSV string into an array and converts it back to a hash by extracting the values from the array.

3. **JSON Conversion:**

    ```perl
    my $json_string = encode_json \%data;
    my $json_hash = decode_json $json_string;
    ```

    This converts the hash to a JSON string using the encode_json subroutine. Then it converts the JSON string back to a hash using the decode_json subroutine.

4. **encode_json and decode_json Subroutines:**

    These subroutines are custom functions used to convert hashes to JSON strings and vice versa. They handle special cases like nested hashes, arrays, and numeric values.

By combining these techniques, this code demonstrates how to perform complex data conversions and manipulations in Perl.