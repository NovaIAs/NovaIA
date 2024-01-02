Sure! Here's a complex Perl code that generates a random password for each user and stores it securely using encryption techniques. The code also includes functionality to validate the strength of the password based on certain criteria.

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Crypt::Eksblowfish::Bcrypt;

sub generate_password {
    my $length = shift;
    my @chars = ('A'..'Z', 'a'..'z', 0..9, qw(! @ # $ % ^ & *));
    my $password = '';
    foreach (1..$length) {
        $password .= $chars[rand @chars];
    }
    return $password;
}

sub encrypt_password {
    my $password = shift;
    my $cost = shift || 10;
    my $salt = Crypt::Eksblowfish::Bcrypt::en_base64(
        substr(Crypt::Eksblowfish::Bcrypt::random_salt(), 0, 16)
    );
    my $encrypted_password = Crypt::Eksblowfish::Bcrypt::bcrypt_hash({
        key_nul => 1,
        cost    => $cost,
        salt    => $salt,
    }, $password);
    return ($encrypted_password, $salt);
}

sub validate_password_strength {
    my $password = shift;
    my $lowercase = $password =~ /[a-z]/;
    my $uppercase = $password =~ /[A-Z]/;
    my $digit = $password =~ /\d/;
    my $special_char = $password =~ /[!@#$%^&*]/;
    my $length = length $password;
    my $strong = $lowercase && $uppercase && $digit && $special_char && $length >= 8;
    return $strong;
}

# Example usage
my $user = 'example_user';
my $password = generate_password(12);
my ($encrypted_password, $salt) = encrypt_password($password);
my $strong_password = validate_password_strength($password);

print "Username: $user\n";
print "Generated Password: $password\n";
print "Encrypted Password: $encrypted_password\n";
print "Salt: $salt\n";
print "Password Strength: " . ($strong_password ? 'Strong' : 'Weak') . "\n";
```

Explanation:
- The code starts by including the necessary modules, `strict` and `warnings`, to enforce good programming practices.
- The `generate_password` subroutine takes a length parameter and generates a random password of that length using a combination of uppercase letters, lowercase letters, digits, and special characters.
- The `encrypt_password` subroutine takes a password and an optional cost parameter (default is 10) to determine the computational cost of the encryption. It generates a random salt, encrypts the password using bcrypt algorithm, and returns the encrypted password and salt.
- The `validate_password_strength` subroutine takes a password and checks if it meets certain criteria for password strength. It checks for the presence of lowercase, uppercase, digit, special character, and a minimum length of 8 characters. It returns a boolean value indicating the strength of the password.
- The example usage portion demonstrates how to generate a password for a user, encrypt it, and validate its strength. It then prints the username, generated password, encrypted password, salt used for encryption, and the password strength.

Please note that this code relies on the `Crypt::Eksblowfish::Bcrypt` module for secure password encryption. You may need to install this module using the command `cpan Crypt::Eksblowfish::Bcrypt` before running the code.