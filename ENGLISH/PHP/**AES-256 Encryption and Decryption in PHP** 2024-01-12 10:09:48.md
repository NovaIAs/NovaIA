```php
<?php

// Define a function to generate a random string of a given length
function generateRandomString($length) {
    $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    $charactersLength = strlen($characters);
    $randomString = '';
    for ($i = 0; $i < $length; $i++) {
        $randomString .= $characters[rand(0, $charactersLength - 1)];
    }
    return $randomString;
}

// Define a function to encrypt a string using AES-256 encryption
function encryptString($string, $key) {
    $key = hash('sha256', $key);
    $iv = openssl_random_pseudo_bytes(16);
    $encryptedString = openssl_encrypt($string, 'AES-256-CBC', $key, 0, $iv);
    return base64_encode($iv . $encryptedString);
}

// Define a function to decrypt a string using AES-256 encryption
function decryptString($string, $key) {
    $key = hash('sha256', $key);
    $string = base64_decode($string);
    $iv = substr($string, 0, 16);
    $encryptedString = substr($string, 16);
    $decryptedString = openssl_decrypt($encryptedString, 'AES-256-CBC', $key, 0, $iv);
    return $decryptedString;
}

// Generate a random string of 16 characters
$randomString = generateRandomString(16);

// Encrypt the string "Hello, world!" using the random string as the key
$encryptedString = encryptString("Hello, world!", $randomString);

// Decrypt the encrypted string using the random string as the key
$decryptedString = decryptString($encryptedString, $randomString);

// Print the encrypted and decrypted strings
echo "Encrypted String: " . $encryptedString . "\n";
echo "Decrypted String: " . $decryptedString . "\n";

?>
```

Explanation:

1. We define three functions:
   - `generateRandomString($length)`: Generates a random string of a given length.
   - `encryptString($string, $key)`: Encrypts a string using AES-256 encryption.
   - `decryptString($string, $key)`: Decrypts a string using AES-256 encryption.

2. We generate a random string of 16 characters using `generateRandomString(16)`.

3. We encrypt the string "Hello, world!" using the random string as the key with `encryptString("Hello, world!", $randomString)`.

4. We decrypt the encrypted string using the random string as the key with `decryptString($encryptedString, $randomString)`.

5. We print the encrypted and decrypted strings using `echo`.

This code demonstrates how to encrypt and decrypt a string using AES-256 encryption in PHP. It generates a random string, encrypts a message using that string as the key, decrypts the encrypted message using the same key, and prints the encrypted and decrypted strings.