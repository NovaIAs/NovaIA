```php
<?php

// Define a function to generate a random string of a given length
function generateRandomString($length) {
    $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    $randomString = '';
    for ($i = 0; $i < $length; $i++) {
        $randomString .= $characters[rand(0, strlen($characters) - 1)];
    }
    return $randomString;
}

// Define a function to encrypt a string using the AES-256 algorithm
function encryptAES256($string, $key) {
    $iv = openssl_random_pseudo_bytes(16);
    $encryptedString = openssl_encrypt($string, 'AES-256-CBC', $key, OPENSSL_RAW_DATA, $iv);
    return base64_encode($iv . $encryptedString);
}

// Define a function to decrypt a string using the AES-256 algorithm
function decryptAES256($string, $key) {
    $iv = substr(base64_decode($string), 0, 16);
    $encryptedString = substr(base64_decode($string), 16);
    $decryptedString = openssl_decrypt($encryptedString, 'AES-256-CBC', $key, OPENSSL_RAW_DATA, $iv);
    return $decryptedString;
}

// Generate a random encryption key
$encryptionKey = generateRandomString(32);

// Encrypt a message using the AES-256 algorithm
$encryptedMessage = encryptAES256('This is a secret message', $encryptionKey);

// Decrypt the message using the AES-256 algorithm
$decryptedMessage = decryptAES256($encryptedMessage, $encryptionKey);

// Print the encrypted and decrypted messages
echo "Encrypted Message: " . $encryptedMessage . "\n";
echo "Decrypted Message: " . $decryptedMessage . "\n";

```

This code generates a random encryption key, encrypts a message using the AES-256 algorithm, and then decrypts the message using the same key. The code also prints the encrypted and decrypted messages.

The code uses the following functions:

* `generateRandomString()`: This function generates a random string of a given length.
* `encryptAES256()`: This function encrypts a string using the AES-256 algorithm.
* `decryptAES256()`: This function decrypts a string using the AES-256 algorithm.

The code first calls the `generateRandomString()` function to generate a random encryption key. Then, it calls the `encryptAES256()` function to encrypt a message using the encryption key. Next, it calls the `decryptAES256()` function to decrypt the message using the same encryption key. Finally, the code prints the encrypted and decrypted messages.