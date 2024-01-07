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

// Define a function to encrypt a string using AES-256 encryption
function encryptString($string, $key) {
    $key = hash('sha256', $key);
    $iv = openssl_random_pseudo_bytes(16);
    $encryptedString = openssl_encrypt($string, 'AES-256-CBC', $key, OPENSSL_RAW_DATA, $iv);
    return base64_encode($iv . $encryptedString);
}

// Define a function to decrypt a string using AES-256 encryption
function decryptString($string, $key) {
    $key = hash('sha256', $key);
    $string = base64_decode($string);
    $iv = substr($string, 0, 16);
    $encryptedString = substr($string, 16);
    $decryptedString = openssl_decrypt($encryptedString, 'AES-256-CBC', $key, OPENSSL_RAW_DATA, $iv);
    return $decryptedString;
}

// Generate a random string of length 32
$randomString = generateRandomString(32);

// Encrypt the random string using a secret key
$encryptedString = encryptString($randomString, 'MySecretKey');

// Decrypt the encrypted string using the same secret key
$decryptedString = decryptString($encryptedString, 'MySecretKey');

// Check if the decrypted string matches the original random string
if ($decryptedString === $randomString) {
    echo 'Encryption and decryption successful!';
} else {
    echo 'Encryption or decryption failed!';
}

```

Explanation:

1. We define three functions:

    * `generateRandomString()` generates a random string of a given length.
    * `encryptString()` encrypts a string using AES-256 encryption with a given key.
    * `decryptString()` decrypts an encrypted string using AES-256 encryption with the same key.

2. We generate a random string of length 32 using the `generateRandomString()` function.

3. We encrypt the random string using the `encryptString()` function with a secret key.

4. We decrypt the encrypted string using the `decryptString()` function with the same secret key.

5. Finally, we check if the decrypted string matches the original random string. If they match, it means that the encryption and decryption were successful.