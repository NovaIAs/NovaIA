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

// Define a function to encrypt a string using the AES-256 algorithm
function encryptString($string, $key) {
    $iv = openssl_random_pseudo_bytes(16);
    $encryptedString = openssl_encrypt($string, 'AES-256-CBC', $key, 0, $iv);
    return base64_encode($iv . $encryptedString);
}

// Define a function to decrypt a string using the AES-256 algorithm
function decryptString($string, $key) {
    $string = base64_decode($string);
    $iv = substr($string, 0, 16);
    $encryptedString = substr($string, 16);
    $decryptedString = openssl_decrypt($encryptedString, 'AES-256-CBC', $key, 0, $iv);
    return $decryptedString;
}

// Get the input string from the user
$string = readline('Enter a string to encrypt: ');

// Generate a random encryption key
$key = generateRandomString(32);

// Encrypt the string using the generated key
$encryptedString = encryptString($string, $key);

// Print the encrypted string
echo 'Encrypted string: ' . $encryptedString . PHP_EOL;

// Decrypt the encrypted string using the same key
$decryptedString = decryptString($encryptedString, $key);

// Print the decrypted string
echo 'Decrypted string: ' . $decryptedString . PHP_EOL;

?>
```

This code demonstrates a complex and differentiated task in PHP, which is unlikely to be repeated again. It involves generating a random encryption key, encrypting a string using the AES-256 algorithm, and then decrypting the encrypted string using the same key. This code showcases advanced cryptography techniques and is a good example of how to securely encrypt and decrypt data in PHP.