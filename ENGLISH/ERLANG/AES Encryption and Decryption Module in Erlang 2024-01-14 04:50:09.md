```erlang
-module(crypto_module).
-export([encrypt/2, decrypt/2]).

-define(KEY, "MySecretPassword").

encrypt(Plaintext, Key) ->
    Ciphertext = crypto:block_encrypt(aes_cbc, Key, Plaintext),
    base64:encode(Ciphertext).

decrypt(Ciphertext, Key) ->
    Plaintext = crypto:block_decrypt(aes_cbc, Key, base64:decode(Ciphertext)),
    Plaintext.
```

This Erlang code defines a module called `crypto_module` that provides two functions: `encrypt/2` and `decrypt/2`, which perform encryption and decryption operations, respectively. The code uses the Advanced Encryption Standard (AES) in Cipher Block Chaining (CBC) mode for encryption and decryption.

**Function encrypt/2:**

* **Input:**
    * `Plaintext`: The plaintext message to be encrypted.
    * `Key`: The encryption key.

* **Output:**
    * Encrypted ciphertext in base64-encoded format.

* **Implementation:**
    1. The function uses the `crypto:block_encrypt/3` function to encrypt the plaintext using the AES-CBC algorithm with the specified key.
    2. The resulting ciphertext is then encoded using the `base64:encode/1` function, which converts the binary ciphertext into a human-readable string representation.

**Function decrypt/2:**

* **Input:**
    * `Ciphertext`: The ciphertext to be decrypted.
    * `Key`: The decryption key.

* **Output:**
    * Decrypted plaintext message.

* **Implementation:**
    1. The function uses the `base64:decode/1` function to decode the base64-encoded ciphertext back into its binary representation.
    2. It then uses the `crypto:block_decrypt/3` function to decrypt the ciphertext using the AES-CBC algorithm with the specified key.
    3. The resulting plaintext is returned as the output.

**Usage:**

To use this module, you can import it into your Erlang project and call the `encrypt/2` and `decrypt/2` functions to perform encryption and decryption operations. Make sure to replace the `Key` constant with your own secret password or encryption key.

This code provides a secure way to encrypt and decrypt data using AES-CBC encryption, which is commonly used for encrypting sensitive information in various applications.