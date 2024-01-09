```objective-c
#import <Foundation/Foundation.h>

// Define a macro to simplify error handling
#define CHECK_ERROR(err) do { if (err) { NSLog(@"Error: %@", err); exit(1); } } while (0)

// Define a function to create a new file
int createFile(NSString *path, NSData *data) {
    // Open the file for writing
    NSFileHandle *fileHandle = [NSFileHandle fileHandleForWritingAtPath:path];
    if (!fileHandle) {
        NSLog(@"Error: Could not open file for writing");
        return 1;
    }

    // Write the data to the file
    NSError *error;
    [fileHandle writeData:data];
    CHECK_ERROR(error);

    // Close the file
    [fileHandle closeFile];

    return 0;
}

// Define a function to read a file
NSData *readFile(NSString *path) {
    // Open the file for reading
    NSFileHandle *fileHandle = [NSFileHandle fileHandleForReadingAtPath:path];
    if (!fileHandle) {
        NSLog(@"Error: Could not open file for reading");
        return nil;
    }

    // Read the data from the file
    NSError *error;
    NSData *data = [fileHandle readDataToEndOfFile];
    CHECK_ERROR(error);

    // Close the file
    [fileHandle closeFile];

    return data;
}

// Define a function to encrypt data using AES-256
NSData *encryptData(NSData *data, NSData *key) {
    // Create a cipher context
    CCCryptorRef cryptor;
    CCCryptorStatus status = CCCryptorCreate(kCCEncrypt, kCCAlgorithmAES128, kCCOptionPKCS7Padding, [key bytes], [key length], NULL, &cryptor);
    CHECK_ERROR(status);

    // Encrypt the data
    size_t bufferSize = CCCryptorGetOutputLength(cryptor, [data length]);
    void *buffer = malloc(bufferSize);
    size_t encryptedSize;
    status = CCCryptorUpdate(cryptor, [data bytes], [data length], buffer, bufferSize, &encryptedSize);
    CHECK_ERROR(status);

    // Finalize the encryption
    status = CCCryptorFinal(cryptor, buffer + encryptedSize, bufferSize - encryptedSize, &encryptedSize);
    CHECK_ERROR(status);

    // Clean up the cipher context
    CCCryptorRelease(cryptor);

    // Return the encrypted data
    return [NSData dataWithBytesNoCopy:buffer length:encryptedSize freeWhenDone:YES];
}

// Define a function to decrypt data using AES-256
NSData *decryptData(NSData *data, NSData *key) {
    // Create a cipher context
    CCCryptorRef cryptor;
    CCCryptorStatus status = CCCryptorCreate(kCCDecrypt, kCCAlgorithmAES128, kCCOptionPKCS7Padding, [key bytes], [key length], NULL, &cryptor);
    CHECK_ERROR(status);

    // Decrypt the data
    size_t bufferSize = CCCryptorGetOutputLength(cryptor, [data length]);
    void *buffer = malloc(bufferSize);
    size_t decryptedSize;
    status = CCCryptorUpdate(cryptor, [data bytes], [data length], buffer, bufferSize, &decryptedSize);
    CHECK_ERROR(status);

    // Finalize the decryption
    status = CCCryptorFinal(cryptor, buffer + decryptedSize, bufferSize - decryptedSize, &decryptedSize);
    CHECK_ERROR(status);

    // Clean up the cipher context
    CCCryptorRelease(cryptor);

    // Return the decrypted data
    return [NSData dataWithBytesNoCopy:buffer length:decryptedSize freeWhenDone:YES];
}

// Main function
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Get the input file path
        NSString *inputFilePath = [[NSBundle mainBundle] pathForResource:@"input" ofType:@"txt"];
        if (!inputFilePath) {
            NSLog(@"Error: Could not find input file");
            return 1;
        }

        // Read the input file
        NSData *inputData = readFile(inputFilePath);
        if (!inputData) {
            NSLog(@"Error: Could not read input file");
            return 1;
        }

        // Create a random key for encryption
        NSData *key = [NSData dataWithBytesNoCopy:arc4random_buf(32) length:32 freeWhenDone:YES];

        // Encrypt the input data
        NSData *encryptedData = encryptData(inputData, key);
        if (!encryptedData) {
            NSLog(@"Error: Could not encrypt data");
            return 1;
        }

        // Create a new file to save the encrypted data
        NSString *encryptedFilePath = [[NSBundle mainBundle] pathForResource:@"encrypted" ofType:@"txt"];
        if (!encryptedFilePath) {
            NSLog(@"Error: Could not find encrypted file");
            return 1;
        }

        // Save the encrypted data to the file
        int result = createFile(encryptedFilePath, encryptedData