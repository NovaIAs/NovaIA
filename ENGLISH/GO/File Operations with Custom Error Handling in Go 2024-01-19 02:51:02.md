```go
package main

import (
	"fmt"
	"os"
)

// This is a custom error type to handle errors that occur during file operations.
type FileError struct {
	operation string
	path      string
	err       error
}

// This function is used to create a new FileError.
func NewFileError(operation, path string, err error) *FileError {
	return &FileError{operation: operation, path: path, err: err}
}

// This function implements the Error() method for the FileError type.
func (e *FileError) Error() string {
	return fmt.Sprintf("Error %q while %q %q: %v", e.err, e.operation, e.path, e.err)
}

// This function reads the contents of a file and returns the contents as a string.
func readFile(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", NewFileError("reading", path, err)
	}
	return string(data), nil
}

// This function writes the contents of a string to a file.
func writeFile(path, data string) error {
	err := os.WriteFile(path, []byte(data), 0644)
	if err != nil {
		return NewFileError("writing", path, err)
	}
	return nil
}

// This function copies the contents of one file to another.
func copyFile(src, dst string) error {
	data, err := readFile(src)
	if err != nil {
		return err
	}
	err = writeFile(dst, data)
	if err != nil {
		return err
	}
	return nil
}

// This function moves a file from one location to another.
func moveFile(src, dst string) error {
	err := copyFile(src, dst)
	if err != nil {
		return err
	}
	err = os.Remove(src)
	if err != nil {
		return NewFileError("removing", src, err)
	}
	return nil
}

// This function deletes a file.
func deleteFile(path string) error {
	err := os.Remove(path)
	if err != nil {
		return NewFileError("deleting", path, err)
	}
	return nil
}

// This function creates a new directory.
func createDirectory(path string) error {
	err := os.Mkdir(path, 0755)
	if err != nil {
		return NewFileError("creating directory", path, err)
	}
	return nil
}

// This function removes a directory.
func removeDirectory(path string) error {
	err := os.Remove(path)
	if err != nil {
		return NewFileError("removing directory", path, err)
	}
	return nil
}

// This function lists the files in a directory.
func listFiles(path string) ([]string, error) {
	files, err := os.ReadDir(path)
	if err != nil {
		return nil, NewFileError("listing files", path, err)
	}
	var filenames []string
	for _, file := range files {
		filenames = append(filenames, file.Name())
	}
	return filenames, nil
}

// This function returns the size of a file.
func fileSize(path string) (int64, error) {
	fileInfo, err := os.Stat(path)
	if err != nil {
		return 0, NewFileError("getting file size", path, err)
	}
	return fileInfo.Size(), nil
}

// This function checks if a file exists.
func fileExists(path string) bool {
	_, err := os.Stat(path)
	if err != nil {
		return false
	}
	return true
}

// This function is the main function of the program.
func main() {
	// Create a new directory.
	err := createDirectory("test")
	if err != nil {
		fmt.Println(err)
		return
	}

	// Create a new file in the directory.
	err = writeFile("test/test.txt", "Hello, world!")
	if err != nil {
		fmt.Println(err)
		return
	}

	// Read the contents of the file.
	data, err := readFile("test/test.txt")
	if err != nil {
		fmt.Println(err)
		return
	}

	// Print the contents of the file to the console.
	fmt.Println(data)

	// Copy the file to another location.
	err = copyFile("test/test.txt", "test/test2.txt")
	if err != nil {
		fmt.Println(err)
		return
	}

	// Move the file to another location.
	err = moveFile("test/test.txt", "test/test3.txt")
	if err != nil {
		fmt.Println(err)
		return
	}

	// Delete the file.
	err = deleteFile("test/test3.txt")
	if err != nil {
		fmt.Println(err)
		return
	}

	// List the files in the directory.
	files, err := listFiles("test")
	if err != nil {
		fmt.Println(err)
		return
	}

	// Print the list of files to the console.
	for _, file := range files {
		fmt.Println(file)
	}

	// Get the size of a file.
	size, err := fileSize("test/test2.txt")
	if err != nil {
		fmt.Println(err)
		return
	}

	// Print the size of the file to the console.
	fmt.Println(size)

	// Check if a file exists.
	exists := fileExists("test/test2.txt")
	if exists {
		fmt.Println("File exists.")
	} else {
		fmt.Println("File does not exist.")
	}

	// Remove the directory.
	err = removeDirectory("test")
	if err != nil {
		fmt.Println(err)
		return
	}
}
```

This code performs various file operations, including reading, writing, copying, moving, deleting, listing, and checking for existence. It also creates and removes directories. The code uses custom error handling with a `FileError` type to provide more detailed information about file-related errors. The program demonstrates how to interact with files and directories in a structured and organized manner.