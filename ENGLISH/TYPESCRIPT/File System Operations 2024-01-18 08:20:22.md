```typescript
// Define a complex data structure to represent a hierarchical file system
interface FileSystemItem {
  name: string;
  type: "file" | "directory";
  size?: number; // Size of a file in bytes (applicable only for files)
  children?: FileSystemItem[]; // List of child items (applicable only for directories)
}

// Create a recursive function to traverse the file system and calculate the total size of all files
function calculateTotalSize(item: FileSystemItem): number {
  // If the item is a file, return its size
  if (item.type === "file") {
    return item.size || 0;
  }

  // If the item is a directory, recursively calculate the total size of all its children
  else {
    let totalSize = 0;
    for (const child of item.children || []) {
      totalSize += calculateTotalSize(child);
    }
    return totalSize;
  }
}

// Create a function to find the largest file in a file system
function findLargestFile(item: FileSystemItem): FileSystemItem | null {
  // If the item is a file, return it
  if (item.type === "file") {
    return item;
  }

  // If the item is a directory, recursively find the largest file in its children
  else {
    let largestFile: FileSystemItem | null = null;
    for (const child of item.children || []) {
      const largestChildFile = findLargestFile(child);
      if (largestChildFile && (!largestFile || largestChildFile.size > largestFile.size)) {
        largestFile = largestChildFile;
      }
    }
    return largestFile;
  }
}

// Create a function to find all files in a file system that match a given size
function findFilesWithSize(item: FileSystemItem, targetSize: number): FileSystemItem[] {
  // If the item is a file and its size matches the target size, return it
  if (item.type === "file" && item.size === targetSize) {
    return [item];
  }

  // If the item is a directory, recursively find all files in its children that match the target size
  else {
    let matchingFiles: FileSystemItem[] = [];
    for (const child of item.children || []) {
      matchingFiles = matchingFiles.concat(findFilesWithSize(child, targetSize));
    }
    return matchingFiles;
  }
}

// Create a function to print the file system in a tree-like structure
function printFileSystem(item: FileSystemItem, depth: number = 0) {
  // Indent the output based on the depth of the item
  const indentation = " ".repeat(depth * 2);

  // Print the item's name and size (if it's a file)
  console.log(`${indentation}${item.name} (${item.type === "file" ? item.size : "directory"})`);

  // If the item is a directory, recursively print its children
  if (item.type === "directory") {
    for (const child of item.children || []) {
      printFileSystem(child, depth + 1);
    }
  }
}

// Create a sample file system
const fileSystem: FileSystemItem = {
  name: "Root Directory",
  type: "directory",
  children: [
    {
      name: "Subdirectory 1",
      type: "directory",
      children: [
        {
          name: "File 1.txt",
          type: "file",
          size: 1024,
        },
        {
          name: "File 2.txt",
          type: "file",
          size: 2048,
        },
      ],
    },
    {
      name: "Subdirectory 2",
      type: "directory",
      children: [
        {
          name: "File 3.txt",
          type: "file",
          size: 4096,
        },
        {
          name: "Subdirectory 3",
          type: "directory",
          children: [
            {
              name: "File 4.txt",
              type: "file",
              size: 8192,
            },
          ],
        },
      ],
    },
  ],
};

// Calculate the total size of all files in the file system
const totalSize = calculateTotalSize(fileSystem);
console.log(`Total size of all files: ${totalSize} bytes`);

// Find the largest file in the file system
const largestFile = findLargestFile(fileSystem);
console.log(`Largest file: ${largestFile.name} (${largestFile.size} bytes)`);

// Find all files in the file system that match a given size
const matchingFiles = findFilesWithSize(fileSystem, 2048);
console.log(`Files with size 2048 bytes:`);
matchingFiles.forEach((file) => console.log(`- ${file.name}`));

// Print the file system in a tree-like structure
console.log("File System:");
printFileSystem(fileSystem);
```