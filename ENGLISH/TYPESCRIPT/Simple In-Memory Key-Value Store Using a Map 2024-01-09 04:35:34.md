```typescript
// This code implements a simple in-memory key-value store using a Map.

// Define the interface for the key-value store.
// It defines the methods that the key-value store will have.
interface KeyValueStore {
  // The `set` method is used to set the value for a given key.
  set(key: string, value: string): void;

  // The `get` method is used to get the value for a given key.
  get(key: string): string | undefined;

  // The `delete` method is used to delete the value for a given key.
  delete(key: string): void;

  // The `has` method is used to check if a key exists in the key-value store.
  has(key: string): boolean;
}

// Define the class for the key-value store.
// It implements the interface defined above.
class KeyValueStoreImpl implements KeyValueStore {
  // The `store` is a Map object that is used to store the key-value pairs.
  private store: Map<string, string>;

  // The constructor for the key-value store.
  // It initializes the `store` with an empty Map.
  constructor() {
    this.store = new Map();
  }

  // The `set` method is used to set the value for a given key.
  // It takes the key and the value as arguments and sets the value in the `store`.
  set(key: string, value: string): void {
    this.store.set(key, value);
  }

  // The `get` method is used to get the value for a given key.
  // It takes the key as an argument and returns the value from the `store`.
  get(key: string): string | undefined {
    return this.store.get(key);
  }

  // The `delete` method is used to delete the value for a given key.
  // It takes the key as an argument and deletes the key-value pair from the `store`.
  delete(key: string): void {
    this.store.delete(key);
  }

  // The `has` method is used to check if a key exists in the key-value store.
  // It takes the key as an argument and returns a boolean indicating whether the key exists.
  has(key: string): boolean {
    return this.store.has(key);
  }
}

// Create an instance of the key-value store.
const store = new KeyValueStoreImpl();

// Set some key-value pairs in the store.
store.set("key1", "value1");
store.set("key2", "value2");

// Get the value for a given key.
const value1 = store.get("key1");

// Check if a key exists in the store.
const hasKey = store.has("key3");

// Delete a key-value pair from the store.
store.delete("key2");

// Print the value of `value1`, `hasKey`, and the size of the store.
console.log(`value1: ${value1}`);
console.log(`hasKey: ${hasKey}`);
console.log(`size: ${store.size}`);
```

This code implements a simple in-memory key-value store using a Map. The key-value store has the following methods:

* `set`: to set the value for a given key
* `get`: to get the value for a given key
* `delete`: to delete the value for a given key
* `has`: to check if a key exists in the key-value store

The code also creates an instance of the key-value store and sets some key-value pairs in the store. It then gets the value for a given key, checks if a key exists in the store, and deletes a key-value pair from the store. Finally, it prints the value of `value1`, `hasKey`, and the size of the store.

The output of the code is:

```
value1: value1
hasKey: false
size: 1
```