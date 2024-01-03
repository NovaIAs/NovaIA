```javascript
// Define a function to generate a random integer between a minimum and maximum value
function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Define a function to generate a random string of a specified length
function generateRandomString(length) {
  var result = '';
  var characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  var charactersLength = characters.length;
  for (var i = 0; i < length; i++) {
    result += characters.charAt(getRandomInt(0, charactersLength - 1));
  }
  return result;
}

// Define a function to generate a unique ID
function generateUniqueId() {
  return generateRandomString(16);
}

// Define a function to create a new object with a unique ID
function createObject() {
  return {
    id: generateUniqueId(),
    name: generateRandomString(10),
    age: getRandomInt(1, 100),
    city: generateRandomString(10)
  };
}

// Define a function to create an array of objects
function createArrayOfObjects(size) {
  var objects = [];
  for (var i = 0; i < size; i++) {
    objects.push(createObject());
  }
  return objects;
}

// Define a function to print an array of objects to the console
function printArrayOfObjects(objects) {
  console.log(JSON.stringify(objects, null, 2));
}

// Generate an array of 10 objects and print it to the console
var objects = createArrayOfObjects(10);
printArrayOfObjects(objects);
```

Explanation:

1. We define a function `getRandomInt` to generate a random integer between a minimum and maximum value. This function is used to generate random values for the object properties.

2. We define a function `generateRandomString` to generate a random string of a specified length. This function is used to generate random values for the object properties.

3. We define a function `generateUniqueId` to generate a unique ID for each object. This ID is used to identify each object in the array.

4. We define a function `createObject` to create a new object with a unique ID, a random name, a random age, and a random city.

5. We define a function `createArrayOfObjects` to create an array of objects. This function takes the size of the array as an argument and uses the `createObject` function to create each object in the array.

6. We define a function `printArrayOfObjects` to print an array of objects to the console. This function uses the JSON.stringify() method to convert the array of objects into a JSON string and then prints it to the console.

7. We generate an array of 10 objects using the `createArrayOfObjects` function and print it to the console using the `printArrayOfObjects` function.