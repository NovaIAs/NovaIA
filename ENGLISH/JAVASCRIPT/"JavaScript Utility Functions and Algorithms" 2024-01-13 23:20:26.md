```javascript
// Define a function to generate a random integer between two values
function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Define a function to shuffle an array
function shuffleArray(array) {
  for (let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
}

// Define a function to generate a random string of a given length
function generateRandomString(length) {
  const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  let result = '';
  for (let i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * characters.length));
  }
  return result;
}

// Define a function to convert a string to an array of characters
function stringToCharArray(string) {
  const array = [];
  for (let i = 0; i < string.length; i++) {
    array.push(string.charAt(i));
  }
  return array;
}

// Define a function to convert an array of characters to a string
function charArrayToString(array) {
  let string = '';
  for (let i = 0; i < array.length; i++) {
    string += array[i];
  }
  return string;
}

// Define a function to encrypt a string using a Caesar cipher with a given key
function caesarEncrypt(string, key) {
  const charArray = stringToCharArray(string);
  for (let i = 0; i < charArray.length; i++) {
    const code = charArray[i].charCodeAt(0);
    if (code >= 65 && code <= 90) { // Uppercase letters
      charArray[i] = String.fromCharCode(((code - 65 + key) % 26) + 65);
    } else if (code >= 97 && code <= 122) { // Lowercase letters
      charArray[i] = String.fromCharCode(((code - 97 + key) % 26) + 97);
    }
  }
  return charArrayToString(charArray);
}

// Define a function to decrypt a string encrypted with a Caesar cipher with a given key
function caesarDecrypt(string, key) {
  const charArray = stringToCharArray(string);
  for (let i = 0; i < charArray.length; i++) {
    const code = charArray[i].charCodeAt(0);
    if (code >= 65 && code <= 90) { // Uppercase letters
      charArray[i] = String.fromCharCode(((code - 65 - key + 26) % 26) + 65);
    } else if (code >= 97 && code <= 122) { // Lowercase letters
      charArray[i] = String.fromCharCode(((code - 97 - key + 26) % 26) + 97);
    }
  }
  return charArrayToString(charArray);
}

// Define a function to generate a random password of a given length
function generatePassword(length) {
  const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()_+';
  let password = '';
  for (let i = 0; i < length; i++) {
    password += characters.charAt(Math.floor(Math.random() * characters.length));
  }
  return password;
}

// Define a function to validate a password based on certain criteria
function validatePassword(password) {
  const criteria = [
    {
      regex: /[a-z]/,
      message: 'Password must contain at least one lowercase letter.'
    },
    {
      regex: /[A-Z]/,
      message: 'Password must contain at least one uppercase letter.'
    },
    {
      regex: /[0-9]/,
      message: 'Password must contain at least one number.'
    },
    {
      regex: /[^a-zA-Z0-9]/,
      message: 'Password must contain at least one special character.'
    },
    {
      regex: /.{8,}/,
      message: 'Password must be at least 8 characters long.'
    }
  ];

  for (const criterion of criteria) {
    if (!criterion.regex.test(password)) {
      return criterion.message;
    }
  }

  return 'Password is valid.';
}

// Define a function to hash a string using the SHA-256 algorithm
function sha256(string) {
  const hash = crypto.createHash('sha256');
  hash.update(string);
  return hash.digest('hex');
}

// Define a function to generate a random number between two values, inclusive
function randomNumber(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Define a function to generate a random array of a given size, filled with random numbers
function randomArray(size) {
  const array = [];
  for (let i = 0; i < size; i++) {
    array.push(randomNumber(1, 100));
  }
  return array;
}

// Define a function to find the maximum value in an array
function maxInArray(array) {
  let max = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > max) {
      max = array[i];
    }
  }
  return max;
}

// Define a function to find the minimum value in an array
function minInArray(array) {
  let min = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < min) {
      min = array[i];
    }
  }
  return min;
}

// Define a function to calculate the average of an array of numbers
function average(array) {
  let sum = 0;
  for (let i = 0; i < array.length; i++) {
    sum += array[i];
  }
  return sum / array.length;
}

// Define a function to find the median of an array of numbers
function median(array) {
  array.sort((a, b) => a - b);
  const middle = Math.floor(array.length / 2);
  if (array.length % 2 === 0) {
    return (array[middle] + array[middle - 1]) / 2;
  } else {
    return array[middle];
  }
}

// Define a function to find the mode of an array of numbers
function mode(array) {
  const counts = {};
  for (let i = 0; i < array.length; i++) {
    if (counts[array[i]] === undefined) {
      counts[array[i]] = 0;
    }
    counts[array[i]]++;
  }
  let maxCount = 0;
  let modeValue = null;
  for (const value in counts) {
    if (counts[value] > maxCount) {
      maxCount = counts[value];
      modeValue = value;
    }
  }
  return modeValue;
}

// Define a function to find the intersection of two arrays
function intersection(array1, array2) {
  const result = [];
  for (let i = 0; i < array1.length; i++) {
    if (array2.includes(array1[i])) {
      result.push(array1[i]);
    }
  }
  return result;
}

// Define a function to find the union of two arrays
function union(array1, array2) {
  const result = [...array1];
  for (let i = 0; i < array2.length; i++) {
    if (!result.includes(array2[i])) {
      result.push(array2[i]);
    }
  }
  return result;
}

// Define a function to find the difference of two arrays
function difference(array1, array2) {
  const result = [];
  for (let i = 0; i < array1.length; i++) {
    if (!array2.includes(array1[i])) {
      result.push(array1[i]);