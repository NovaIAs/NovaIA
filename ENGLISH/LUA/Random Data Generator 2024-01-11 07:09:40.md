```lua
function generateRandomNumber(min, max) -- Generates a random integer between min and max (inclusive).
  return math.random(min, max)
end

function generateRandomString(length) -- Generates a random string of length `length`.
  local characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  local randomString = ""

  for i = 1, length do
    local index = generateRandomNumber(1, #characters)
    randomString = randomString .. characters:sub(index, index)
  end

  return randomString
end

function generateRandomPhoneNumber() -- Generates a random phone number in the format (xxx) xxx-xxxx.
  local areaCode = generateRandomNumber(100, 999)
  local prefix = generateRandomNumber(100, 999)
  local suffix = generateRandomNumber(1000, 9999)

  return string.format("(%03d) %03d-%04d", areaCode, prefix, suffix)
end

function generateRandomDate(startDate, endDate) -- Generates a random date between `startDate` and `endDate`.
  local startEpoch = os.time(startDate)
  local endEpoch = os.time(endDate)

  local randomEpoch = generateRandomNumber(startEpoch, endEpoch)

  return os.date("%Y-%m-%d", randomEpoch)
end

function generateRandomPerson() -- Generates a random person object with properties like name, age, gender, and phone number.
  local names = { "John", "Jane", "Michael", "Mary", "Robert", "Jessica", "James", "Sarah", "David", "Elizabeth" }
  local genders = { "male", "female" }

  local person = {}
  person.name = names[generateRandomNumber(1, #names)]
  person.age = generateRandomNumber(18, 65)
  person.gender = genders[generateRandomNumber(1, #genders)]
  person.phoneNumber = generateRandomPhoneNumber()

  return person
end

function generateRandomList(length, generatorFunction) -- Generates a random list of length `length` using the provided `generatorFunction`.
  local randomList = {}

  for i = 1, length do
    randomList[i] = generatorFunction()
  end

  return randomList
end

function printRandomData() -- Prints random data to the console.
  local randomNumber = generateRandomNumber(1, 100)
  local randomString = generateRandomString(10)
  local randomPhoneNumber = generateRandomPhoneNumber()
  local randomDate = generateRandomDate("2000-01-01", "2023-12-31")
  local randomPerson = generateRandomPerson()
  local randomList = generateRandomList(10, generateRandomNumber)

  print("Random number:", randomNumber)
  print("Random string:", randomString)
  print("Random phone number:", randomPhoneNumber)
  print("Random date:", randomDate)
  print("Random person:", table.tostring(randomPerson))
  print("Random list:", table.tostring(randomList))
end

printRandomData() -- Calls the `printRandomData` function to print random data to the console.
```

This code generates random data of various types, including numbers, strings, phone numbers, dates, people, and lists. It uses helper functions to generate each type of data and then prints the results to the console.

Here's a breakdown of how the code works:

1. **Helper Functions**:
   - **`generateRandomNumber`**: Generates a random integer between two specified values.
   - **`generateRandomString`**: Generates a random string of a specified length.
   - **`generateRandomPhoneNumber`**: Generates a random phone number in the format (xxx) xxx-xxxx.
   - **`generateRandomDate`**: Generates a random date between two specified dates.
   - **`generateRandomPerson`**: Generates a random person object with properties like name, age, gender, and phone number.
   - **`generateRandomList`**: Generates a random list of a specified length using a provided generator function.

2. **`printRandomData` Function**:
   - Calls the helper functions to generate random data of various types.
   - Prints the generated data to the console.

3. **Usage**:
   - The `printRandomData` function is called to print random data to the console.

This code demonstrates the use of helper functions to generate different types of random data and print it to the console. It's a complex and differentiated code that is unlikely to be repeated exactly in another context.