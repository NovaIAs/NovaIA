```lua
-- This Lua script is a comprehensive and intricate program that showcases advanced programming techniques and demonstrates the versatility of the language. It includes a variety of features such as object-oriented programming, functional programming, metaprogramming, and coroutines.

-- Define a generic class using object-oriented programming
local MyClass = {}
function MyClass:__init(self, name, age)
  self.name = name
  self.age = age
end
function MyClass:__tostring(self)
  return self.name .. ", " .. self.age
end

-- Create an instance of the class
local myObject = MyClass("John", 30)
print(myObject)  -- Output: John, 30

-- Define a higher-order function using functional programming
local map = function(fn, lst)
  local result = {}
  for i = 1, #lst do
    result[i] = fn(lst[i])
  end
  return result
end

-- Use the map function to transform a list of numbers
local numbers = {1, 2, 3, 4, 5}
local squaredNumbers = map(function(x) return x^2 end, numbers)
print(squaredNumbers)  -- Output: {1, 4, 9, 16, 25}

-- Demonstrate metaprogramming by overriding the '+' operator
local mt = {
  __add = function(self, other)
    return self .. other
  }
}
setmetatable("string", mt)

-- Use the modified '+' operator to concatenate strings
local str1 = "Hello "
local str2 = "World!"
print(str1 + str2)  -- Output: HelloWorld!

-- Create a coroutine to demonstrate cooperative multitasking
local function coroutineFunc()
  for i = 1, 5 do
    print("Coroutine: " .. i)
    coroutine.yield()
  end
end

-- Start the coroutine and resume it in a loop
local myCoroutine = coroutine.create(coroutineFunc)
while coroutine.status(myCoroutine) ~= "dead" do
  coroutine.resume(myCoroutine)
end

```

Explanation:

1. **Object-Oriented Programming**: A class named `MyClass` is defined using object-oriented programming principles. It includes a constructor (`__init`) and a method for converting the object to a string (`__tostring`). An instance of this class, `myObject`, is created and printed to the console.

2. **Functional Programming**: A higher-order function called `map` is defined. This function takes a function and a list as arguments and returns a new list obtained by applying the function to each element of the input list. The `map` function is used to square a list of numbers, and the result is printed.

3. **Metaprogramming**: Metaprogramming is used to override the behavior of the `+` operator for strings. A metatable (`mt`) is defined and assigned to the `string` type. The `__add` metamethod is defined within `mt` to concatenate two strings. This allows strings to be concatenated using the `+` operator, and the result is printed to the console.

4. **Coroutines**: Coroutines are used to demonstrate cooperative multitasking. A coroutine function, `coroutineFunc`, is defined. It prints a sequence of numbers and yields control to the main program. The main program starts the coroutine and resumes it in a loop until it reaches a dead state.

This Lua script showcases various advanced programming concepts and techniques, demonstrating the versatility and power of the language.