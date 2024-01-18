```lua
-- This code is a comprehensive utility script that combines multiple functionalities, including date manipulation, string operations, mathematical calculations, and file handling. It is designed to be versatile and useful for a wide range of tasks.

-- Module for date manipulation
local DateUtils = {}

-- Function to get the current date and time
DateUtils.now = function()
    local dt = os.date("*t")
    return string.format("%04d-%02d-%02d %02d:%02d:%02d", dt.year, dt.month, dt.day, dt.hour, dt.min, dt.sec)
end

-- Function to add a specified number of days to a given date
DateUtils.addDays = function(date, days)
    local dt = os.date(date, "%Y-%m-%d")
    dt.day = dt.day + days
    return string.format("%04d-%02d-%02d", dt.year, dt.month, dt.day)
end

-- Function to subtract a specified number of days from a given date
DateUtils.subtractDays = function(date, days)
    return DateUtils.addDays(date, -days)
end

-- Function to format a date to a specific format
DateUtils.formatDate = function(date, format)
    local dt = os.date(date, "%Y-%m-%d")
    return string.format(format, dt.year, dt.month, dt.day)
end

-- Module for string manipulation
local StringUtils = {}

-- Function to split a string into an array of substrings based on a specified delimiter
StringUtils.split = function(str, delimiter)
    local result = {}
    local start = 1
    local end = string.find(str, delimiter, start)
    while end do
        result[#result + 1] = string.sub(str, start, end - 1)
        start = end + 1
        end = string.find(str, delimiter, start)
    end
    result[#result + 1] = string.sub(str, start)
    return result
end

-- Function to join an array of strings into a single string using a specified delimiter
StringUtils.join = function(arr, delimiter)
    local result = ""
    for i = 1, #arr do
        result = result .. arr[i]
        if i < #arr then
            result = result .. delimiter
        end
    end
    return result
end

-- Function to reverse a string
StringUtils.reverse = function(str)
    local result = ""
    for i = #str, 1, -1 do
        result = result .. string.sub(str, i, i)
    end
    return result
end

-- Module for mathematical calculations
local MathUtils = {}

-- Function to calculate the factorial of a non-negative integer
MathUtils.factorial = function(n)
    if n == 0 then
        return 1
    end
    return n * MathUtils.factorial(n - 1)
end

-- Function to calculate the greatest common divisor of two integers
MathUtils.gcd = function(a, b)
    while b ~= 0 do
        local temp = b
        b = a % b
        a = temp
    end
    return a
end

-- Function to calculate the least common multiple of two integers
MathUtils.lcm = function(a, b)
    return (a * b) / MathUtils.gcd(a, b)
end

-- Module for file handling
local FileUtils = {}

-- Function to read the contents of a file into a string
FileUtils.readFile = function(path)
    local file = io.open(path, "r")
    local content = file:read("*a")
    file:close()
    return content
end

-- Function to write a string to a file
FileUtils.writeFile = function(path, content)
    local file = io.open(path, "w")
    file:write(content)
    file:close()
end

-- Function to append a string to a file
FileUtils.appendFile = function(path, content)
    local file = io.open(path, "a")
    file:write(content)
    file:close()
end

-- Function to check if a file exists
FileUtils.fileExists = function(path)
    return io.open(path, "r") ~= nil
end

-- Function to delete a file
FileUtils.deleteFile = function(path)
    if FileUtils.fileExists(path) then
        os.remove(path)
    end
end

```