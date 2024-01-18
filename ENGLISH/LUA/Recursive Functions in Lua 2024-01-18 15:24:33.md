```lua
-- Fibonacci Sequence Generator (using a recursive function)

fib = function(n)
    if n == 0 or n == 1 then
        return n
    else
        return fib(n-1) + fib(n-2)
    end
end

-- Function to find the factorial of a number (using a recursive function)

factorial = function(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n-1)
    end
end

-- Function to check if a number is prime (using a recursive function)

isPrime = function(n, i)
    if i == 1 then
        return true
    elseif n % i == 0 then
        return false
    else
        return isPrime(n, i-1)
    end
end

-- Function to reverse a string (using a recursive function)

reverse = function(s)
    if s == "" then
        return ""
    else
        return reverse(string.sub(s, 2)) .. string.sub(s, 1, 1)
    end
end

-- Function to find the greatest common divisor of two numbers (using a recursive function)

gcd = function(a, b)
    if b == 0 then
        return a
    else
        return gcd(b, a % b)
    end
end

-- Function to find the least common multiple of two numbers (using a recursive function)

lcm = function(a, b)
    return (a * b) / gcd(a, b)
end

-- Function to check if a list is sorted (using a recursive function)

isSorted = function(lst)
    if #lst <= 1 then
        return true
    elseif lst[1] > lst[2] then
        return false
    else
        return isSorted(table.sub(lst, 2))
    end
end

-- Function to find the sum of a list (using a recursive function)

sum = function(lst)
    if #lst == 0 then
        return 0
    else
        return lst[1] + sum(table.sub(lst, 2))
    end
end

-- Function to find the maximum value in a list (using a recursive function)

max = function(lst)
    if #lst == 0 then
        return nil
    elseif #lst == 1 then
        return lst[1]
    else
        return math.max(lst[1], max(table.sub(lst, 2)))
    end
end

-- Function to find the minimum value in a list (using a recursive function)

min = function(lst)
    if #lst == 0 then
        return nil
    elseif #lst == 1 then
        return lst[1]
    else
        return math.min(lst[