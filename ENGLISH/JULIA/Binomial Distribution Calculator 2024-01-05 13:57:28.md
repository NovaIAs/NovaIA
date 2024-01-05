```julia
# Define a function to calculate the factorial of a non-negative integer
factorial(n) = if n == 0
                 1
             else
                 n * factorial(n-1)
             end

# Define a function to calculate the binomial coefficient of two non-negative integers
binomial(n, k) = factorial(n) / (factorial(k) * factorial(n-k))

# Define a function to calculate the probability of getting exactly k heads when flipping n coins
prob_heads(n, k) = binomial(n, k) * (1/2)^n

# Define a function to calculate the expected number of heads when flipping n coins
expected_heads(n) = sum(i * prob_heads(n, i) for i in 0:n)

# Define a function to calculate the variance of the number of heads when flipping n coins
variance_heads(n) = sum((i - expected_heads(n))^2 * prob_heads(n, i) for i in 0:n)

# Print the probability, expected value, and variance of getting exactly 5 heads when flipping 10 coins
println("Probability of getting exactly 5 heads when flipping 10 coins: ", prob_heads(10, 5))
println("Expected number of heads when flipping 10 coins: ", expected_heads(10))
println("Variance of the number of heads when flipping 10 coins: ", variance_heads(10))
```

This code defines functions to calculate the factorial of a non-negative integer, the binomial coefficient of two non-negative integers, the probability of getting exactly k heads when flipping n coins, the expected number of heads when flipping n coins, and the variance of the number of heads when flipping n coins. Then, it prints the probability, expected value, and variance of getting exactly 5 heads when flipping 10 coins.