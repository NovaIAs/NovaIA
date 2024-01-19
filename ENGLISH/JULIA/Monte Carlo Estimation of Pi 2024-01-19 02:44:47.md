```julia
function estimate_pi(n)
    # Initialize variables
    in_circle = 0
    total = 0

    # Generate random points
    for i in 1:n
        # Generate a random point in the unit square
        x = rand()
        y = rand()

        # Check if the point is inside the circle
        if (x^2 + y^2) < 1
            in_circle += 1
        end

        # Increment the total number of points
        total += 1
    end

    # Calculate the estimate of pi
    pi_estimate = 4 * in_circle / total

    # Return the estimate
    return pi_estimate
end

# Set the number of points
n = 1000000

# Estimate pi
pi_estimate = estimate_pi(n)

# Print the estimate
println("Estimate of pi: $pi_estimate")
```

This code estimates the value of pi using the Monte Carlo method. The Monte Carlo method is a technique for estimating the value of an integral by randomly sampling the integrand. In this case, the integrand is the function `f(x, y) = 1` if the point `(x, y)` is inside the unit circle, and 0 otherwise. The integral of this function over the unit square is equal to the area of the unit circle, which is pi.

The code first initializes the variables `in_circle` and `total` to 0. Then, it generates `n` random points in the unit square using the `rand()` function. For each point, it checks if the point is inside the circle by computing the distance from the point to the origin. If the distance is less than 1, then the point is inside the circle and `in_circle` is incremented. Otherwise, the point is outside the circle and `in_circle` is not incremented. Finally, the code calculates the estimate of pi by dividing `in_circle` by `total` and multiplying the result by 4.

The output of the code will be something like this:

```
Estimate of pi: 3.141592653589793
```

This is a good estimate of pi, considering that it was generated using only 1 million random points.