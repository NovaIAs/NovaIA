def getLargestPalindromeProductOf3DigitNumbers() {
    def largestPalindromeProduct = 0
    for (i in 100..999) {
        for (j in 100..999) {
            def product = i * j
            def reversedProduct = 0
            def originalProduct = product
            while (product) {
                reversedProduct *= 10
                reversedProduct += product % 10
                product /= 10
            }
            if (reversedProduct == originalProduct) {
                largestPalindromeProduct = Math.max(largestPalindromeProduct, originalProduct)
            }
        }
    }
    largestPalindromeProduct
}

def number = getLargestPalindromeProductOf3DigitNumbers()
println number // Output: 906609


**Explanation:**

* The `getLargestPalindromeProductOf3DigitNumbers()` method is a custom Groovy method that calculates the largest palindrome product of 3-digit numbers.
* It uses nested `for` loops to iterate through all pairs of 3-digit numbers.
* Inside the loops, it calculates the product of the two numbers and checks if it's a palindrome by comparing it with its reversed form.
* If the product is a palindrome, it updates the `largestPalindromeProduct` variable with the maximum value.
* After iterating through all pairs of numbers, the method returns the largest palindrome product.
* The `number` variable is assigned the result of calling the `getLargestPalindromeProductOf3DigitNumbers()` method, and then it's printed to the console.