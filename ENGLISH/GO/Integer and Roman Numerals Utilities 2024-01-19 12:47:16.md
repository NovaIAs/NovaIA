```go
package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

const (
	maxInt = int(^uint(0) >> 1)
	minInt = -maxInt - 1
)

func main() {
	// Define a function to calculate the factorial of a non-negative integer.
	factorial := func(n int) int {
		if n < 0 || n > maxInt {
			panic("invalid input: factorial is undefined for negative integers and integers greater than maxInt")
		}
		if n == 0 {
			return 1
		}
		result := 1
		for i := 1; i <= n; i++ {
			result *= i
		}
		return result
	}

	// Define a function to check if a given integer is prime.
	isPrime := func(n int) bool {
		if n <= 1 {
			return false
		}
		if n <= 3 {
			return true
		}
		if n%2 == 0 || n%3 == 0 {
			return false
		}
		for i := 5; i*i <= n; i += 6 {
			if n%i == 0 || n%(i+2) == 0 {
				return false
			}
		}
		return true
	}

	// Define a function to find the greatest common divisor (GCD) of two integers.
	gcd := func(a, b int) int {
		if a == 0 {
			return b
		}
		return gcd(b%a, a)
	}

	// Define a function to find the least common multiple (LCM) of two integers.
	lcm := func(a, b int) int {
		return (a * b) / gcd(a, b)
	}

	// Define a function to convert an integer to a Roman numeral.
	intToRoman := func(n int) string {
		if n < 1 || n > 3999 {
			panic("invalid input: integer must be between 1 and 3999")
		}
		romanNumerals := []struct {
			value  int
			symbol string
		}{
			{1000, "M"},
			{900, "CM"},
			{500, "D"},
			{400, "CD"},
			{100, "C"},
			{90, "XC"},
			{50, "L"},
			{40, "XL"},
			{10, "X"},
			{9, "IX"},
			{5, "V"},
			{4, "IV"},
			{1, "I"},
		}
		var result strings.Builder
		for _, numeral := range romanNumerals {
			for n >= numeral.value {
				result.WriteString(numeral.symbol)
				n -= numeral.value
			}
		}
		return result.String()
	}

	// Define a function to convert a Roman numeral to an integer.
	romanToInt := func(s string) int {
		romanNumerals := map[string]int{
			"M":  1000,
			"CM": 900,
			"D":  500,
			"CD": 400,
			"C":  100,
			"XC": 90,
			"L":  50,
			"XL": 40,
			"X":  10,
			"IX": 9,
			"V":  5,
			"IV": 4,
			"I":  1,
		}
		result := 0
		for i := 0; i < len(s); {
			if i+1 < len(s) && romanNumerals[string(s[i])] < romanNumerals[string(s[i+1])] {
				result += romanNumerals[string(s[i+1])