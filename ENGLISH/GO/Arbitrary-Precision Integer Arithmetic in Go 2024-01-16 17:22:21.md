```go
// Package number provides a library for working with numbers.
package number

import (
	"errors"
	"fmt"
	"math"
	"math/big"
	"strconv"
	"strings"
)

// Int is a type that represents an integer value.
type Int struct {
	value *big.Int
}

// NewInt creates a new Int from an integer value.
func NewInt(value int64) *Int {
	return &Int{value: big.NewInt(value)}
}

// FromString creates a new Int from a string representation of an integer value.
func FromString(s string) (*Int, error) {
	value, ok := big.NewInt(0).SetString(s, 10)
	if !ok {
		return nil, errors.New("invalid integer string")
	}
	return &Int{value: value}, nil
}

// String returns the string representation of an Int.
func (n *Int) String() string {
	return n.value.String()
}

// Add returns the sum of two Ints.
func (n *Int) Add(other *Int) *Int {
	return &Int{value: n.value.Add(n.value, other.value)}
}

// Subtract returns the difference of two Ints.
func (n *Int) Subtract(other *Int) *Int {
	return &Int{value: n.value.Sub(n.value, other.value)}
}

// Multiply returns the product of two Ints.
func (n *Int) Multiply(other *Int) *Int {
	return &Int{value: n.value.Mul(n.value, other.value)}
}

// Divide returns the quotient of two Ints.
func (n *Int) Divide(other *Int) (*Int, error) {
	quotient := new(big.Int)
	remainder := new(big.Int)
	if other.value.Cmp(big.NewInt(0)) == 0 {
		return nil, errors.New("division by zero")
	}
	quotient, remainder = n.value.DivMod(n.value, other.value, remainder)
	if remainder.Cmp(big.NewInt(0)) != 0 {
		return nil, errors.New("division does not result in an integer")
	}
	return &Int{value: quotient}, nil
}

// Mod returns the remainder of the division of two Ints.
func (n *Int) Mod(other *Int) (*Int, error) {
	if other.value.Cmp(big.NewInt(0)) == 0 {
		return nil, errors.New("division by zero")
	}
	remainder := new(big.Int)
	quotient, remainder := n.value.DivMod(n.value, other.value, remainder)
	if quotient.Cmp(big.NewInt(0)) != 0 {
		return nil, errors.New("division does not result in an integer")
	}
	return &Int{value: remainder}, nil
}

// Pow returns the result of raising an Int to a power.
func (n *Int) Pow(exponent int64) *Int {
	return &Int{value: n.value.Exp(n.value, big.NewInt(exponent), nil)}
}

// Sqrt returns the square root of an Int.
func (n *Int) Sqrt() (*Int, error) {
	if n.value.Cmp(big.NewInt(0)) < 0 {
		return nil, errors.New("square root of a negative number")
	}
	sqrt := new(big.Int)
	if !sqrt.SetUint64(math.Sqrt(float64(n.value.Uint64()))) {
		return nil, errors.New("square root is not an integer")
	}
	return &Int{value: sqrt}, nil
}

// Cmp compares two Ints and returns an integer indicating the relationship between them.
// The result is:
//
//   -1 if n < other
//    0 if n == other
//   +1 if n > other
//
func (n *Int) Cmp(other *Int) int {
	return n.value.Cmp(other.value)
}

// IsPrime returns true if an Int is prime, and false otherwise.
func (n *Int) IsPrime() bool {
	return n.value.ProbablyPrime(20)
}

// Factorize factorizes an Int into its prime factors.
func (n *Int) Factorize() map[int64]int64 {
	factors := make(map[int64]int64)
	p := big.NewInt(2)
	for n.value.Cmp(big.NewInt(1)) > 0 {
		if n.value.Mod(p).Cmp(big.NewInt(0)) == 0 {
			factors[p.Int64()]++
			n.value = n.value.Div(n.value, p)
		} else {
			p.Add(p, big.NewInt(1))
		}
	}
	return factors
}

// GCD returns the greatest common divisor of two Ints.
func GCD(a, b *Int) *Int {
	for b.value.Cmp(big.NewInt(0)) != 0 {
		a, b = b, a.Mod(b)
	}
	return a
}

// LCM returns the least common multiple of two Ints.
func LCM(a, b *Int) *Int {
	return a.Multiply(b).Divide(GCD(a, b))
}

// IsPerfectNumber returns true if an Int is a perfect number, and false otherwise.
// A perfect number is a number that is equal to the sum of its proper divisors.
func (n *Int) IsPerfectNumber() bool {
	if n.value.Cmp(big.NewInt(1)) <= 0 {
		return false
	}
	sum := big.NewInt(0)
	for i := big.NewInt(2); i.Cmp(n.value.Div(n.value, big.NewInt(2))) < 0; i.Add(i, big.NewInt(1)) {
		if n.value.Mod(i).Cmp(big.NewInt(0)) == 0 {
			sum.Add(sum, i)
		}
	}
	return sum.Cmp(n.value) == 0
}

// IsAbundantNumber returns true if an Int is an abundant number, and false otherwise.
// An abundant number is a number that is greater than the sum of its proper divisors.
func (n *Int) IsAbundantNumber() bool {
	if n.value.Cmp(big.NewInt(12)) < 0 {
		return false
	}
	sum := big.NewInt(0)
	for i := big.NewInt(2); i.Cmp(n.value.Div(n.value, big.NewInt(2))) < 0; i.Add(i, big.NewInt(1)) {
		if n.value.Mod(i).Cmp(big.NewInt(0)) == 0 {
			sum.Add(sum, i)
		}
	}
	return sum.Cmp(n.value) > 0
}

// IsDeficientNumber returns true if an Int is a deficient number, and false otherwise.
// A deficient number is a number that is less than the sum of its proper divisors.
func (n *Int) IsDeficientNumber() bool {
	if n.value.Cmp(big.NewInt(12)) < 0 {
		return false
	}
	sum := big.NewInt(0)
	for i := big.NewInt(2); i.Cmp(n.value.Div(n.value, big.NewInt(2))) < 0; i.Add(i, big.NewInt(1)) {
		if n.value.Mod(i).Cmp(big.NewInt(0)) == 0 {
			sum.Add(sum, i)
		}
	}
	return sum.Cmp(n.value) < 0
}

// IsWeirdNumber returns true if an Int is a weird number, and false otherwise.
// A weird number is a number that is abundant and deficient.
func (n *Int) IsWeirdNumber() bool {
	return n.IsAbundantNumber() && n.IsDeficientNumber()
}

// IsUglyNumber returns true if an Int is an ugly number, and false otherwise.
// An ugly number is a number that is only divisible by 2, 3, and 5.
func (n *Int) IsUglyNumber() bool {
	if n.value.Cmp(big.New