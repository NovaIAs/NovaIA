```go
// Package gocomplex provides a comprehensive set of complex number operations.
package gocomplex

import (
	"github.com/gonum/blas"
	"github.com/gonum/blas/blas64"

	"fmt"
	"math"
	"math/cmplx"
)

// Complex represents a complex number.
type Complex struct {
	Real, Imag float64
}

// NewComplex creates a new complex number.
func NewComplex(real, imag float64) Complex {
	return Complex{real, imag}
}

// Add performs addition of two complex numbers.
func Add(c, a, b Complex) {
	c.Real = a.Real + b.Real
	c.Imag = a.Imag + b.Imag
}

// Sub performs subtraction of two complex numbers.
func Sub(c, a, b Complex) {
	c.Real = a.Real - b.Real
	c.Imag = a.Imag - b.Imag
}

// Mul performs multiplication of two complex numbers.
func Mul(c, a, b Complex) {
	c.Real = a.Real*b.Real - a.Imag*b.Imag
	c.Imag = a.Real*b.Imag + a.Imag*b.Real
}

// Div performs division of two complex numbers.
func Div(c, a, b Complex) {
	denom := b.Real*b.Real + b.Imag*b.Imag
	c.Real = (a.Real*b.Real + a.Imag*b.Imag) / denom
	c.Imag = (a.Imag*b.Real - a.Real*b.Imag) / denom
}

// Conj returns the conjugate of a complex number.
func Conj(c Complex) Complex {
	return Complex{c.Real, -c.Imag}
}

// Abs returns the absolute value of a complex number.
func Abs(c Complex) float64 {
	return math.Sqrt(c.Real*c.Real + c.Imag*c.Imag)
}

// Arg returns the argument of a complex number.
func Arg(c Complex) float64 {
	return math.Atan2(c.Imag, c.Real)
}

// Pow returns the power of a complex number.
func Pow(c, a Complex, n int) Complex {
	var result Complex
	result.Real = math.Pow(a.Real, float64(n))
	result.Imag = math.Pow(a.Imag, float64(n))
	return result
}

// Exp returns the exponential of a complex number.
func Exp(c Complex) Complex {
	return Complex{math.Exp(c.Real) * math.Cos(c.Imag), math.Exp(c.Real) * math.Sin(c.Imag)}
}

// Log returns the natural logarithm of a complex number.
func Log(c Complex) Complex {
	return Complex{math.Log(Abs(c)), Arg(c)}
}

// Sin returns the sine of a complex number.
func Sin(c Complex) Complex {
	return Complex{math.Sin(c.Real) * math.Cosh(c.Imag), math.Cos(c.Real) * math.Sinh(c.Imag)}
}

// Cos returns the cosine of a complex number.
func Cos(c Complex) Complex {
	return Complex{math.Cos(c.Real) * math.Cosh(c.Imag), -math.Sin(c.Real) * math.Sinh(c.Imag)}
}

// Tan returns the tangent of a complex number.
func Tan(c Complex) Complex {
	return Div(Sin(c), Cos(c))
}

// Sqrt returns the square root of a complex number.
func Sqrt(c Complex) Complex {
	return Pow(c, Complex{0.5, 0}, 1)
}

// Cbrt returns the cube root of a complex number.
func Cbrt(c Complex) Complex {
	return Pow(c, Complex{1.0 / 3.0, 0}, 1)
}

// Asm creates a new complex number from an assembly value.
func Asm(f float64) Complex {
	return Complex{math.Real(f), math.Imag(f)}
}

// Cmplx creates a new complex number from a Go complex value.
func Cmplx(r, i float64) Complex {
	return Complex{r, i}
}

// String returns a string representation of a complex number.
func (c Complex) String() string {
	return fmt.Sprintf("(%g+%gi)", c.Real, c.Imag)
}

// MarshalJSON implements the MarshalJSON method of the json.Marshaler interface.
func (c Complex) MarshalJSON() ([]byte, error) {
	return []byte(fmt.Sprintf(`"%g+%gi"`, c.Real, c.Imag)), nil
}

// UnmarshalJSON implements the UnmarshalJSON method of the json.Unmarshaler interface.
func (c *Complex) UnmarshalJSON(b []byte) error {
	_, err := fmt.Sscanf(string(b), `"%g+%gi"`, &c.Real, &c.Imag)
	return err
}

// Blas returns a BLAS matrix representation of a complex number.
func (c Complex) Blas() blas.General {
	return blas64.General{
		Rows:   1,
		Cols:   2,
		Stride: 1,
		Data:   []float64{c.Real, c.Imag},
	}
}

// Dot performs the dot product of two complex vectors.
func Dot(x, y []Complex) Complex {
	var c Complex
	for i, xv := range x {
		c.Real += xv.Real * y[i].Real
		c.Imag += xv.Imag * y[i].Imag
	}
	return c
}

// Cross performs the cross product of two complex vectors.
func Cross(x, y []Complex) Complex {
	return Complex{x[1].Real*y[2].Real - x[1].Imag*y[2].Imag - x[2].Real*y[1].Real + x[2].Imag*y[1].Imag,
		x[2].Real*y[0].Real - x[2].Imag*y[0].Imag - x[0].Real*y[2].Real + x[0].Imag*y[2].Imag,
		x[0].Real*y[1].Real - x[0].Imag*y[1].Imag - x[1].Real*y[0].Real + x[1].Imag*y[0].Imag}
}

// Norm returns the norm of a complex vector.
func Norm(x []Complex) float64 {
	var sum float64
	for _, xv := range x {
		sum += xv.Real*xv.Real + xv.Imag*xv.Imag
	}
	return math.Sqrt(sum)
}

// Conj returns the conjugate of a complex matrix.
func Conj(m [][]Complex) [][]Complex {
	for i, row := range m {
		for j, c := range row {
			m[i][j] = Conj(c)
		}
	}
	return m
}

// Transpose returns the transpose of a complex matrix.
func Transpose(m [][]Complex) [][]Complex {
	n := len(m)
	if n == 0 {
		return nil
	}
	p := len(m[0])
	t := make([][]Complex, p)
	for i := range t {
		t[i] = make([]Complex, n)
	}
	for i, row := range m {
		for j, c := range row {
			t[j][i] = c
		}
	}
	return t
}

// Mul performs multiplication of two complex matrices.
func Mul(x, y [][]Complex) [][]Complex {
	m := len(x)
	n := len(x[0])
	p := len(y[0])
	z := make([][]Complex, m)
	for i := range z {
		z[i] = make([]Complex, p)
	}
	var s Complex
	for i := 0; i < m; i++ {
		for j := 0; j < p; j++ {
			for k := 0; k < n; k++ {
				Mul(&s, x[i][k], y[k][j])
				Add(&z[i][j], z[i][j], s)
			}
		}
	}
	return z
}

// Inv returns the inverse of a complex matrix.
func Inv(a [][]Complex) ([][]Complex, error) {
	if len(a) == 0 || len(a[0]) == 0 {
		return nil, fmt.Errorf("matrix must have at least one row and column")
	}
	m := len(a)
	n := len(a[0])
	if m != n {
		return nil, fmt.Errorf("matrix must be square")
	}
	aug := make([][]Complex, m)
	for i := range aug {
		aug[i] = make([]Complex, 2*n)
		copy(aug[i], a[i])
		for j := n; j < 2*n; j++ {
			if i == j-n {
				aug[i][j] = Complex{1, 0}
			} else {
				aug[i][j] = Complex{0, 0}
			}
		}
	}
	for i := 0; i < m; i++ {
		pivot := i
		for j := i + 1; j < m; j++ {
			if Abs(aug[j][i]) > Abs(aug[pivot][i]) {
				pivot = j
			}
		}
		if i != pivot {
			aug[i], aug[pivot] = aug[pivot], aug[i]
		}
		for j := i + 1; j < m; j++ {
			m := Div(aug[j][i], aug[i][i])
			for k := i; k < 2*n; k++ {
				aug[j][k] = Sub(aug[j][k], Mul(m, aug[i][k]))
			}
		}
	}
	for i := m - 1; i >= 0; i-- {
		for j := 0; j < i; j++ {
			m := Div(aug[j][i], aug[i][i])
			for k := i; k < 2*n; k++ {
				aug[j][k] = Sub(aug[j][k], Mul(m, aug[i][k]))
			}
		}
	}
	inv := make([][]Complex, m)
	for i := range inv {
		inv[i] = make([]Complex, n)
		copy(inv[i], aug[i][n:])
	}
	return inv, nil
```