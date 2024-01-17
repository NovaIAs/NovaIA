```
class Matrix

  def initialize(array)
    @array = array
  end

  def +(other)
    raise TypeError, 'Cannot add matrices of different dimensions' unless @array.size == other.array.size
    Matrix.new(Array.new(@array.size) { |i| @array[i].zip(other.array[i]).map { |x, y| x + y } })
  end

  def -(other)
    raise TypeError, 'Cannot subtract matrices of different dimensions' unless @array.size == other.array.size
    Matrix.new(Array.new(@array.size) { |i| @array[i].zip(other.array[i]).map { |x, y| x - y } })
  end

  def *(other)
    raise TypeError, 'Cannot multiply matrices of different dimensions' unless @array[0].size == other.array.size
    Matrix.new(Array.new(@array.size) { |i|
      Array.new(other.array[0].size) { |j|
        @array[i].zip(other.array).map { |x, y| x * y }.sum
      }
    })
  end

  def inspect
    @array.map { |row| row.inspect }.join("\n")
  end

end

# Example usage:

m1 = Matrix.new([[1, 2], [3, 4]])
m2 = Matrix.new([[5, 6], [7, 8]])

puts m1 + m2
# => Matrix([[6, 8], [10, 12]])

puts m1 - m2
# => Matrix([[-4, -4], [-4, -4]])

puts m1 * m2
# => Matrix([[19, 22], [43, 50]])
```

Explanation:

* The `Matrix` class represents a mathematical matrix.
* It has an `initialize` method which takes a 2D array as its argument and stores it in the `@array` instance variable.
* It has addition (`+`), subtraction (`-`), and multiplication (`*`) operators defined. These operators take another matrix as their argument and return a new matrix as the result of the operation.
* The `inspect` method is used to generate a string representation of the matrix, which is useful for debugging and printing the matrix to the screen.

Example usage:

* The example usage creates two matrices `m1` and `m2` using the `Matrix.new` constructor.
* It then uses the addition (`+`), subtraction (`-`), and multiplication (`*`) operators to perform various operations on the matrices.
* The results of the operations are printed to the screen using the `puts` statement.