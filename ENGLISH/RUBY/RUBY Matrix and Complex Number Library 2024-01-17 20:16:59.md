```ruby
# Define a module to encapsulate the functionality
module ComplexCode

  # Define a class to represent a complex number
  class ComplexNumber
    attr_reader :real, :imaginary

    def initialize(real, imaginary)
      @real = real
      @imaginary = imaginary
    end

    # Add two complex numbers
    def +(other)
      ComplexNumber.new(@real + other.real, @imaginary + other.imaginary)
    end

    # Subtract two complex numbers
    def -(other)
      ComplexNumber.new(@real - other.real, @imaginary - other.imaginary)
    end

    # Multiply two complex numbers
    def *(other)
      real_part = @real * other.real - @imaginary * other.imaginary
      imaginary_part = @real * other.imaginary + @imaginary * other.real
      ComplexNumber.new(real_part, imaginary_part)
    end

    # Divide two complex numbers
    def /(other)
      denominator = other.real**2 + other.imaginary**2
      real_part = (@real * other.real + @imaginary * other.imaginary) / denominator
      imaginary_part = (@imaginary * other.real - @real * other.imaginary) / denominator
      ComplexNumber.new(real_part, imaginary_part)
    end

    # Find the absolute value of a complex number
    def abs
      Math.sqrt(@real**2 + @imaginary**2)
    end

    # Find the argument of a complex number
    def arg
      Math.atan2(@imaginary, @real)
    end

    # Represent the complex number as a string
    def to_s
      "#{@real} + #{@imaginary}i"
    end
  end

  # Define a class to represent a matrix
  class Matrix
    attr_reader :rows, :columns

    def initialize(rows, columns)
      @rows = rows
      @columns = columns
      @data = Array.new(rows) { Array.new(columns, 0) }
    end

    # Set the value of an element in the matrix
    def []=(row, column, value)
      @data[row][column] = value
    end

    # Get the value of an element in the matrix
    def [](row, column)
      @data[row][column]
    end

    # Add two matrices
    def +(other)
      Matrix.new(@rows, @columns).tap do |result|
        for i in 0...@rows
          for j in 0...@columns
            result[i, j] = self[i, j] + other[i, j]
          end
        end
      end
    end

    # Subtract two matrices
    def -(other)
      Matrix.new(@rows, @columns).tap do |result|
        for i in 0...@rows
          for j in 0...@columns
            result[i, j] = self[i, j] - other[i, j]
          end
        end
      end
    end

    # Multiply two matrices
    def *(other)
      Matrix.new(@rows, other.columns).tap do |result|
        for i in 0...@rows
          for j in 0...other.columns
            for k in 0...other.rows
              result[i, j] += self[i, k] * other[k, j]
            end
          end
        end
      end
    end

    # Find the determinant of a matrix
    def det
      if @rows != @columns
        raise "Matrix is not square"
      end

      if @rows == 1
        return @data[0][0]
      end

      sum = 0
      for i in 0...@rows
        cofactor = Matrix.new(@rows - 1, @columns - 1)
        for j in 1...@rows
          for k in 0...@columns
            if k < i
              cofactor[j - 1, k] = self[j, k]
            elsif k > i
              cofactor[j - 1, k - 1] = self[j, k]
            end
          end
        end

        sum += (-1)**i * self[0, i] * cofactor.det
      end

      sum
    end

    # Find the inverse of a matrix
    def inv
      if @rows != @columns
        raise "Matrix is not square"
      end

      det = self.det
      if det == 0
        raise "Matrix is not invertible"
      end

      Matrix.new(@rows, @columns).tap do |result|
        for i in 0...@rows
          for j in 0...@columns
            cofactor = Matrix.new(@rows - 1, @columns - 1)
            for k in 0...@rows
              if k != i