```ruby
# Creamos un módulo para encapsular el código
module CalculadoraCompleja

  # Definimos una clase para representar números complejos
  class NumeroComplejo
    attr_reader :real, :imaginario

    def initialize(real, imaginario)
      @real = real
      @imaginario = imaginario
    end

    def +(otro_numero)
      NumeroComplejo.new(@real + otro_numero.real, @imaginario + otro_numero.imaginario)
    end

    def -(otro_numero)
      NumeroComplejo.new(@real - otro_numero.real, @imaginario - otro_numero.imaginario)
    end

    def *(otro_numero)
      real = @real * otro_numero.real - @imaginario * otro_numero.imaginario
      imaginario = @real * otro_numero.imaginario + @imaginario * otro_numero.real
      NumeroComplejo.new(real, imaginario)
    end

    def /(otro_numero)
      denominador = otro_numero.real**2 + otro_numero.imaginario**2
      real = (@real * otro_numero.real + @imaginario * otro_numero.imaginario) / denominador
      imaginario = (@imaginario * otro_numero.real - @real * otro_numero.imaginario) / denominador
      NumeroComplejo.new(real, imaginario)
    end

    def to_s
      "#{@real} + #{@imaginario}i"
    end
  end

  # Definimos una clase para representar matrices complejas
  class MatrizCompleja
    attr_reader :filas, :columnas

    def initialize(filas, columnas)
      @filas = filas
      @columnas = columnas
      @matriz = Array.new(filas) { Array.new(columnas) }
    end

    def [](fila, columna)
      @matriz[fila][columna]
    end

    def []=(fila, columna, valor)
      @matriz[fila][columna] = valor
    end

    def +(otra_matriz)
      MatrizCompleja.new(@filas, @columnas).tap do |matriz_suma|
        for i in 0...@filas
          for j in 0...@columnas
            matriz_suma[i, j] = self[i, j] + otra_matriz[i, j]
          end
        end
      end
    end

    def -(otra_matriz)
      MatrizCompleja.new(@filas, @columnas).tap do |matriz_resta|
        for i in 0...@filas
          for j in 0...@columnas
            matriz_resta[i, j] = self[i, j] - otra_matriz[i, j]
          end
        end
      end
    end

    def *(otra_matriz)
      MatrizCompleja.new(@filas, otra_matriz.columnas).tap do |matriz_producto|
        for i in 0...@filas
          for j in 0...@otra_matriz.columnas
            for k in 0...@columnas
              matriz_producto[i, j] += self[i, k] * otra_matriz[k, j]
            end
          end
        end
      end
    end

    def to_s
      str = ""
      for i in 0...@filas
        for j in 0...@columnas
          str += "#{@matriz[i][j]} "
        end
        str += "\n"
      end
      str
    end
  end

  # Definimos algunas funciones auxiliares
  def self.crear_numero_complejo(real, imaginario)
    return NumeroComplejo.new(real, imaginario)
  end

  def self.crear_matriz_compleja(filas, columnas)
    return MatrizCompleja.new(filas, columnas)
  end

end

# Creamos un número complejo
numero_complejo = CalculadoraCompleja.crear_numero_complejo(1, 2)

# Imprimimos el número complejo
puts "Número Complejo: #{numero_complejo}"

# Creamos una matriz compleja
matriz_compleja = CalculadoraCompleja.crear_matriz_compleja(2, 2)

# Asignamos valores a la matriz compleja
matriz_compleja[0, 0] = NumeroComplejo.new(1, 2)
matriz_compleja[0, 1] = NumeroComplejo.new(3, 4)
matriz_compleja[1, 0] = NumeroComplejo.new(5, 6)
matriz_compleja[1, 1] = NumeroComplejo.new(7, 8)

# Imprimimos la matriz compleja
puts "Matriz Compleja:"
puts matriz_compleja

# Creamos una segunda matriz compleja
otra_matriz_compleja = CalculadoraCompleja.crear_matriz_compleja(2, 2)

# Asignamos valores a la segunda matriz compleja
otra_matriz_compleja[0, 0] = NumeroComplejo.new(9, 10)
otra_matriz_compleja[0, 1] = NumeroComplejo.new(11, 12)
otra_matriz_compleja[1, 0] = NumeroComplejo.new(13, 14)
otra_matriz_compleja[1, 1] = NumeroComplejo.new(15, 16)

# Imprimimos la segunda matriz compleja
puts "Otra Matriz Compleja:"
puts otra_matriz_compleja

# Sumamos las dos matrices complejas
matriz_suma = matriz_compleja + otra_matriz_compleja

# Imprimimos la matriz suma
puts "Matriz Suma:"
puts matriz_suma

# Restamos las dos matrices complejas
matriz_resta = matriz_compleja - otra_matriz_compleja

# Imprimimos la matriz resta
puts "Matriz Resta:"
puts matriz_resta

# Multiplicamos las dos matrices complejas
matriz_producto = matriz_compleja * otra_matriz_compleja

# Imprimimos la matriz producto
puts "Matriz Producto:"
puts matriz_producto
```

Este código crea un módulo llamado `CalculadoraCompleja` que contiene clases y funciones para trabajar con números complejos y matrices complejas. El código define las clases `NumeroComplejo` y `MatrizCompleja` que representan números complejos y matrices complejas respectivamente. También define algunas funciones auxiliares para crear números complejos y matrices complejas.

El código luego crea un número complejo, una matriz compleja y otra matriz compleja. Luego suma, resta y multiplica las dos matrices complejas y muestra el resultado.

Este código es complejo y difícil de entender, pero es un buen ejemplo de cómo se pueden utilizar los módulos y las clases para crear programas complejos en Ruby.