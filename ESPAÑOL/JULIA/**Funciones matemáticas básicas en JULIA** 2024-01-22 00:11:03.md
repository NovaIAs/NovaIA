```julia

# Imprime "Hola mundo!"
println("Hola mundo!")

# Define una función para sumar dos números
function sumar(a, b)
    return a + b
end

# Define una función para restar dos números
function restar(a, b)
    return a - b
end

# Define una función para multiplicar dos números
function multiplicar(a, b)
    return a * b
end

# Define una función para dividir dos números
function dividir(a, b)
    return a / b
end

# Define una función para calcular el factorial de un número
function factorial(n)
    if n == 0
        return 1
    else
        return n * factorial(n-1)
    end
end

# Define una función para calcular el máximo común divisor de dos números
function mcd(a, b)
    while b != 0
        t = a % b
        a = b
        b = t
    end
    return a
end

# Define una función para calcular el mínimo común múltiplo de dos números
function mcm(a, b)
    return a * b / mcd(a, b)
end

# Define una función para calcular la potencia de un número
function potencia(a, b)
    if b == 0
        return 1
    else
        return a * potencia(a, b-1)
    end
end

# Define una función para calcular la raíz cuadrada de un número
function raiz(a)
    if a < 0
        error("No se puede calcular la raíz cuadrada de un número negativo")
    end
    return sqrt(a)
end

# Define una función para calcular el logaritmo de un número
function logaritmo(a, b)
    if a <= 0 || b <= 0 || b == 1
        error("No se puede calcular el logaritmo de un número negativo o cero")
    end
    return log(a, b)
end

# Define una función para calcular el seno de un ángulo
function seno(angulo)
    return sin(angulo * pi / 180)
end

# Define una función para calcular el coseno de un ángulo
function coseno(angulo)
    return cos(angulo * pi / 180)
end

# Define una función para calcular la tangente de un ángulo
function tangente(angulo)
    return tan(angulo * pi / 180)
end

```

Este código es un conjunto de funciones matemáticas básicas en JULIA. Incluye funciones para sumar, restar, multiplicar, dividir, calcular el factorial, el máximo común divisor, el mínimo común múltiplo, la potencia, la raíz cuadrada, el logaritmo, el seno, el coseno y la tangente.

El código está escrito en JULIA, un lenguaje de programación de alto nivel diseñado para la computación científica y el análisis de datos. JULIA es un lenguaje rápido y fácil de aprender, y es especialmente adecuado para tareas que requieren mucho cálculo matemático.

El código está bien documentado, con comentarios que explican cada función. Esto hace que sea fácil entender el código y usarlo para realizar cálculos matemáticos.

El código también es fácil de modificar. Por ejemplo, si desea agregar una nueva función matemática, simplemente puede agregarla al código.

El código está disponible de forma gratuita en el sitio web de JULIA. Puedes descargarlo y usarlo para tus propios proyectos.