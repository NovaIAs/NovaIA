```julia

# Definición de una Función Compleja con Variables Múltiples

function ecuacion_compleja(x, y, z)
    # Operación de Multiplicación Compleja
    producto = x .* y .+ z .^ 2

    # Definición de una Variable Compleja
    resultado = producto + im * (x .^ 2)

    # Retorno de la Variable Compleja
    return resultado
end

# Parámetros de Entrada
x = 2.0 + 3.0im
y = 4.0 - 2.0im
z = 1.0 + 5.0im

# Llamada a la Función Compleja
ecuacion_compleja(x, y, z)

# Definición de una Función Anónima Compleja

ecuacion_anonima = (x, y, z) -> x .* y .+ z .^ 2 + im * (x .^ 2)

# Parámetros de Entrada
x = 2.0 + 3.0im
y = 4.0 - 2.0im
z = 1.0 + 5.0im

# Llamada a la Función Anónima Compleja
ecuacion_anonima(x, y, z)

# Definición de una Función Recursiva Compleja

function recursion_compleja(n, z)
    if n == 0
        return 1
    end

    recursion_compleja(n - 1, z) * z
end

# Parámetros de Entrada
n = 4
z = 1.0 + 2.0im

# Llamada a la Función Recursiva Compleja
recursion_compleja(n, z)

# Creación y Uso de un Tipo Complejo Personalizado

struct PuntoComplejo
    real::Float64
    imaginario::Float64
end

# Definición de un Punto Complejo
punto_complejo = PuntoComplejo(2.0, 3.0)

# Acceder a los Campos del Punto Complejo
punto_complejo.real
punto_complejo.imaginario

# Lógica Condicional Compleja con Operadores de Comparación

if real(x) > real(y) && imag(x) < imag(y)
    println("La parte real de x es mayor que la parte real de y, y la parte imaginaria de x es menor que la parte imaginaria de y.")
end

```