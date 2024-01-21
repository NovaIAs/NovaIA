```julia
# Definición de una función
function miFuncion(x::Real)
    # Cálculo del seno de x
    seno_x = sin(x)

    # Cálculo del coseno de x
    coseno_x = cos(x)

    # Cálculo de la tangente de x
    tangente_x = tan(x)

    # Devolución de los resultados
    return seno_x, coseno_x, tangente_x
end

# Creación de un vector con los valores de x
x_vector = linspace(0, 2π, 100)

# Cálculo de los valores de seno, coseno y tangente para cada valor de x
seno_vector, coseno_vector, tangente_vector = map(miFuncion, x_vector)

# Gráfica de los valores calculados
plot(x_vector, seno_vector, label="Seno")
plot!(x_vector, coseno_vector, label="Coseno")
plot!(x_vector, tangente_vector, label="Tangente")

# Leyenda de la gráfica
legend()

# Título de la gráfica
title("Gráfica de seno, coseno y tangente")

# Ejes de la gráfica
xlabel("x")
ylabel("y")

# Visualización de la gráfica
show()
```

Explicación del código:

1. Definimos una función llamada `miFuncion` que recibe como argumento un número real `x` y devuelve el seno, coseno y tangente de `x`.

2. Creamos un vector de valores de `x` que van desde 0 hasta 2π con 100 puntos.

3. Calculamos los valores de seno, coseno y tangente para cada valor de `x` utilizando la función `map` y la función `miFuncion`.

4. Creamos una gráfica con los valores calculados utilizando la función `plot`.

5. Añadimos la leyenda de la gráfica utilizando la función `legend()`.

6. Añadimos el título de la gráfica utilizando la función `title()`.

7. Añadimos los ejes de la gráfica utilizando las funciones `xlabel()` e `ylabel()`.

8. Visualizamos la gráfica utilizando la función `show()`.