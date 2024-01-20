```julia

# Importar bibliotecas necesarias
using LinearAlgebra, Plots, Optim

# Definir una función objetivo
function objetivo(x, y)
    return x^2 + y^2
end

# Definir las restricciones
function restricciones(x, y)
    return [x + y <= 10, x - y >= 1]
end

# Crear un problema de optimización
problema = Optim.Problem(objetivo, restricciones)

# Resolver el problema de optimización
resultado = Optim.optimize(problema)

# Obtener los valores óptimos de x e y
x_optimo = resultado.solution[1]
y_optimo = resultado.solution[2]

# Mostrar los valores óptimos
println("Valores óptimos: x = $x_optimo, y = $y_optimo")

# Crear una gráfica con los valores óptimos
plot(objetivo, (x, y) -> x_optimo, (x, y) -> y_optimo)

```

Explicación del código:

* Se importan las bibliotecas necesarias para realizar el cálculo de optimización y generar gráficos.
* Se define la función objetivo que se quiere minimizar. En este caso, es la suma de los cuadrados de x e y.
* Se definen las restricciones que deben cumplirse. En este caso, la suma de x e y debe ser menor o igual a 10, y la diferencia entre x e y debe ser mayor o igual a 1.
* Se crea un problema de optimización utilizando la función Optim.Problem().
* Se resuelve el problema de optimización utilizando la función Optim.optimize().
* Se obtienen los valores óptimos de x e y.
* Se muestran los valores óptimos.
* Se crea una gráfica con los valores óptimos.