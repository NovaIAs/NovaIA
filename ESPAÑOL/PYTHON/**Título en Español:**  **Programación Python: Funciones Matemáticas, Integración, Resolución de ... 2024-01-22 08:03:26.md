```
from math import *
from scipy.integrate import *
from sympy import *

# Definir una función
def f(x):
    return x**2 + 2*x - 3

# Calcular la integral de la función
integral = integrate(f, (x, 0, 1))

# Resolver una ecuación diferencial
y = Function('y')
eq = Eq(y.diff(x), y**2)
sol = dsolve(eq, (y, x))

# Crear una matriz
A = Matrix([[1, 2, 3], [4, 5, 6], [7, 8, 9]])

# Calcular el determinante de la matriz
det = A.det()

# Calcular los valores propios de la matriz
eigenvalues = A.eigenvals()

# Graficar la función
x = np.linspace(-5, 5, 100)
y = f(x)
plt.plot(x, y)
plt.show()

# Crear un diccionario
d = {'nombre': 'Juan', 'edad': 20, 'ocupacion': 'estudiante'}

# Recorrer el diccionario
for key, value in d.items():
    print(key, ':', value)
```

Explicación del código:

* La primera línea importa la biblioteca `math` que contiene funciones matemáticas básicas.
* La segunda línea importa la biblioteca `scipy.integrate` que contiene funciones para calcular integrales.
* La tercera línea importa la biblioteca `sympy` que contiene funciones para resolver ecuaciones simbólicas.
* La cuarta línea define una función `f(x)` que calcula el valor de la función `x^2 + 2x - 3`.
* La quinta línea calcula la integral de la función `f(x)` entre los límites `0` y `1`.
* La sexta línea resuelve la ecuación diferencial `y' = y^2` con la condición inicial `y(0) = 1`.
* La séptima línea crea una matriz `A` de 3x3.
* La octava línea calcula el determinante de la matriz `A`.
* La novena línea calcula los valores propios de la matriz `A`.
* La décima línea grafica la función `f(x)` entre los límites `-5` y `5`.
* La undécima línea crea un diccionario `d` con tres claves: `nombre`, `edad` y `ocupacion`.
* La duodécima línea recorre el diccionario `d` e imprime cada clave y su valor correspondiente.