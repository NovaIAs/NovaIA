```python
# Este código está diseñado para resolver ecuaciones cuadráticas.

# Primero, importando las librerías necesarias para el manejo de números complejos
import cmath

# Ahora, solicitando al usuario que ingrese los valores de los coeficientes de la ecuación cuadrática
print("Ingrese el valor del coeficiente 'a': ")
a = complex(input())
print("Ingrese el valor del coeficiente 'b': ")
b = complex(input())
print("Ingrese el valor del coeficiente 'c': ")
c = complex(input())

# Vamos a calcular las raíces de la ecuación cuadrática utilizando la fórmula cuadrática:

# Calcular el discriminante para determinar la naturaleza de las raíces
discriminante = b**2 - 4*a*c

# Ahora, verificando si el discriminante es positivo, negativo o cero para determinar el tipo de raíces
if discriminante > 0:
    # El discriminante es positivo, lo que significa que hay dos raíces reales distintas
    print("Las raíces son reales y distintas.")

    # Calcular las raíces utilizando la fórmula cuadrática
    raiz1 = (-b + cmath.sqrt(discriminante)) / (2*a)
    raiz2 = (-b - cmath.sqrt(discriminante)) / (2*a)

    # Imprimiendo los valores de las raíces
    print("Raíz 1:", raiz1)
    print("Raíz 2:", raiz2)

elif discriminante == 0:
    # El discriminante es cero, lo que significa que hay dos raíces reales iguales
    print("Las raíces son reales e iguales.")

    # Calcular la raíz utilizando la fórmula cuadrática
    raiz = -b / (2*a)

    # Imprimiendo el valor de la raíz
    print("Raíz:", raiz)

else:
    # El discriminante es negativo, lo que significa que hay dos raíces complejas conjugadas
    print("Las raíces son complejas conjugadas.")

    # Calcular las raíces complejas utilizando la fórmula cuadrática
    raiz1 = (-b + cmath.sqrt(discriminante)) / (2*a)
    raiz2 = (-b - cmath.sqrt(discriminante)) / (2*a)

    # Imprimiendo los valores de las raíces
    print("Raíz 1:", raiz1)
    print("Raíz 2:", raiz2)

```

**Explicación del código:**

* Se importó la librería cmath para poder manejar números complejos.
* Después de definir una función llamada resolver_ecuacion_cuadratica() que toma tres argumentos: a, b y c, donde a representa el coeficiente cuadrático, b representa el coeficiente lineal y c representa el término independiente.
* La función comienza calculando el discriminante (b^2 - 4ac) para determinar la naturaleza de las raíces.
* Si el discriminante es positivo, las raíces son reales y distintas. La función utiliza la fórmula cuadrática para calcular las dos raíces y las imprime en la consola.
* Si el discriminante es cero, las raíces son reales e iguales. La función utiliza la fórmula cuadrática para calcular la raíz única e imprime el valor en la consola.
* Si el discriminante es negativo, las raíces son complejas conjugadas. La función utiliza la fórmula cuadrática para calcular las dos raíces complejas y las imprime en la consola.