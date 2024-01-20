```scala
// Importamos las clases necesarias para trabajar con vectores y computación numérica
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.sqrt

// Definimos las matrices A y B de tamaño 3x3
val A = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0), (7.0, 8.0, 9.0))
val B = DenseMatrix((10.0, 11.0, 12.0), (13.0, 14.0, 15.0), (16.0, 17.0, 18.0))

// Calculamos la multiplicación de matrices A y B
val C = A * B

// Imprimimos la matriz C
println("Matriz C:")
println(C)

// Definimos un vector con 3 componentes
val x = DenseVector(1.0, 2.0, 3.0)

// Calculamos el producto escalar del vector x consigo mismo
val x_dot_x = x dot x

// Imprimimos el resultado del producto escalar
println("Producto escalar de x consigo mismo:")
println(x_dot_x)

// Calculamos la norma euclidiana del vector x
val x_norm = sqrt(x_dot_x)

// Imprimimos la norma euclidiana del vector x
println("Norma euclidiana del vector x:")
println(x_norm)
```

Explicación del código:

* En primer lugar, importamos las clases necesarias para trabajar con vectores y computación numérica.
* A continuación, definimos las matrices A y B de tamaño 3x3 y las inicializamos con algunos valores.
* Calculamos la multiplicación de matrices A y B utilizando la operación `*` y almacenamos el resultado en la matriz C.
* Imprimimos la matriz C para visualizar los resultados de la multiplicación de matrices.
* Definimos un vector con 3 componentes y calculamos el producto escalar del vector x consigo mismo utilizando la operación `dot`.
* Imprimimos el resultado del producto escalar para comprobar si el vector x es perpendicular a sí mismo.
* Por último, calculamos la norma euclidiana del vector x utilizando la función `sqrt` y la raíz cuadrada del producto escalar.
* Imprimimos la norma euclidiana del vector x para obtener la longitud del vector en el espacio euclidiano.