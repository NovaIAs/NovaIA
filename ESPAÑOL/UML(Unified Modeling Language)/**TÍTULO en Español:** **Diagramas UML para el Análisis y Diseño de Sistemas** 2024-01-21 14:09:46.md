**Diagrama de Clases**

```
+----------------+
| Clase Persona |
+----------------+
| Atributos:      |
| - nombre        |
| - edad           |
| - género         |
+----------------+
| Métodos:        |
| - caminar()      |
| - hablar()       |
| - dormir()       |
+----------------+

+---------------------+
| Clase Trabajador |
+---------------------+
| Atributos:      |
| - nombre        |
| - edad           |
| - género         |
| - ocupación     |
+---------------------+
| Métodos:        |
| - trabajar()      |
| - ganarDinero() |
+---------------------+

+--------------------------+
| Clase Estudiante |
+--------------------------+
| Atributos:      |
| - nombre        |
| - edad           |
| - género         |
| - grado          |
+--------------------------+
| Métodos:        |
| - estudiar()     |
| - tomarNotas() |
| - hacerExámenes() |
+--------------------------+
```

**Diagrama de Objetos**

```
+-------------------+
| Persona Juan     |
+-------------------+
| nombre: Juan     |
| edad: 20         |
| género: masculino |
+-------------------+

+----------------------------+
| Trabajador María López      |
+----------------------------+
| nombre: María López       |
| edad: 30                 |
| género: femenino           |
| ocupación: enfermera       |
+----------------------------+

+--------------------------+
| Estudiante Pedro Sánchez |
+--------------------------+
| nombre: Pedro Sánchez  |
| edad: 18               |
| género: masculino       |
| grado: 11              |
+--------------------------+
```

**Diagrama de Casos de Uso**

```
+--------------------+
| Caso de Uso: Comprar |
+--------------------+
| Actores:           |
| - Cliente          |
| - Vendedor        |
+--------------------+
| Flujo de eventos:  |
| 1. El cliente entra en la tienda. |
| 2. El cliente elige un producto. |
| 3. El cliente lleva el producto a la caja. |
| 4. El vendedor escanea el producto. |
| 5. El vendedor le dice al cliente el precio del producto. |
| 6. El cliente paga al vendedor. |
| 7. El vendedor le da al cliente el producto. |
| 8. El cliente sale de la tienda. |
+--------------------+
```

**Diagrama de Secuencia**

```
+--------------------------------------+
| Caso de Uso: Comprar                 |
+--------------------------------------+
| Actor: Cliente                       |
+--------------------------------------+
| Flujo de eventos:                  |
| 1. Cliente entra en la tienda.        |
| 2. Cliente elige un producto.        |
| 3. Cliente lleva el producto a la caja. |
| 4. Vendedor escanea el producto.   |
| 5. Vendedor le dice al cliente el precio del producto. |
| 6. Cliente paga al vendedor.         |
| 7. Vendedor le da al cliente el producto. |
| 8. Cliente sale de la tienda.        |
+--------------------------------------+
```

**Explicación del código:**

El código anterior es un ejemplo de un análisis y diseño de sistema utilizando UML. El diagrama de clases define las clases del sistema, sus atributos y métodos. El diagrama de objetos muestra las instancias de las clases y sus relaciones. El diagrama de casos de uso define los casos de uso del sistema, los actores que participan en cada caso de uso y el flujo de eventos de cada caso de uso. El diagrama de secuencia muestra la secuencia de eventos en un caso de uso.

Este código es complejo y amplio, pero es un ejemplo de cómo se pueden utilizar las herramientas de UML para analizar y diseñar sistemas. UML es un lenguaje estándar que se utiliza en todo el mundo para el desarrollo de software.