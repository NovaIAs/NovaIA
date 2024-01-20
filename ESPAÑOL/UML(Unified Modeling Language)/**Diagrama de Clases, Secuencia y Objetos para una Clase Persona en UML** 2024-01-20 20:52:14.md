**Diagrama de Clases**

```
+----------------+
| Clase Persona |
+----------------+
| + nombre: String |
| + edad: int |
| + dirección: String |
| + teléfono: String |
| + correoElectrónico: String |
| +----------------------+
| + Constructor(nombre, edad, dirección, teléfono, correoElectrónico) |
| + getNombre(): String |
| + getEdad(): int |
| + getDirección(): String |
| + getTeléfono(): String |
| + getCorreoElectrónico(): String |
| + setNombre(nombre: String): void |
| + setEdad(edad: int): void |
| + setDirección(dirección: String): void |
| + setTeléfono(teléfono: String): void |
| + setCorreoElectrónico(correoElectrónico: String): void |
+----------------------+

```

**Diagrama de Secuencia**

```
Persona: crear()
Persona: getNombre()
Persona: getEdad()
Persona: getDirección()
Persona: getTeléfono()
Persona: getCorreoElectrónico()
Persona: setNombre()
Persona: setEdad()
Persona: setDirección()
Persona: setTeléfono()
Persona: setCorreoElectrónico()

```

**Diagrama de Objetos**

```
+----------------+
| Persona 1 |
+----------------+
| nombre: Juan |
| edad: 20 |
| dirección: Calle Mayor 123 |
| teléfono: 666 666 666 |
| correoElectrónico: juan@ejemplo.com |
+----------------------+

+----------------+
| Persona 2 |
+----------------+
| nombre: María |
| edad: 25 |
| dirección: Calle Menor 456 |
| teléfono: 777 777 777 |
| correoElectrónico: maria@ejemplo.com |
+----------------------+

```

**Explicación del código**

El código UML anterior define una clase Persona con los atributos nombre, edad, dirección, teléfono y correoElectrónico. La clase también tiene un constructor que inicializa los atributos con los valores proporcionados como argumentos. Los métodos de la clase permiten obtener y establecer los valores de los atributos.

El diagrama de secuencia muestra cómo se llama a los métodos de la clase Persona para crear un objeto Persona, obtener los valores de sus atributos y establecer nuevos valores para ellos.

El diagrama de objetos muestra dos objetos Persona, cada uno con sus propios valores de atributo.