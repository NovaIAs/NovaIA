```
**Programa en Portugués: Factorial de un número**

**Variables declaradas:**
- **n:** Número para el cual se calculará el factorial.
- **i:** Variable auxiliar para el cálculo del factorial.
- **factorial:** Resultado del cálculo del factorial.

**Inicio del programa:**
```
INICIO
```

**Entrada de datos:**
Se solicita al usuario que ingrese el número para el cual se calculará el factorial.
```
ESCRIBE "Ingrese el número para calcular el factorial: "
LEE n
```

**Cálculo del factorial:**
Se utiliza un ciclo **PARA** para calcular el factorial. El ciclo itera desde 1 hasta el valor de **n**. En cada iteración, el valor actual de **i** se multiplica por el valor actual de **factorial**.
```
factorial <- 1
PARA i DE 1 HASTA n
   factorial <- i * factorial
FIN PARA
```

**Salida de datos:**
Se muestra el resultado del cálculo del factorial en pantalla.
```
ESCRIBE "El factorial de " + n + " es " + factorial
```

**Fin del programa:**
```
FIN
```

**Explicación del código:**

- El programa comienza declarando las variables **n**, **i** y **factorial**.
- La variable **n** se utiliza para almacenar el número para el cual se calculará el factorial.
- La variable **i** se utiliza como variable auxiliar para el cálculo del factorial.
- La variable **factorial** se utiliza para almacenar el resultado del cálculo del factorial.
- A continuación, el programa solicita al usuario que ingrese el número para el cual se calculará el factorial.
- El programa utiliza un ciclo **PARA** para calcular el factorial. El ciclo itera desde 1 hasta el valor de **n**. En cada iteración, el valor actual de **i** se multiplica por el valor actual de **factorial**.
- Finalmente, el programa muestra el resultado del cálculo del factorial en pantalla.