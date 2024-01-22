```groovy
// Importamos la librería de Groovy para trabajar con expresiones regulares
import groovy.text.RegularExpression

// Definimos una variable con el texto de entrada que queremos procesar
String texto = "Esta es una cadena de texto que contiene números y palabras."

// Creamos una expresión regular para encontrar los números en el texto
RegularExpression expresionRegular = ~/[\d]+/

// Usamos la expresión regular para encontrar todos los números en el texto
List<String> numeros = expresionRegular.findAll(texto)

// Imprimimos los números encontrados
println "Números encontrados en el texto:"
numeros.each { println it }

// Definimos una nueva variable con el texto de entrada que queremos procesar
String texto2 = "Esta es otra cadena de texto que contiene palabras y fechas."

// Creamos una expresión regular para encontrar las fechas en el texto
RegularExpression expresionRegular2 = ~/(\d{2})\/(\d{2})\/(\d{4})/

// Usamos la expresión regular para encontrar todas las fechas en el texto
List<String> fechas = expresionRegular2.findAll(texto2)

// Imprimimos las fechas encontradas
println "Fechas encontradas en el texto:"
fechas.each { println it }

// Definimos una nueva variable con el texto de entrada que queremos procesar
String texto3 = "Esta es una cadena de texto que contiene palabras y direcciones de correo electrónico."

// Creamos una expresión regular para encontrar las direcciones de correo electrónico en el texto
RegularExpression expresionRegular3 = /[\w\.\-]+@[\w\.\-]+\.[\w\.\-]+/

// Usamos la expresión regular para encontrar todas las direcciones de correo electrónico en el texto
List<String> correosElectronicos = expresionRegular3.findAll(texto3)

// Imprimimos las direcciones de correo electrónico encontradas
println "Direcciones de correo electrónico encontradas en el texto:"
correosElectronicos.each { println it }
```

Este código se puede explicar de la siguiente manera:

1. **Importamos la librería de Groovy para trabajar con expresiones regulares:**

```groovy
import groovy.text.RegularExpression
```

2. **Definimos una variable con el texto de entrada que queremos procesar:**

```groovy
String texto = "Esta es una cadena de texto que contiene números y palabras."
```

3. **Creamos una expresión regular para encontrar los números en el texto:**

```groovy
RegularExpression expresionRegular = ~/[\d]+/
```

4. **Usamos la expresión regular para encontrar todos los números en el texto:**

```groovy
List<String> numeros = expresionRegular.findAll(texto)
```

5. **Imprimimos los números encontrados:**

```groovy
println "Números encontrados en el texto:"
numeros.each { println it }
```

6. **Definimos una nueva variable con el texto de entrada que queremos procesar:**

```groovy
String texto2 = "Esta es otra cadena de texto que contiene palabras y fechas."
```

7. **Creamos una expresión regular para encontrar las fechas en el texto:**

```groovy
RegularExpression expresionRegular2 = ~/(\d{2})\/(\d{2})\/(\d{4})/
```

8. **Usamos la expresión regular para encontrar todas las fechas en el texto:**

```groovy
List<String> fechas = expresionRegular2.findAll(texto2)
```

9. **Imprimimos las fechas encontradas:**

```groovy
println "Fechas encontradas en el texto:"
fechas.each { println it }
```

10. **Definimos una nueva variable con el texto de entrada que queremos procesar:**

```groovy
String texto3 = "Esta es una cadena de texto que contiene palabras y direcciones de correo electrónico."
```

11. **Creamos una expresión regular para encontrar las direcciones de correo electrónico en el texto:**

```groovy
RegularExpression expresionRegular3 = /[\w\.\-]+@[\w\.\-]+\.[\w\.\-]+/
```

12. **Usamos la expresión regular para encontrar todas las direcciones de correo electrónico en el texto:**

```groovy
List<String> correosElectronicos = expresionRegular3.findAll(texto3)
```

13. **Imprimimos las direcciones de correo electrónico encontradas:**

```groovy
println "Direcciones de correo electrónico encontradas en el texto:"
correosElectronicos.each { println it }
```

Este código es un ejemplo de cómo usar expresiones regulares en Groovy para encontrar y extraer información de una cadena de texto.