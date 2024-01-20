```smalltalk
"Definir una clase llamada 'Rectangulo' que tenga dos atributos, 'largo' y 'ancho'."
Object subclass: Rectangulo [
    "Definir los atributos de la clase 'Rectangulo'."
    instanceVariableNames: 'largo ancho'
]

"Definir un método llamado 'calcularArea' en la clase 'Rectangulo'."
Rectangulo >> calcularArea [
    "Calcular el área del rectángulo multiplicando el largo por el ancho."
    ^ self largo * self ancho
]

"Crear una instancia de la clase 'Rectangulo' con un largo de 10 y un ancho de 5."
rectangulo := Rectangulo new.
rectangulo largo: 10.
rectangulo ancho: 5.

"Imprimir el área del rectángulo en la consola."
Transcript show: 'El área del rectángulo es: ', rectangulo calcularArea, cr.
```

**Explicación:**

* La primera línea define una nueva clase llamada `Rectangulo` que hereda de la clase `Object`.
* La segunda línea define los atributos de la clase `Rectangulo`. En este caso, los atributos son `largo` y `ancho`.
* La tercera línea define un método llamado `calcularArea` en la clase `Rectangulo`. Este método calcula el área del rectángulo multiplicando el largo por el ancho.
* La cuarta línea crea una instancia de la clase `Rectangulo` con un largo de 10 y un ancho de 5.
* La quinta línea imprime el área del rectángulo en la consola.

Este código crea una clase llamada `Rectangulo` que tiene dos atributos, `largo` y `ancho`. También define un método llamado `calcularArea` que calcula el área del rectángulo multiplicando el largo por el ancho. Luego, crea una instancia de la clase `Rectangulo` con un largo de 10 y un ancho de 5, y finalmente imprime el área del rectángulo en la consola.