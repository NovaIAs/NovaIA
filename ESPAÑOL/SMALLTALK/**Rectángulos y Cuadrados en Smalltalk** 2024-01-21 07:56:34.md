```smalltalk
"Definir una clase llamada 'Rectángulo' con las siguientes propiedades:"

Rectángulo subclass: [Rectángulo]
	instanceVariableNames: 'largo ancho'
	classVariableNames: ''
	poolDictionaries: ''

"Inicializar una instancia de la clase 'Rectángulo' con el largo y ancho dados:"

Rectángulo new
	largo: 100;
	ancho: 50

"Definir un método llamado 'área' en la clase 'Rectángulo' que calcula y devuelve el área del rectángulo:"

Rectángulo methodsFor: 'calculating' add: [área]
	^ largo * ancho

"Definir un método llamado 'perímetro' en la clase 'Rectángulo' que calcula y devuelve el perímetro del rectángulo:"

Rectángulo methodsFor: 'calculating' add: [perímetro]
	^ 2 * (largo + ancho)

"Definir una clase llamada 'Cuadrado' como subclase de 'Rectángulo' con las siguientes propiedades:"

Cuadrado subclass: [Cuadrado] of: Rectángulo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''

"Inicializar una instancia de la clase 'Cuadrado' con el lado dado:"

Cuadrado new
	lado: 10

"Definir un método llamado 'área' en la clase 'Cuadrado' que devuelve el área del cuadrado:"

Cuadrado methodsFor: 'calculating' add: [área]
	^ lado * lado

"Definir un método llamado 'perímetro' en la clase 'Cuadrado' que devuelve el perímetro del cuadrado:"

Cuadrado methodsFor: 'calculating' add: [perímetro]
	^ 4 * lado

"Crear una instancia de la clase 'Rectángulo' con el largo y ancho dados:"

miRectángulo := Rectángulo new
	largo: 100;
	ancho: 50

"Crear una instancia de la clase 'Cuadrado' con el lado dado:"

miCuadrado := Cuadrado new
	lado: 10

"Imprimir el área y el perímetro del rectángulo:"

Transcript show: 'Área del rectángulo: ', miRectángulo área; cr.
Transcript show: 'Perímetro del rectángulo: ', miRectángulo perímetro; cr.

"Imprimir el área y el perímetro del cuadrado:"

Transcript show: 'Área del cuadrado: ', miCuadrado área; cr.
Transcript show: 'Perímetro del cuadrado: ', miCuadrado perímetro; cr.
```

Explicación del código:

1. Creamos la clase `Rectángulo` con dos variables de instancia: `largo` y `ancho`.

2. En la clase `Rectángulo`, definimos dos métodos: `área` y `perímetro` para calcular y devolver el área y el perímetro del rectángulo, respectivamente.

3. Creamos la clase `Cuadrado` como subclase de `Rectángulo`, que hereda las variables de instancia y los métodos de `Rectángulo`.

4. En la clase `Cuadrado`, definimos el método `lado` para establecer el lado del cuadrado. Además, redefinimos los métodos `área` y `perímetro` para calcular el área y el perímetro del cuadrado, utilizando el lado como variable.

5. Creamos instancias de las clases `Rectángulo` y `Cuadrado` con los valores dados.

6. Imprimimos el área y el perímetro del rectángulo y del cuadrado.