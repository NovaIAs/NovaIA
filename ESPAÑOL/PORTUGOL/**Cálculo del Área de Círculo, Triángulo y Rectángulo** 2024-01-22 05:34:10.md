```
// Programa que calcula el área de un círculo, de un triángulo y de un rectángulo.

// Definir las variables
real radio, base, altura, lado1, lado2, area_circulo, area_triangulo, area_rectangulo;

// Leer los datos del usuario
print("Ingrese el radio del círculo: ");
read(radio);
print("Ingrese la base del triángulo: ");
read(base);
print("Ingrese la altura del triángulo: ");
read(altura);
print("Ingrese el lado 1 del rectángulo: ");
read(lado1);
print("Ingrese el lado 2 del rectángulo: ");
read(lado2);

// Calcular el área del círculo
area_circulo = pow(radio, 2) * pi;

// Calcular el área del triángulo
area_triangulo = (base * altura) / 2;

// Calcular el área del rectángulo
area_rectangulo = lado1 * lado2;

// Imprimir los resultados
print();
print("El área del círculo es:", area_circulo);
print("El área del triángulo es:", area_triangulo);
print("El área del rectángulo es:", area_rectangulo);

```

Explicación del código:

* El programa comienza definiendo las variables que se utilizarán.
* A continuación, se leen los datos del usuario.
* Luego, se calculan las áreas de las figuras geométricas utilizando las fórmulas correspondientes.
* Finalmente, se imprimen los resultados en la consola.