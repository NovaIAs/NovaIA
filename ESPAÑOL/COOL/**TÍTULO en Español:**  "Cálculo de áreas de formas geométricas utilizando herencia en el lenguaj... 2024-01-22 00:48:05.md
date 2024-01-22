```cool
-- Definir una clase abstracta `Forma` con un atributo `area` y un método `calcular_area`.

class Forma {
  area: Float;

  calcular_area : () -> Float;
}

-- Definir una clase `Cuadrado` que hereda de `Forma`.

class Cuadrado : Forma {
  lado: Float;

  calcular_area : () -> Float {{
    return lado * lado;
  }}
}

-- Definir una clase `Rectangulo` que hereda de `Forma`.

class Rectangulo : Forma {
  ancho: Float;
  alto: Float;

  calcular_area : () -> Float {{
    return ancho * alto;
  }}
}

-- Definir una clase `Circulo` que hereda de `Forma`.

class Circulo : Forma {
  radio: Float;

  calcular_area : () -> Float {{
    return PI * radio * radio;
  }}
}

-- Crear una lista de objetos `Forma`.

formas : List[Forma] := [
  new Cuadrado(lado = 5.0),
  new Rectangulo(ancho = 10.0, alto = 20.0),
  new Circulo(radio = 3.0)
];

-- Iterar sobre la lista de objetos `Forma` y mostrar el área de cada uno.

for forma in formas {
  println("El área de la forma es " + forma.calcular_area() + " unidades cuadradas.");
}
```

**Explicación:**

* Esta implementación de clases y métodos en COOL crea una forma jerárquica de clases que permiten calcular el área de diferentes tipos de formas geométricas. 


* La clase `Forma` abstracta define un atributo `area` y un método `calcular_area` que se utilizan para calcular el área de la forma. 


* Las clases `Cuadrado`, `Rectangulo` y `Circulo` heredan de la clase `Forma` y proporcionan implementaciones específicas del método `calcular_area` para cada tipo de forma. 


* La lista `formas` se crea con objetos de cada una de las clases concretas. 


* El bucle `for` itera sobre la lista `formas` y muestra el área de cada forma llamando al método `calcular_area` del objeto correspondiente.