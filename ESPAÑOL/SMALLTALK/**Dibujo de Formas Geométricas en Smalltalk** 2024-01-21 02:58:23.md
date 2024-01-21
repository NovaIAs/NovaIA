```smalltalk
"Definir un objeto `Circulo` con propiedades de centro y radio"
Circulo := Objeto nuevo.
Circulo centro: Punto nuevo coordenadaX: 0.0 coordenadaY: 0.0.
Circulo radio: 10.0.

"Definir un objeto `Rectangulo` con propiedades de origen, ancho y alto"
Rectangulo := Objeto nuevo.
Rectangulo origen: Punto nuevo coordenadaX: 0.0 coordenadaY: 0.0.
Rectangulo ancho: 20.0.
Rectangulo alto: 10.0.

"Definir un método `dibujar` para `Circulo`"
Circulo definirDibujar: [
  "Obtener el contexto de gráficos actual (p. ej., una ventana o un buffer de imagen)"
  contexto := Gráficos contextoActual.

  "Establecer el color de relleno del círculo"
  contexto colorDeRelleno: Color rojo.

  "Dibujar el círculo"
  contexto círculo centro: centro radio: radio.
].

"Definir un método `dibujar` para `Rectangulo`"
Rectangulo definirDibujar: [
  "Obtener el contexto de gráficos actual (p. ej., una ventana o un buffer de imagen)"
  contexto := Gráficos contextoActual.

  "Establecer el color de relleno del rectángulo"
  contexto colorDeRelleno: Color azul.

  "Dibujar el rectángulo"
  contexto rectángulo origen: origen ancho: ancho alto: alto.
].

"Crear una lista de objetos de forma (círculos y rectángulos)"
formas := Lista nueva.
formas añadir: Circulo.
formas añadir: Rectangulo.

"Iterar sobre la lista de formas y dibujar cada una"
formas hacer: [:forma |
  forma dibujar.
].
```

**Explicación del código:**

1. Primero, definimos dos clases, `Circulo` y `Rectangulo`, que representan formas geométricas básicas.
2. Cada clase tiene su propio conjunto de propiedades: `Circulo` tiene un centro y un radio, mientras que `Rectangulo` tiene un origen, ancho y alto.
3. A continuación, definimos dos métodos de dibujo, `dibujar` para `Circulo` y `dibujar` para `Rectangulo`. Estos métodos se utilizan para dibujar las formas en una ventana o en un buffer de imagen.
4. Finalmente, creamos una lista de objetos de forma (círculos y rectángulos) y usamos un bucle `hacer:` para iterar sobre la lista y dibujar cada forma.

Este código es una demostración básica de cómo crear y dibujar formas geométricas en Smalltalk. Se puede ampliar fácilmente para soportar más formas y operaciones más complejas.