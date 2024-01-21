```
"Objeto punto"

Punto := Class new.

Punto initialize: x y [
  super initialize.
  x: x.
  y: y.
].

Punto + p [
  ^(Punto new x: self x + p x y: self y + p y)
].

"Objeto recta"

Recta := Class new.

Recta initialize: p q [
  super initialize.
  p: p.
  q: q.
].

Recta m [
  ^(q y - p y) / (q x - p x)
].

Recta b [
  ^p y - m * p x
].

Recta from: p to: q [
  ^Recta new p: p q: q
].

"Objeto círculo"

Circulo := Class new.

Circulo initialize: c r [
  super initialize.
  c: c.
  r: r
].

Circulo area [
  ^Pi * r * r
].

Circulo perimetro [
  ^2 * Pi * r
].

"Objeto triángulo"

Triangulo := Class new.

Triangulo initialize: a b c [
  super initialize.
  a: a.
  b: b.
  c: c.
].

Triangulo area [
  ^(a x * (b y - c y) + b x * (c y - a y) + c x * (a y - b y)) / 2
].

Triangulo perimetro [
  ^a + b + c
].

"Objeto cuadrado"

Cuadrado := Class new.

Cuadrado initialize: l [
  super initialize.
  l: l.
].

Cuadrado area [
  ^l * l
].

Cuadrado perimetro [
  ^4 * l
].

"Objeto rectángulo"

Rectangulo := Class new.

Rectangulo initialize: an al [
  super initialize.
  an: an.
  al: al.
].

Rectangulo area [
  ^an * al
].

Rectangulo perimetro [
  ^2 * (an + al)
].

"Objeto polígono"

Poligono := Class new.

Poligono initialize: ps [
  super initialize.
  ps: ps.
].

Poligono area [
  ^0
].

Poligono perimetro [
  ^0
].

"Instanciar objetos"

p1 := Punto new x: 1 y: 2.
p2 := Punto new x: 3 y: 4.
r := Recta new p: p1 q: p2.
c := Circulo new c: p1 r: 2.
t := Triangulo new a: p1 b: p2 c: Punto new x: 5 y: 6.
cu := Cuadrado new l: 3.
re := Rectangulo new an: 4 al: 5.
po := Poligono new ps: [p1 p2 Punto new x: 7 y: 8].

"Calcular propiedades"

r m.
r b.
c area.
c perimetro.
t area.
t perimetro.
cu area.
cu perimetro.
re area.
re perimetro.
po area.
po perimetro.
```

Este código Smalltalk crea varias clases de objetos geométricos, como puntos, rectas, círculos, triángulos, cuadrados, rectángulos y polígonos. Cada clase tiene sus propias propiedades y métodos. Por ejemplo, la clase Punto tiene las propiedades x e y, y el método + para sumar dos puntos. La clase Recta tiene las propiedades p y q, y los métodos m y b para calcular la pendiente y el intercepto de la recta.

El código también instancia varios objetos de estas clases y calcula sus propiedades. Por ejemplo, el objeto r es una recta que pasa por los puntos p1 y p2. El código calcula la pendiente y el intercepto de r usando los métodos m y b. El objeto c es un círculo con centro en p1 y radio 2. El código calcula el área y el perímetro de c usando los métodos area y perimetro.

Este código es un ejemplo de cómo usar Smalltalk para crear objetos y calcular sus propiedades. Smalltalk es un lenguaje de programación orientado a objetos, lo que significa que puede crear fácilmente objetos que representen objetos del mundo real. Smalltalk también tiene una sintaxis muy sencilla, lo que hace que sea fácil de aprender y usar.