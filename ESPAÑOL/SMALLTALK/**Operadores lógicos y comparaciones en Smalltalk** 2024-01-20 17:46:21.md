```smalltalk

;; Enumeración General
EnumSingleton := #('Cliente' 'Trabajador' 'Administrador')
EnumSingleton := EnumSingleton asEnumeration.

;; Cláusulas y condiciones simples.
s1 := 'Eugenio'.
s2 := 'Eugenio'.

si s1 = s2 [ "Eugenio" printCR ] [ "!" printCR ].


c1 := (s1 = s2) and: [s1 == s2].
c2 := (s1 = s2) or: [s1 == s2].
c3 := s1 <= s2.
c4 := s1 >= s2.
c5 := s1 < s2.
c6 := s1 > s2.
c7 := s1 <> s2.
c8 := [1 2 3] notIncludes: 4.

c1 printCR.
c2 printCR.
c3 printCR.
c4 printCR.
c5 printCR.
c6 printCR.
c7 printCR.
c8 printCR.


;; Cláusulas y condiciones con variables.
n1 := 123.
n2 := 456.
res := n2 div: n1.

si res = 3 [
	"La división de n2 entre n1 es 3" printCR
] si res < 3 [
	"La división de n2 entre n1 es menor que 3" printCR
] si res > 3 [
	"La división de n2 entre n1 es mayor que 3" printCR
].

;; Cláusulas y condiciones con rango
l1 := [1 2 3 4 5].
l2 := [11 22 33 44 55].

[(l1 size) to: (l2 size)] do: [ :i | l1 at: i print. l2 at: i printCR ].

```

Explicación del código:

* Se define una enumeración llamada `EnumSingleton` que contiene tres valores: `Cliente`, `Trabajador` y `Administrador`.
* Se comparan dos cadenas de texto, `s1` y `s2`, utilizando diferentes operadores de comparación, como `=`, `==`, `<=`, `>=`, `<`, `>`, y `<>`.
* Se evalúan expresiones booleanas complejas utilizando operadores lógicos como `and`, `or`, y `not`.
* Se utiliza una cláusula `si` para evaluar una variable numérica, `n1`, y realizar diferentes acciones en función del resultado de la evaluación.
* Se utiliza una cláusula `si` para evaluar una variable lista, `l1`, y realizar diferentes acciones en función de la longitud de la lista.
* Se utiliza un bucle `do:` para recorrer un rango de valores y realizar una acción para cada valor del rango.