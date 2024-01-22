```smalltalk
"Clase que modela un punto en el espacio bidimensional."
Object subclass: [ Punto ]
	instanceVariableNames: 'x y'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Geometría'

"Inicializa un nuevo punto con las coordenadas especificadas."
[ :unX :unY ] -> Punto new
	super new.
	x := unX.
	y := unY.

"Devuelve la coordenada x del punto."
[  ] -> Punto>>x
	^x

"Devuelve la coordenada y del punto."
[  ] -> Punto>>y
	^y

"Modifica la coordenada x del punto."
[ :nuevoX ] -> Punto>>#x: nuevoX
	x := nuevoX

"Modifica la coordenada y del punto."
[ :nuevoY ] -> Punto>>#y: nuevoY
	y := nuevoY

"Devuelve el punto negado (el punto que tiene las coordenadas opuestas)."
[  ] -> Punto>>negado
	^Punto new x: -x y: -y

"Devuelve la distancia entre este punto y otro punto."
[ :otroPunto ] -> Punto>>distanciaA: otroPunto
	^Math sqrt: ((x - otroPunto x) ** 2 + (y - otroPunto y) ** 2)

"Devuelve una cadena de texto que representa al punto."
[  ] -> Punto>>comoTexto
	^'[' x comoTexto ', ' y comoTexto ']'

"Clase que modela un triángulo en el espacio bidimensional."
Object subclass: [ Triángulo ]
	instanceVariableNames: 'vértice1 vértice2 vértice3'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Geometría'

"Inicializa un nuevo triángulo con los vértices especificados."
[ :v1 :v2 :v3 ] -> Triángulo new
	super new.
	vértice1 := v1.
	vértice2 := v2.
	vértice3 := v3.

"Devuelve el vértice 1 del triángulo."
[  ] -> Triángulo>>vértice1
	^vértice1

"Devuelve el vértice 2 del triángulo."
[  ] -> Triángulo>>vértice2
	^vértice2

"Devuelve el vértice 3 del triángulo."
[  ] -> Triángulo>>vértice3
	^vértice3

"Modifica el vértice 1 del triángulo."
[ :nuevoVértice1 ] -> Triángulo>>#vértice1: nuevoVértice1
	vértice1 := nuevoVértice1

"Modifica el vértice 2 del triángulo."
[ :nuevoVértice2 ] -> Triángulo>>#vértice2: nuevoVértice2
	vértice2 := nuevoVértice2

"Modifica el vértice 3 del triángulo."
[ :nuevoVértice3 ] -> Triángulo>>#vértice3: nuevoVértice3
	vértice3 := nuevoVértice3

"Devuelve una cadena de texto que representa al triángulo."
[  ] -> Triángulo>>comoTexto
	^'{ ' vértice1 comoTexto ', ' vértice2 comoTexto ', ' vértice3 comoTexto ' }'

"Clase que modela un círculo en el espacio bidimensional."
Object subclass: [ Círculo ]
	instanceVariableNames: 'centro radio'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Geometría'

"Inicializa un nuevo círculo con el centro y el radio especificados."
[ :unCentro :unRadio ] -> Círculo new
	super new.
	centro := unCentro.
	radio := unRadio.

"Devuelve el centro del círculo."
[  ] -> Círculo>>centro
	^centro

"Devuelve el radio del círculo."