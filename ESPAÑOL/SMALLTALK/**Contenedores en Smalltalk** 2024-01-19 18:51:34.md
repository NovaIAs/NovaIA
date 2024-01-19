```smalltalk
"Definimos la clase 'Contenedor' con dos variables de instancia 'elementos' y 'capacidad'."
Contenedor subclass: [ Contenedor |

    instanceVariableNames: 'elementos capacidad'

    "El método 'inicializarConCapacidad' establece la capacidad inicial del contenedor."
    class initializer: [ :capacidad |
        super initialize.
        elementos := OrderedCollection new.
        capacidad := capacidad.
    ]

    "El método 'agregar' agrega un elemento al contenedor si hay espacio disponible."
    agregar: [ :elemento |
        capacidad >= elementos size ifTrue: [ |
            elementos add: elemento.
        ].
    ]

    "El método 'remover' remueve un elemento del contenedor."
    remover: [ :elemento |
        elementos remove: elemento.
    ]

    "El método 'tamanyo' devuelve el número de elementos en el contenedor."
    tamanyo: [ |
        elementos size.
    ]

    "El método 'capacidad' devuelve la capacidad máxima del contenedor."
    capacidad: [ |
        capacidad.
    ]

    "El método 'estaVacio' determina si el contenedor está vacío."
    estaVacio: [ |
        elementos isEmpty.
    ]

    "El método 'estaLleno' determina si el contenedor está lleno."
    estaLleno: [ |
        elementos size = capacidad.
    ]

    "El método 'tomarTodos' devuelve todos los elementos del contenedor como una colección."
    tomarTodos: [ |
        elementos copy.
    ]
]

"Definimos una instancia de la clase 'Contenedor' llamada 'miContenedor' con una capacidad de 5."
miContenedor := Contenedor initializeWithCapacity: 5.

"Agregamos elementos al contenedor."
miContenedor agregar: 1.
miContenedor agregar: 2.
miContenedor agregar: 3.

"Obtenemos el número de elementos en el contenedor."
println: miContenedor tamanyo.

"Verificamos si el contenedor está lleno."
println: miContenedor estaLleno.

"Removemos un elemento del contenedor."
miContenedor remover: 2.

"Recuperamos todos los elementos del contenedor como una colección."
coleccion := miContenedor tomarTodos.

"Imprimimos los elementos en la consola."
coleccion do: [ :elemento | println: elemento ].
```

Explicación del código:

* Definimos una clase 'Contenedor' que representa un contenedor de elementos con una capacidad máxima.
* El método 'inicializarConCapacidad' de la clase 'Contenedor' establece la capacidad inicial del contenedor.
* El método 'agregar' de la clase 'Contenedor' agrega un elemento al contenedor si hay espacio disponible.
* El método 'remover' de la clase 'Contenedor' remueve un elemento del contenedor.
* El método 'tamanyo' de la clase 'Contenedor' devuelve el número de elementos en el contenedor.
* El método 'capacidad' de la clase 'Contenedor' devuelve la capacidad máxima del contenedor.
* El método 'estaVacio' de la clase 'Contenedor' determina si el contenedor está vacío.
* El método 'estaLleno' de la clase 'Contenedor' determina si el contenedor está lleno.
* El método 'tomarTodos' de la clase 'Contenedor' devuelve todos los elementos del contenedor como una colección.
* Creamos una instancia de la clase 'Contenedor' llamada 'miContenedor' con una capacidad de 5.
* Agregamos elementos al contenedor utilizando el método 'agregar'.
* Obtenemos el número de elementos en el contenedor utilizando el método 'tamanyo'.
* Verificamos si el contenedor está lleno utilizando el método 'estaLleno'.
* Removemos un elemento del contenedor utilizando el método 'remover'.
* Recuperamos todos los elementos del contenedor como una colección utilizando el método 'tomarTodos'.
* Imprimimos los elementos en la consola.

Este código demuestra la creación de una clase 'Contenedor' con métodos para agregar, remover y obtener elementos del contenedor, así como también para determinar si el contenedor está vacío o lleno. Además, se muestra cómo utilizar la clase 'Contenedor' para crear una instancia, agregar y remover elementos, y recuperar todos los elementos como una colección.