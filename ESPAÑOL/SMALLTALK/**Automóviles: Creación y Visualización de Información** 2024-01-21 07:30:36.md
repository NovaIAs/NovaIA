```smalltalk
# Clase 'Coche'
Coche subclass: Coche {

    # Variables de instancia
    marca: ''
    modelo: ''
    anno: 0
    color: ''
    precio: 0.0

    # Métodos de acceso
    [[:selector | selector endsWith: ':'] each] do: [:selector |
        selector := selector copy.
        selector := selector replaceAll: ':' with: '_'.
        self addClassSelector: selector method:
            [:aReceiver |
                ^(aReceiver variableValue: selector)
            ]
    ].

    [[:selector | selector endsWith: '_:'] each] do: [:selector |
        selector := selector copy.
        selector := selector replaceAll: '_:' with: ':'.
        self addClassSelector: selector method:
            [:aReceiver :value |
                aReceiver setVariableValue: selector to: value
            ]
    ].

    # Métodos de instancia
    constructor: [marca modelo anno color precio] {
        self marca: marca.
        self modelo: modelo.
        self anno: anno.
        self color: color.
        self precio: precio
    }.

    precio: [unaCantidad] {
        self precio := unaCantidad.
        ^self
    }.

    toString {
        ^String streamContents: [:unStream |
            unStream nextPutAll: self marca; nextPutAll: ' ';
            nextPutAll: self modelo; nextPutAll: ' (';
            nextPutAll: self anno asString; nextPutAll: ') ';
            nextPutAll: self color; nextPutAll: ' - $';
            nextPutAll: self precio asString
        ]
    }.

}

# Clase 'Main'
Main subclass: Main {

    # Métodos de clase
    run {
        objs := OrderedCollection new.

        objs add: (Coche new constructor: 'Ford' 'Mustang' 2023 'Azul' 35000.0).
        objs add: (Coche new constructor: 'Toyota' 'Camry' 2022 'Rojo' 25000.0).
        objs add: (Coche new constructor: 'Honda' 'Civic' 2021 'Blanco' 20000.0).

        objs do: [:unCoche |
            Transcript cr; show: (unCoche toString)
        ].

        objs at: 1 precio: 27000.0.
    }.

}

# Ejecución del programa
Main run.
```

Explicación del código:

1. **Clase 'Coche'**:

    * Esta clase representa un coche con variables de instancia para la marca, el modelo, el año, el color y el precio.
    * Los métodos de acceso se crean dinámicamente para cada variable de instancia.
    * El constructor inicializa las variables de instancia con los valores proporcionados.
    * Los métodos de instancia `precio:` y `toString` permiten establecer y obtener el precio del coche y devolver una representación de texto del mismo.

2. **Clase 'Main'**:

    * Esta clase contiene el método de clase `run` que se utiliza para ejecutar el programa.
    * El método `run` crea una colección ordenada de coches utilizando la clase `Coche`.
    * El método `do:` se utiliza para iterar sobre la colección y mostrar la información de cada coche en la consola.
    * El método `at:` se utiliza para obtener el primer coche de la colección y modificar su precio.

3. **Ejecución del programa**:

    * El método `Main run` se invoca para ejecutar el programa.
    * El programa crea una colección de coches y muestra la información de cada coche en la consola.
    * El precio del primer coche se modifica y se muestra la nueva información del coche.