```smalltalk

;; Declaración de las clases

class Perro

    ;; Atributos
    name: "Rex"
    age: 3

    ;; Constructor
    init: name anAge

        self name: name.
        self age: anAge.

    ;; Métodos
    ladrar:

        "El perro ladra."

        Transcript show: 'Guau!'.

    correr:

        "El perro corre."

        Transcript show: 'Estoy corriendo!'.

    comer:

        "El perro come."

        Transcript show: 'Estoy comiendo!'.

class Gato

    ;; Atributos
    name: "Misifu"
    age: 2

    ;; Constructor
    init: name anAge

        self name: name.
        self age: anAge.

    ;; Métodos
    maullar:

        "El gato maulla."

        Transcript show: 'Miau!'.

    saltar:

        "El gato salta."

        Transcript show: 'Estoy saltando!'.

    dormir:

        "El gato duerme."

        Transcript show: 'Estoy durmiendo!'.

class Mascota

    ;; Atributos
    name: ""
    age: 0

    ;; Constructor
    init: name anAge

        self name: name.
        self age: anAge.

    ;; Métodos
    comer:

        "La mascota come."

        Transcript show: 'Estoy comiendo!'.

    dormir:

        "La mascota duerme."

        Transcript show: 'Estoy durmiendo!'.

;; Creación de los objetos

perro := Perro new: 'Rex' with: 3.
gato := Gato new: 'Misifu' with: 2.

;; Llamada a los métodos

perro ladrar.
gato maullar.
perro correr.
gato saltar.
perro comer.
gato dormir.

```

Explicación del código:

* Declaración de las clases: Se declaran las clases `Perro`, `Gato` y `Mascota`. Cada clase tiene sus propios atributos y métodos.
* Constructor: Cada clase tiene un constructor que se utiliza para crear objetos de esa clase. El constructor toma dos argumentos: el nombre y la edad de la mascota.
* Métodos: Cada clase tiene varios métodos que se pueden llamar para realizar diferentes acciones. Por ejemplo, el perro puede ladrar, correr y comer. El gato puede maullar, saltar y dormir. Y la mascota puede comer y dormir.
* Creación de los objetos: Se crean dos objetos utilizando los constructores de las clases `Perro` y `Gato`. El objeto `perro` se llama `Rex` y tiene 3 años. El objeto `gato` se llama `Misifu` y tiene 2 años.
* Llamada a los métodos: Se llaman a los métodos de los objetos para realizar diferentes acciones. Por ejemplo, se llama al método `ladrar` del objeto `perro` para que el perro ladre. Se llama al método `maullar` del objeto `gato` para que el gato maulle. Y así sucesivamente.