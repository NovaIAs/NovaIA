```cool
clase ClasePrincipal {
    -- Atributos
    variable padre: Padre;
    variable madre: Madre;
    variable hijos: Lista[Hijo];

    -- Constructor
    crear(padre: Padre, madre: Madre) {
        auto clase_principal = nuevo ClasePrincipal;
        clase_principal.padre = padre;
        clase_principal.madre = madre;
        clase_principal.hijos = nueva Lista[Hijo];
        devolver clase_principal;
    }

    -- Métodos
    método añadirHijo(hijo: Hijo) {
        hijos.añadir(hijo);
    }

    método obtenerHijos() {
        devolver hijos;
    }

    método obtenerPadre() {
        devolver padre;
    }

    método obtenerMadre() {
        devolver madre;
    }

    método imprimirFamilia() {
        escribir("Padre: ");
        padre.imprimirNombre();
        escribir("Madre: ");
        madre.imprimirNombre();
        escribir("Hijos: ");
        para (i en 0 hasta hijos.largo()) {
            hijos[i].imprimirNombre();
        }
    }
}

clase Persona {
    -- Atributos
    variable nombre: String;

    -- Constructor
    crear(nombre: String) {
        auto persona = nuevo Persona;
        persona.nombre = nombre;
        devolver persona;
    }

    -- Métodos
    método imprimirNombre() {
        escribir(nombre);
    }
}

clase Padre: Persona {
    -- Atributos
   
    -- Constructor
    crear(nombre: String) {
        auto padre = nuevo Padre;
        padre.nombre = nombre;
        devolver padre;
    }
}

clase Madre: Persona {
    -- Atributos
   
    -- Constructor
    crear(nombre: String) {
        auto madre = nuevo Madre;
        madre.nombre = nombre;
        devolver madre;
    }
}

clase Hijo: Persona {
    -- Atributos
   
    -- Constructor
    crear(nombre: String) {
        auto hijo = nuevo Hijo;
        hijo.nombre = nombre;
        devolver hijo;
    }
}

clase Lista[T] {
    -- Atributos
    variable elementos: Array[T];
    variable largo: Int;

    -- Constructor
    crear() {
        auto lista = nuevo Lista[T];
        lista.elementos = nuevo Array[T];
        lista.largo = 0;
        devolver lista;
    }

    -- Métodos
    método añadir(elemento: T) {
        elementos[largo] = elemento;
        largo++;
    }

    método obtener(índice: Int) {
        devolver elementos[índice];
    }

    método largo() {
        devolver largo;
    }
}

-- Función principal
función principal() {
    auto padre = Padre.crear("Juan");
    auto madre = Madre.crear("María");

    auto hijo1 = Hijo.crear("Pedro");
    auto hijo2 = Hijo.crear("Ana");
    auto hijo3 = Hijo.crear("Carlos");

    auto clase_principal = ClasePrincipal.crear(padre, madre);
    clase_principal.añadirHijo(hijo1);
    clase_principal.añadirHijo(hijo2);
    clase_principal.añadirHijo(hijo3);

    clase_principal.imprimirFamilia();
}
```

Explicación del código:

* La clase `ClasePrincipal` representa una familia, con un padre, una madre y una lista de hijos.
* La clase `Persona` es una clase base para las clases `Padre`, `Madre` e `Hijo`.
* La clase `Padre` hereda de la clase `Persona`.
* La clase `Madre` hereda de la clase `Persona`.
* La clase `Hijo` hereda de la clase `Persona`.
* La clase `Lista[T]` representa una lista de elementos de tipo `T`.
* La función `principal` crea una familia, añadiendo un padre, una madre y tres hijos a la lista de hijos.
* La función `imprimirFamilia` imprime los nombres del padre, la madre y los hijos.

Este código es un ejemplo de cómo utilizar la herencia y la composición en COOL para crear un programa complejo.