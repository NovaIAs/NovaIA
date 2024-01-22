```cool
clase Día {
    entero día;
    entero mes;
    entero año;

    constructor (entero día, entero mes, entero año) {
        self.día = día;
        self.mes = mes;
        self.año = año;
    }

    str to_string() {
        return self.día + "/" + self.mes + "/" + self.año;
    }
}

clase Persona {
    str nombre;
    Día fecha_nacimiento;

    constructor (str nombre, Día fecha_nacimiento) {
        self.nombre = nombre;
        self.fecha_nacimiento = fecha_nacimiento;
    }

    str to_string() {
        return "Nombre: " + self.nombre + "\nFecha de nacimiento: " + self.fecha_nacimiento.to_string();
    }
}

clase ListaPersonas {
    Persona[] personas;
    entero tamaño;

    constructor () {
        self.personas = [];
        self.tamaño = 0;
    }

    void insertar (Persona persona) {
        self.personas[self.tamaño] = persona;
        self.tamaño++;
    }

    Persona[] obtener_personas() {
        return self.personas;
    }

    entero obtener_tamaño() {
        return self.tamaño;
    }
}

clase Principal {
    public static void main() {
        ListaPersonas lista_personas = new ListaPersonas();

        lista_personas.insertar(new Persona("Juan", new Día(1, 1, 2000)));
        lista_personas.insertar(new Persona("María", new Día(2, 2, 2001)));
        lista_personas.insertar(new Persona("Pedro", new Día(3, 3, 2002)));

        Persona[] personas = lista_personas.obtener_personas();
        entero tamaño = lista_personas.obtener_tamaño();

        for (entero i = 0; i < tamaño; i++) {
            println(personas[i].to_string());
        }
    }
}
```

Explicación del código:

* Se definen las clases `Día`, `Persona`, `ListaPersonas` y `Principal`.
* La clase `Día` representa un día con su día, mes y año.
* La clase `Persona` representa a una persona con su nombre y fecha de nacimiento.
* La clase `ListaPersonas` representa una lista de personas.
* La clase `Principal` contiene el método `main` que se ejecuta cuando se ejecuta el programa.
* En el método `main` se crea una instancia de la clase `ListaPersonas` y se le añaden tres personas.
* A continuación, se obtienen las personas de la lista y se imprimen sus datos.