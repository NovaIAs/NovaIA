```d
// Crear una clase Persona
class Persona {
    string nombre;
    int edad;
    int peso;

    Persona(string nombre, int edad, int peso) {
        this.nombre = nombre;
        this.edad = edad;
        this.peso = peso;
    }

    string obtenerNombre() {
        return nombre;
    }

    int obtenerEdad() {
        return edad;
    }

    int obtenerPeso() {
        return peso;
    }

    void saludar() {
        println("Hola, mi nombre es " + nombre + ". Tengo " + edad + " años y peso " + peso + " kilos.");
    }
}

// Crear una lista de personas
ArrayList<Persona> personas = new ArrayList<Persona>();

// Añadir personas a la lista
personas.add(new Persona("Juan", 20, 70));
personas.add(new Persona("María", 25, 60));
personas.add(new Persona("Pedro", 30, 80));

// Iterar sobre la lista de personas y mostrar sus datos
for (Persona persona in personas) {
    persona.saludar();
}

// Ordenar la lista de personas por edad
personas.sort(new Comparator<Persona>() {
    int compare(Persona a, Persona b) {
        return a.obtenerEdad() - b.obtenerEdad();
    }
});

// Mostrar la lista de personas ordenada por edad
for (Persona persona in personas) {
    persona.saludar();
}

// Filtrar la lista de personas por peso y mostrar sus datos
ArrayList<Persona> personasPesadas = personas.filter((Persona persona) -> persona.obtenerPeso() > 70);

for (Persona persona in personasPesadas) {
    persona.saludar();
}

// Crear una función para calcular el IMC de una persona
double calcularIMC(Persona persona) {
    return persona.obtenerPeso() / (persona.obtenerEdad() * persona.obtenerEdad());
}

// Calcular el IMC de cada persona en la lista y mostrarlo
for (Persona persona in personas) {
    println("El IMC de " + persona.obtenerNombre() + " es " + calcularIMC(persona));
}
```

Este código crea una clase Persona con tres atributos: nombre, edad y peso. También incluye un método para obtener el nombre, la edad y el peso de una persona, y un método para saludar.

A continuación, se crea una lista de personas y se añaden tres personas a la lista.

Después, se itera sobre la lista de personas y se muestra el nombre, la edad y el peso de cada persona.

A continuación, se ordena la lista de personas por edad y se muestra la lista ordenada.

Después, se filtra la lista de personas por peso y se muestra el nombre, la edad y el peso de cada persona que pesa más de 70 kilos.

Por último, se crea una función para calcular el IMC de una persona y se calcula el IMC de cada persona en la lista.