```d
// Importamos las bibliotecas necesarias
import std.stdio, std.algorithm, std.traits;

// Definimos una clase Persona con sus propiedades
class Persona {
    private string nombre;
    private string apellidos;
    private int edad;

    // Constructor de la clase Persona
    public Persona(string nombre, string apellidos, int edad) {
        this.nombre = nombre;
        this.apellidos = apellidos;
        this.edad = edad;
    }

    // Método para obtener el nombre de la persona
    public string getNombre() {
        return nombre;
    }

    // Método para obtener los apellidos de la persona
    public string getApellidos() {
        return apellidos;
    }

    // Método para obtener la edad de la persona
    public int getEdad() {
        return edad;
    }

    // Método para imprimir la información de la persona
    public void imprimir() {
        writeln("Nombre: ", nombre);
        writeln("Apellidos: ", apellidos);
        writeln("Edad: ", edad);
    }
}

// Definimos una clase ListaPersonas que gestionará una lista de personas
class ListaPersonas {
    private Persona[] personas;
    private int numPersonas;

    // Constructor de la clase ListaPersonas
    public ListaPersonas() {
        personas = new Persona[10];
        numPersonas = 0;
    }

    // Método para añadir una persona a la lista
    public void añadirPersona(Persona persona) {
        if (numPersonas == personas.length) {
            personas = personas.resize(personas.length * 2);
        }
        personas[numPersonas++] = persona;
    }

    // Método para obtener el número de personas en la lista
    public int getNumPersonas() {
        return numPersonas;
    }

    // Método para obtener una persona de la lista por su índice
    public Persona getPersona(int índice) {
        if (índice < 0 || índice >= numPersonas) {
            throw new IndexOutOfBoundsException("Índice fuera de rango");
        }
        return personas[índice];
    }

    // Método para imprimir la información de todas las personas de la lista
    public void imprimir() {
        for (int i = 0; i < numPersonas; i++) {
            personas[i].imprimir();
        }
    }

    // Método para ordenar la lista de personas por edad en orden ascendente
    public void ordenarPorEdad() {
        personas.sort!((a, b) => a.getEdad() - b.getEdad());
    }

    // Método para buscar una persona en la lista por su nombre y apellidos
    public Persona buscarPersona(string nombre, string apellidos) {
        for (int i = 0; i < numPersonas; i++) {
            if (personas[i].getNombre() == nombre && personas[i].getApellidos() == apellidos) {
                return personas[i];
            }
        }
        return null;
    }
}

// Función principal del programa
void main() {
    // Creamos una lista de personas
    ListaPersonas listaPersonas = new ListaPersonas();

    // Añadimos algunas personas a la lista
    listaPersonas.añadirPersona(new Persona("Juan", "García", 20));
    listaPersonas.añadirPersona(new Persona("María", "López", 25));
    listaPersonas.añadirPersona(new Persona("Pedro", "Sánchez", 30));

    // Imprimimos la información de todas las personas de la lista
    writeln("Lista de personas:");
    listaPersonas.imprimir();

    // Ordenamos la lista de personas por edad en orden ascendente
    listaPersonas.ordenarPorEdad();

    // Imprimimos la información de todas las personas de la lista después de ordenarlas
    writeln("Lista de personas ordenadas por edad:");
    listaPersonas.imprimir();

    // Buscamos a una persona en la lista por su nombre y apellidos
    Persona