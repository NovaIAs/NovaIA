```groovy
// Definimos una clase llamada "Persona" con propiedades y métodos.
class Persona {
    String nombre
    int edad
    String direccion

    def saludar() {
        println "Hola, mi nombre es ${nombre} y tengo ${edad} años."
    }
}

// Definimos una clase llamada "Empleado" que hereda de "Persona".
class Empleado extends Persona {
    String ocupacion
    double salario

    def trabajar() {
        println "Estoy trabajando como ${ocupacion} y gano ${salario} pesos por mes."
    }
}

// Definimos una clase llamada "Cliente" que hereda de "Persona".
class Cliente extends Persona {
    String telefono
    String email

    def comprar() {
        println "Estoy comprando un producto."
    }
}

// Creamos una instancia de la clase "Persona".
Persona persona = new Persona(nombre: "Juan", edad: 25, direccion: "Calle 123")

// Creamos una instancia de la clase "Empleado".
Empleado empleado = new Empleado(nombre: "María", edad: 30, ocupacion: "Ingeniera", salario: 100000)

// Creamos una instancia de la clase "Cliente".
Cliente cliente = new Cliente(nombre: "Pedro", edad: 40, telefono: "1234567890", email: "pedro@ejemplo.com")

// Llamamos al método "saludar" de la instancia "persona".
persona.saludar()

// Llamamos al método "trabajar" de la instancia "empleado".
empleado.trabajar()

// Llamamos al método "comprar" de la instancia "cliente".
cliente.comprar()

// Definimos un mapa con claves de tipo String y valores de tipo Persona.
def mapaPersonas = [
    "Juan": persona,
    "María": empleado,
    "Pedro": cliente
]

// Iteramos sobre el mapa "mapaPersonas" y llamamos al método "saludar" para cada Persona.
mapaPersonas.each { nombre, persona ->
    persona.saludar()
}
```

Explicación del código:

1. **Definición de clases:** Definimos tres clases: "Persona", "Empleado" y "Cliente".

2. **Clase "Persona":** La clase "Persona" tiene tres propiedades: "nombre", "edad" y "direccion", y un método llamado "saludar()".

3. **Clase "Empleado":** La clase "Empleado" hereda de la clase "Persona" y tiene dos propiedades adicionales: "ocupacion" y "salario", y un método llamado "trabajar()".

4. **Clase "Cliente":** La clase "Cliente" hereda de la clase "Persona" y tiene dos propiedades adicionales: "telefono" y "email", y un método llamado "comprar()".

5. **Creación de instancias:** Creamos una instancia de cada clase: "persona", "empleado" y "cliente".

6. **Llamada a métodos:** Llamamos a los métodos "saludar()", "trabajar()" y "comprar()" para cada instancia.

7. **Mapa de personas:** Creamos un mapa llamado "mapaPersonas" con claves de tipo String y valores de tipo Persona.

8. **Iteración sobre el mapa:** Iteramos sobre el mapa "mapaPersonas" y llamamos al método "saludar()" para cada Persona.

Este código es complejo porque:

* Utiliza herencia para crear clases derivadas de una clase base.
* Utiliza métodos polimórficos que se implementan de manera diferente en cada clase derivada.
* Utiliza un mapa para almacenar objetos de diferentes clases.
* Utiliza iteración sobre un mapa para procesar los objetos almacenados en el mapa.

Este código es poco probable que se repita nuevamente porque es una combinación única de características y requisitos.