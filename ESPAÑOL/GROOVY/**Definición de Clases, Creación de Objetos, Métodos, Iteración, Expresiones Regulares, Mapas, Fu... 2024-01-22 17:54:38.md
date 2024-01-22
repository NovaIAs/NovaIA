**Definición de clases**

```groovy
class Persona {
    String nombre
    String apellido
    int edad

    Persona(String nombre, String apellido, int edad) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

    String getNombreCompleto() {
        return "${nombre} ${apellido}"
    }
}

class Empleado extends Persona {
    String puesto
    double salario

    Empleado(String nombre, String apellido, int edad, String puesto, double salario) : super(nombre, apellido, edad) {
        this.puesto = puesto
        this.salario = salario
    }

    @Override
    String getNombreCompleto() {
        return super.getNombreCompleto() + " [Empleado]"
    }
}

class Cliente extends Persona {
    String rfc
    String telefono

    Cliente(String nombre, String apellido, int edad, String rfc, String telefono) : super(nombre, apellido, edad) {
        this.rfc = rfc
        this.telefono = telefono
    }

    @Override
    String getNombreCompleto() {
        return super.getNombreCompleto() + " [Cliente]"
    }
}
```

**Creación de objetos**

```groovy
def empleado1 = new Empleado("Juan", "García", 30, "Programador", 15000.0)
def empleado2 = new Empleado("María", "Pérez", 25, "Contadora", 12000.0)
def cliente1 = new Cliente("Pedro", "López", 40, "LOPE800101PDL", "555-123-4567")
def cliente2 = new Cliente("Ana", "Fernández", 35, "FERN900203FVA", "555-234-5678")
```

**Uso de métodos**

```groovy
println empleado1.getNombreCompleto() // Juan García [Empleado]
println empleado2.getNombreCompleto() // María Pérez [Empleado]
println cliente1.getNombreCompleto() // Pedro López [Cliente]
println cliente2.getNombreCompleto() // Ana Fernández [Cliente]
```

**Bucle sobre una lista**

```groovy
def personas = [empleado1, empleado2, cliente1, cliente2]

personas.each { persona ->
    println persona.getNombreCompleto()
}
```

**Uso de una expresión regular**

```groovy
def regex = /([a-zA-Z]+) ([a-zA-Z]+)/
def nombreCompleto = "Juan García"

if (nombreCompleto =~ regex) {
    def nombre = regex[0][1]
    def apellido = regex[0][2]

    println "Nombre: ${nombre}"
    println "Apellido: ${apellido}"
}
```

**Uso de un mapa**

```groovy
def mapa = [:]

mapa["Juan García"] = empleado1
mapa["María Pérez"] = empleado2
mapa["Pedro López"] = cliente1
mapa["Ana Fernández"] = cliente2

def persona = mapa["Juan García"]

println persona.getNombreCompleto() // Juan García [Empleado]
```

**Uso de una función**

```groovy
def sumar(a, b) {
    return a + b
}

def resultado = sumar(10, 20)

println resultado // 30
```

**Uso de una clase anónima**

```groovy
def persona = new Persona() {
    String nombre = "Juan"
    String apellido = "García"
    int edad = 30

    @Override
    String getNombreCompleto() {
        return "${nombre} ${apellido}"
    }
}

println persona.getNombreCompleto() // Juan García
```