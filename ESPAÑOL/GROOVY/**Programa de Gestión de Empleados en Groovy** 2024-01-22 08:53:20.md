```groovy
// Clase Persona con propiedades y métodos
class Persona {
    String nombre
    String apellido
    int edad

    String getNombreCompleto() {
        return "$nombre $apellido"
    }

    int getEdad() {
        return edad
    }

    void setEdad(int nuevaEdad) {
        edad = nuevaEdad
    }
}

// Clase Empleado que hereda de Persona y agrega propiedades y métodos específicos
class Empleado extends Persona {
    String cargo
    double salario

    String getCargo() {
        return cargo
    }

    void setCargo(String nuevoCargo) {
        cargo = nuevoCargo
    }

    double getSalario() {
        return salario
    }

    void setSalario(double nuevoSalario) {
        salario = nuevoSalario
    }
}

// Clase Empresa que contiene una lista de empleados y métodos para gestionarla
class Empresa {
    List<Empleado> empleados = []

    void agregarEmpleado(Empleado empleado) {
        empleados.add(empleado)
    }

    List<Empleado> getEmpleados() {
        return empleados
    }

    double getSalarioTotal() {
        double salarioTotal = 0
        for (Empleado empleado : empleados) {
            salarioTotal += empleado.getSalario()
        }
        return salarioTotal
    }
}

// Clase Principal que crea una empresa, agrega empleados y muestra información
class Principal {
    static void main(String[] args) {
        Empresa empresa = new Empresa()

        // Crear y agregar empleados
        Empleado juan = new Empleado()
        juan.nombre = "Juan"
        juan.apellido = "Pérez"
        juan.edad = 30
        juan.cargo = "Gerente"
        juan.salario = 2000.0

        Empleado maria = new Empleado()
        maria.nombre = "María"
        maria.apellido = "García"
        maria.edad = 25
        maria.cargo = "Secretaria"
        maria.salario = 1500.0

        Empleado pedro = new Empleado()
        pedro.nombre = "Pedro"
        pedro.apellido = "López"
        pedro.edad = 40
        pedro.cargo = "Contador"
        pedro.salario = 2500.0

        empresa.agregarEmpleado(juan)
        empresa.agregarEmpleado(maria)
        empresa.agregarEmpleado(pedro)

        // Mostrar información de la empresa y sus empleados
        println "Empresa: ${empresa.getEmpleados().first().empresa}"
        println "Empleados:"
        for (Empleado empleado : empresa.getEmpleados()) {
            println "    ${empleado.getNombreCompleto()} - Edad: ${empleado.getEdad()} - Cargo: ${empleado.getCargo()} - Salario: ${empleado.getSalario()}"
        }

        println "Salario total de la empresa: ${empresa.getSalarioTotal()}"
    }
}
```

Explicación del código:

* La clase `Persona` define las propiedades y métodos comunes a todas las personas, como nombre, apellido y edad.
* La clase `Empleado` hereda de la clase `Persona` y agrega propiedades y métodos específicos para los empleados, como cargo y salario.
* La clase `Empresa` contiene una lista de empleados y métodos para gestionarla, como agregar empleados, obtener la lista de empleados y calcular el salario total de la empresa.
* La clase `Principal` crea una instancia de la clase `Empresa`, agrega empleados a la empresa y muestra información sobre la empresa y sus empleados.

El código utiliza algunas características avanzadas de Groovy, como la sintaxis de cierre `{...}` y la interpolación de cadenas de texto `${...}`, que hacen que el código sea más conciso y legible.