```cool
clase Persona {
    nombre: String;
    edad: Int;
    profesion: String;

    constructor(nombre: String, edad: Int, profesion: String) {
        this.nombre = nombre;
        this.edad = edad;
        this.profesion = profesion;
    }

    procedimiento saludar() {
        print("Hola, mi nombre es " + this.nombre + ". Tengo " + this.edad + " años y soy " + this.profesion + ".");
    }
}

clase Empleado extends Persona {
    salario: Int;

    constructor(nombre: String, edad: Int, profesion: String, salario: Int) : super(nombre, edad, profesion) {
        this.salario = salario;
    }

    procedimiento trabajar() {
        print("Estoy trabajando.");
    }

    procedimiento cobrarSalario() {
        print("Estoy cobrando mi salario: " + this.salario + " euros.");
    }
}

clase Cliente extends Persona {
    numeroCliente: Int;

    constructor(nombre: String, edad: Int, profesion: String, numeroCliente: Int) : super(nombre, edad, profesion) {
        this.numeroCliente = numeroCliente;
    }

    procedimiento comprar() {
        print("Estoy comprando.");
    }

    procedimiento pagar() {
        print("Estoy pagando.");
    }
}

clase Empresa {
    nombre: String;
    direccion: String;
    telefono: String;

    constructor(nombre: String, direccion: String, telefono: String) {
        this.nombre = nombre;
        this.direccion = direccion;
        this.telefono = telefono;
    }

    procedimiento contratarEmpleado(empleado: Empleado) {
        print("Se ha contratado a " + empleado.nombre + " como " + empleado.profesion + ".");
    }

    procedimiento despedirEmpleado(empleado: Empleado) {
        print("Se ha despedido a " + empleado.nombre + " de su puesto de " + empleado.profesion + ".");
    }
}

clase Banco {
    nombre: String;
    direccion: String;
    telefono: String;

    constructor(nombre: String, direccion: String, telefono: String) {
        this.nombre = nombre;
        this.direccion = direccion;
        this.telefono = telefono;
    }

    procedimiento abrirCuenta(cliente: Cliente) {
        print("Se ha abierto una cuenta bancaria para " + cliente.nombre + ".");
    }

    procedimiento cerrarCuenta(cliente: Cliente) {
        print("Se ha cerrado la cuenta bancaria de " + cliente.nombre + ".");
    }
}

clase Main {
    procedimiento iniciarMain() {
        // Crea un objeto de la clase Persona
        persona1 := new Persona("Juan", 25, "Profesor");

        // Muestra el nombre de la persona
        print(persona1.nombre);

        // Crea un objeto de la clase Empleado
        empleado1 := new Empleado("María", 30, "Ingeniera", 2000);

        // Muestra el nombre y el salario del empleado
        print(empleado1.nombre + " gana " + empleado1.salario + " euros.");

        // Crea un objeto de la clase Cliente
        cliente1 := new Cliente("Pedro", 40, "Abogado", 12345678);

        // Muestra el nombre y el número de cliente del cliente
        print(cliente1.nombre + " es el cliente número " + cliente1.numeroCliente + ".");

        // Crea un objeto de la clase Empresa
        empresa1 := new Empresa("ACME Corp.", "Calle Mayor, 123", "912345678");

        // Muestra el nombre de la empresa
        print(empresa1.nombre);

        // Crea un objeto de la clase Banco
        banco1 := new Banco("Banco Santander", "Paseo de la Castellana, 250", "912345679");

        // Muestra el nombre del banco
        print(banco1.nombre);

        // Contrata al empleado en la empresa
        empresa1.contratarEmpleado(empleado1);

        // Despide al empleado de la empresa
        empresa1.despedirEmpleado(empleado1);

        // Abre una cuenta bancaria para el cliente
        banco1.abrirCuenta(cliente1);

        // Cierra la cuenta bancaria del cliente
        banco1.cerrarCuenta(cliente1);
    }
}
```

Este código es un ejemplo de cómo escribir un programa complejo en COOL. El programa crea varios objetos de diferentes clases, incluyendo personas, empleados, clientes, empresas y bancos. Luego, el programa realiza varias operaciones con estos objetos, como mostrar sus nombres, contratar y despedir empleados, abrir y cerrar cuentas bancarias, etc.

Este código es un ejemplo de cómo COOL se puede utilizar para escribir programas complejos que involucran múltiples clases y objetos. COOL es un lenguaje de programación muy versátil que se puede utilizar para escribir una amplia variedad de programas.