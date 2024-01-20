```rust
// Declaración de estructuras
struct Persona {
    nombre: String,
    edad: u8,
    ocupación: String,
}

struct Empresa {
    nombre: String,
    dirección: String,
    empleados: Vec<Persona>,
}

// Función para crear una persona
fn crear_persona(nombre: &str, edad: u8, ocupación: &str) -> Persona {
    Persona {
        nombre: String::from(nombre),
        edad,
        ocupación: String::from(ocupación),
    }
}

// Función para crear una empresa
fn crear_empresa(nombre: &str, dirección: &str, empleados: Vec<Persona>) -> Empresa {
    Empresa {
        nombre: String::from(nombre),
        dirección: String::from(dirección),
        empleados,
    }
}

// Función principal
fn main() {
    // Creación de personas
    let juan = crear_persona("Juan", 25, "Estudiante");
    let maría = crear_persona("María", 30, "Profesora");

    // Creación de una empresa
    let empresa_a = crear_empresa("Empresa A", "Calle 123", vec![juan, maría]);

    // Impresión de la información de la empresa
    println!("Nombre de la empresa: {}", empresa_a.nombre);
    println!("Dirección de la empresa: {}", empresa_a.dirección);
    println!("Empleados de la empresa:");
    for empleado in &empresa_a.empleados {
        println!(" - {} ({}, {})", empleado.nombre, empleado.edad, empleado.ocupación);
    }
}
```

Explicación:

* Se definen dos estructuras, `Persona` y `Empresa`, para representar personas y empresas, respectivamente.
* Se definen dos funciones, `crear_persona` y `crear_empresa`, para crear instancias de estas estructuras.
* En la función `main`, se crean dos personas, Juan y María, y una empresa, Empresa A.
* Se imprimen la información de la empresa y sus empleados.