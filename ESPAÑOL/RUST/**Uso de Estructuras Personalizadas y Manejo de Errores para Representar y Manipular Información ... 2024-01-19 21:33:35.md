**Uso de estructuras personalizadas y manejo de errores para representar y manipular información de empleados**

```rust
// Definición de la estructura `Empleado`
#[derive(Debug)]
struct Empleado {
    nombre: String,
    puesto: String,
    salario: f32,
}

// Implementación de métodos para la estructura `Empleado`
impl Empleado {
    // Constructor que crea un nuevo empleado a partir de su nombre, puesto y salario
    fn new(nombre: &str, puesto: &str, salario: f32) -> Result<Empleado, String> {
        // Comprobación de los valores de entrada
        if nombre.is_empty() {
            return Err("El nombre no puede estar vacío".to_string());
        }
        if puesto.is_empty() {
            return Err("El puesto no puede estar vacío".to_string());
        }
        if salario < 0.0 {
            return Err("El salario no puede ser negativo".to_string());
        }

        // Creación del empleado
        Ok(Empleado {
            nombre: nombre.to_string(),
            puesto: puesto.to_string(),
            salario: salario,
        })
    }

    // Método para obtener el nombre del empleado
    fn obtener_nombre(&self) -> &str {
        &self.nombre
    }

    // Método para obtener el puesto del empleado
    fn obtener_puesto(&self) -> &str {
        &self.puesto
    }

    // Método para obtener el salario del empleado
    fn obtener_salario(&self) -> f32 {
        self.salario
    }

    // Método para aplicar un aumento de salario al empleado
    fn aumentar_salario(&mut self, porcentaje: f32) {
        self.salario *= 1.0 + porcentaje / 100.0;
    }
}

// Función principal
fn main() {
    // Creación de algunos empleados
    let empleado1 = Empleado::new("Juan", "Gerente", 5000.0).unwrap();
    let empleado2 = Empleado::new("María", "Ingeniera", 3000.0).unwrap();
    let empleado3 = Empleado::new("Pedro", "Contador", 2000.0).unwrap();

    // Impresión de los datos de los empleados
    println!("Empleados:");
    println!("{:?}", empleado1);
    println!("{:?}", empleado2);
    println!("{:?}", empleado3);

    // Aumento de salarios
    empleado1.aumentar_salario(10.0);
    empleado2.aumentar_salario(5.0);
    empleado3.aumentar_salario(2.0);

    // Impresión de los datos de los empleados después del aumento de salarios
    println!("\nEmpleados después del aumento de salarios:");
    println!("{:?}", empleado1);
    println!("{:?}", empleado2);
    println!("{:?}", empleado3);
}
```

**Explicación del código:**

1. Se define una estructura `Empleado` que contiene los datos de un empleado, incluyendo su nombre, puesto y salario.
2. Se implementa una serie de métodos para la estructura `Empleado`, incluyendo constructores, getters y setters, y un método para aplicar un aumento de salario.
3. En la función `main`, se crean algunos empleados usando el constructor `Empleado::new`.
4. Se imprimen los datos de los empleados usando `println!`.
5. Se aplica un aumento de salario a los empleados usando el método `aumentar_salario`.
6. Se imprimen los datos de los empleados después del aumento de salarios usando `println!`.