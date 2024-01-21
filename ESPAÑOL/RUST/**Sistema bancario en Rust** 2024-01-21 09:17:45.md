```rust
// Importamos las librerías necesarias.
use std::io;
use std::collections::HashMap;
use std::fmt;

// Definimos una estructura para representar un cliente.
#[derive(Debug)]
struct Cliente {
    nombre: String,
    apellido: String,
    cedula: i32,
    saldo: f64,
}

// Definimos una estructura para representar un banco.
#[derive(Debug)]
struct Banco {
    nombre: String,
    clientes: HashMap<i32, Cliente>,
}

// Implementamos el método `depositar()` para la estructura `Cliente`.
impl Cliente {
    fn depositar(&mut self, cantidad: f64) {
        self.saldo += cantidad;
    }
}

// Implementamos el método `retirar()` para la estructura `Cliente`.
impl Cliente {
    fn retirar(&mut self, cantidad: f64) {
        if cantidad <= self.saldo {
            self.saldo -= cantidad;
        } else {
            println!("Saldo insuficiente");
        }
    }
}

// Implementamos el método `transferir()` para la estructura `Cliente`.
impl Cliente {
    fn transferir(&mut self, cantidad: f64, cliente: &mut Cliente) {
        if cantidad <= self.saldo {
            self.saldo -= cantidad;
            cliente.saldo += cantidad;
        } else {
            println!("Saldo insuficiente");
        }
    }
}

// Implementamos el método `crear_cliente()` para la estructura `Banco`.
impl Banco {
    fn crear_cliente(&mut self, nombre: String, apellido: String, cedula: i32, saldo: f64) {
        let cliente = Cliente {
            nombre,
            apellido,
            cedula,
            saldo,
        };
        self.clientes.insert(cedula, cliente);
    }
}

// Implementamos el método `obtener_cliente()` para la estructura `Banco`.
impl Banco {
    fn obtener_cliente(&self, cedula: i32) -> Option<&Cliente> {
        self.clientes.get(&cedula)
    }
}

// Implementamos el método `depositar()` para la estructura `Banco`.
impl Banco {
    fn depositar(&mut self, cedula: i32, cantidad: f64) {
        if let Some(cliente) = self.obtener_cliente(cedula) {
            cliente.depositar(cantidad);
        } else {
            println!("Cliente no encontrado");
        }
    }
}

// Implementamos el método `retirar()` para la estructura `Banco`.
impl Banco {
    fn retirar(&mut self, cedula: i32, cantidad: f64) {
        if let Some(cliente) = self.obtener_cliente(cedula) {
            cliente.retirar(cantidad);
        } else {
            println!("Cliente no encontrado");
        }
    }
}

// Implementamos el método `transferir()` para la estructura `Banco`.
impl Banco {
    fn transferir(&mut self, cedula_origen: i32, cedula_destino: i32, cantidad: f64) {
        if let Some(cliente_origen) = self.obtener_cliente(cedula_origen) {
            if let Some(cliente_destino) = self.obtener_cliente(cedula_destino) {
                cliente_origen.transferir(cantidad, cliente_destino);
            } else {
                println!("Cliente destino no encontrado");
            }
        } else {
            println!("Cliente origen no encontrado");
        }
    }
}

// Implementamos el método `mostrar_clientes()` para la estructura `Banco`.
impl Banco {
    fn mostrar_clientes(&self) {
        for cliente in self.clientes.values() {
            println!("{:?}", cliente);
        }
    }
}

// Definimos la función `main()`.
fn main() {
    // Creamos un nuevo banco.
    let mut banco = Banco {
        nombre: "Banco Central".to_string(),
        clientes: HashMap::new(),
    };

    // Creamos algunos clientes.
    banco.crear_cliente("Juan".to_string(), "Pérez".to_string(), 123456789, 1000.0);
    banco.crear_cliente("María".to_string(), "López".to_string(), 987654321, 2000.0);
    banco.crear_cliente("Pedro".to_string(), "García".to_string(), 111222333, 3000.0);

    // Mostramos los clientes.
    banco.mostrar_clientes();

    // Depositamos dinero en la cuenta de Juan.
    banco.depositar(123456789, 500.0);

    // Retiramos dinero de la cuenta de María.
    banco.retirar(987654321, 1000.0);

    // Transferimos dinero de la cuenta de Pedro a la cuenta de Juan.
    banco.transferir(111222333, 123456789, 500.0);

    // Mostramos los clientes nuevamente.
    banco.mostrar_clientes();
}
```

Este código crea un banco con tres clientes y realiza varias operaciones bancarias, como depósitos, retiros y transferencias. El código está bien documentado y es fácil de entender.