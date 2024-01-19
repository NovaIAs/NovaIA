```
Componente Sistema {
  Clase Usuario {
    Atributos:
      nombre: String
      email: String
      rol: String
    Métodos:
      iniciarSesión(contraseña: String): boolean
      cerrarSesión()
      getDatos()
  }

  Clase Negocio {
    Atributos:
      nombre: String
      ubicación: String
      horarioDeAtención: String
    Métodos:
      reservarMesa(fecha, hora): boolean
      cancelarReserva(códigoReserva): boolean
      getDisponibilidad(fecha, hora): boolean
  }

  Clase Plato {
    Atributos:
      nombre: String
      precio: double
      ingredientes: List<String>
    Métodos:
      getPrecio()
      getIngredientes()
  }

  Clase Pedido {
    Atributos:
      platos: List<Plato>
      precioTotal: double
      estado: String
    Métodos:
      agregarPlato(plato): void
      quitarPlato(plato): void
      getPrecioTotal()
      getEstado()
  }

  Clase Cocina {
    Atributos:
      pedidos: List<Pedido>
      cocineros: List<Cocinero>
    Métodos:
      recibirPedido(pedido): void
      cocinarPedido(pedido): void
      getPedidos()
      getCocineros()
  }

  Clase Cocinero {
    Atributos:
      nombre: String
      especialidad: String
    Métodos:
      cocinarPlato(plato): void
      getEspecialidad()
  }

  Clase Camarero {
    Atributos:
      nombre: String
      secciones: List<String>
    Métodos:
      tomarPedido(mesa): void
      entregarPedido(mesa): void
      getSecciones()
  }
}

```

Explicación:

* El componente Sistema es el componente principal del sistema. Contiene todas las clases del sistema.
* La clase Usuario representa a los usuarios del sistema.
* La clase Negocio representa a los negocios que ofrecen servicios de restauración.
* La clase Plato representa los platos que se pueden pedir en los negocios.
* La clase Pedido representa los pedidos que se realizan en los negocios.
* La clase Cocina representa la cocina de los negocios.
* La clase Cocinero representa a los cocineros que trabajan en la cocina.
* La clase Camarero representa a los camareros que trabajan en los negocios.