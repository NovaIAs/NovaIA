```
// Programa de biblioteca para gestionar un inventario

// Definición de la clase Articulo

class Artículo {
  // Atributos de los artículos
  nombre : String;
  precio : Float;
  cantidad : Int;

  // Constructor de la clase
  new(nombre: String, precio: Float, cantidad: Int) {
    self.nombre = nombre;
    self.precio = precio;
    self.cantidad = cantidad;
  }

  // Método para obtener el nombre del artículo
  getNombre(): String {
    return self.nombre;
  }

  // Método para obtener el precio del artículo
  getPrecio(): Float {
    return self.precio;
  }

  // Método para obtener la cantidad del artículo
  getCantidad(): Int {
    return self.cantidad;
  }

  // Método para establecer el nombre del artículo
  setNombre(nombre: String) {
    self.nombre = nombre;
  }

  // Método para establecer el precio del artículo
  setPrecio(precio: Float) {
    self.precio = precio;
  }

  // Método para establecer la cantidad del artículo
  setCantidad(cantidad: Int) {
    self.cantidad = cantidad;
  }

  // Método para imprimir la información del artículo
  imprimir() {
    print("Nombre: "+self.nombre);
    print("Precio: "+self.precio);
    print("Cantidad: "+self.cantidad);
  }
}

// Definición de la clase Inventario

class Inventario {
  // Atributos del inventario
  articulos : Array<Articulo>;

  // Constructor de la clase
  new() {
    self.articulos = new Array<Articulo>();
  }

  // Método para agregar un artículo al inventario
  agregarArticulo(articulo: Articulo) {
    self.articulos.add(articulo);
  }

  // Método para eliminar un artículo del inventario
  eliminarArticulo(articulo: Articulo) {
    self.articulos.remove(articulo);
  }

  // Método para obtener un artículo del inventario
  obtenerArticulo(nombre: String): Articulo? {
    for (articulo in self.articulos) {
      if (articulo.getNombre() == nombre) {
        return articulo;
      }
    }
    return null;
  }

  // Método para imprimir el inventario
  imprimir() {
    print("Inventario:");
    for (articulo in self.articulos) {
      articulo.imprimir();
    }
  }
}

// Programa principal

main() {
  // Crear un inventario
  inventario = new Inventario();

  // Agregar algunos artículos al inventario
  inventario.agregarArticulo(new Articulo("Manzanas", 1.50, 10));
  inventario.agregarArticulo(new Articulo("Peras", 2.00, 15));
  inventario.agregarArticulo(new Articulo("Uvas", 3.00, 20));

  // Imprimir el inventario
  inventario.imprimir();

  // Obtener un artículo del inventario
  articulo = inventario.obtenerArticulo("Peras");

  // Imprimir el artículo obtenido
  print("Artículo obtenido:");
  articulo.imprimir();

  // Eliminar el artículo del inventario
  inventario.eliminarArticulo(articulo);

  // Imprimir el inventario después de eliminar el artículo
  inventario.imprimir();
}
```