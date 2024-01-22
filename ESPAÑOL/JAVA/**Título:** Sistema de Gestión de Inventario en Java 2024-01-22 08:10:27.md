```java
// **Importar bibliotecas necesarias:**

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// **Clase principal:**

public class SistemaGestionInventario {

    // **Atributos:**

    private List<Producto> productos;
    private Map<Producto, Integer> stock;

    // **Constructor:**

    public SistemaGestionInventario() {
        this.productos = new ArrayList<>();
        this.stock = new HashMap<>();
    }

    // **Métodos:**

    // **Agregar producto:**

    public void agregarProducto(Producto producto, int cantidad) {
        this.productos.add(producto);
        this.stock.put(producto, cantidad);
    }

    // **Eliminar producto:**

    public void eliminarProducto(Producto producto) {
        this.productos.remove(producto);
        this.stock.remove(producto);
    }

    // **Actualizar stock:**

    public void actualizarStock(Producto producto, int nuevaCantidad) {
        this.stock.put(producto, nuevaCantidad);
    }

    // **Obtener producto por nombre:**

    public Producto obtenerProductoPorNombre(String nombre) {
        for (Producto producto : this.productos) {
            if (producto.getNombre().equals(nombre)) {
                return producto;
            }
        }
        return null;
    }

    // **Obtener stock de producto:**

    public int obtenerStockDeProducto(Producto producto) {
        return this.stock.get(producto);
    }

    // **Generar informe de inventario:**

    public String generarInformeDeInventario() {
        StringBuilder informe = new StringBuilder();
        informe.append("Informe de Inventario:\n");
        for (Producto producto : this.productos) {
            informe.append(producto.getNombre()).append(" (").append(this.stock.get(producto)).append(")\n");
        }
        return informe.toString();
    }

}

// **Clase Producto:**

public class Producto {

    private int id;
    private String nombre;
    private double precio;

    // **Constructor:**

    public Producto(int id, String nombre, double precio) {
        this.id = id;
        this.nombre = nombre;
        this.precio = precio;
    }

    // **Métodos:**

    // **Obtener ID:**

    public int getId() {
        return this.id;
    }

    // **Obtener nombre:**

    public String getNombre() {
        return this.nombre;
    }

    // **Obtener precio:**

    public double getPrecio() {
        return this.precio;
    }

}

// **Uso del sistema:**

public class Main {

    public static void main(String[] args) {
        // **Crear un sistema de gestión de inventario:**

        SistemaGestionInventario inventario = new SistemaGestionInventario();

        // **Agregar algunos productos:**

        inventario.agregarProducto(new Producto(1, "Camisa", 10.0), 5);
        inventario.agregarProducto(new Producto(2, "Pantalón", 15.0), 3);
        inventario.agregarProducto(new Producto(3, "Zapatos", 20.0), 2);

        // **Actualizar el stock de un producto:**

        inventario.actualizarStock(inventario.obtenerProductoPorNombre("Camisa"), 10);

        // **Generar informe de inventario:**

        String informe = inventario.generarInformeDeInventario();

        // **Imprimir informe:**

        System.out.println(informe);
    }

}

```

**Explicación del código:**

1. **Importar bibliotecas necesarias:**

   - `java.util.ArrayList`: Clase que representa una lista de elementos.
   - `java.util.HashMap`: Clase que representa un mapa de clave-valor.
   - `java.util.List`: Interfaz que representa una lista de elementos.
   - `java.util.Map`: Interfaz que representa un mapa de clave-valor.

2. **Clase principal:**

   - `SistemaGestionInventario`: Clase que representa el sistema de gestión de inventario.

3. **Atributos:**

   - `productos`: Lista de productos.
   - `stock`: Mapa que relaciona productos con sus cantidades en stock.

4. **Constructor:**

   - Crea una nueva instancia de `SistemaGestionInventario`.

5. **Métodos:**

   - `agregarProducto`: Añade un nuevo producto al sistema.
   - `eliminarProducto`: Elimina un producto del sistema.
   - `actualizarStock`: Actualiza la cantidad en stock de un producto.
   - `obtenerProductoPorNombre`: Obtiene un producto por su nombre.
   - `obtenerStockDeProducto`: Obtiene la cantidad en stock de un producto.
   - `generarInformeDeInventario`: Genera un informe de inventario.

6. **Clase Producto:**

   - `Producto`: Clase que representa un producto.

7. **Atributos:**

   - `id`: Identificador del producto.
   - `nombre`: Nombre del producto.
   - `precio`: Precio del producto.

8. **Constructor:**

   - Crea una nueva instancia de `Producto`.

9. **Métodos:**

   - `getId`: Obtiene el identificador del producto.
   - `getNombre`: Obtiene el nombre del producto.
   - `getPrecio`: Obtiene el precio del producto.

10. **Uso del sistema:**

    - `Main`: Clase que contiene el método `main` del programa.

11. **Método main:**

    - Crea una nueva instancia de `SistemaGestionInventario`.
    - Agrega algunos productos al sistema.
    - Actualiza el stock de un producto.
    - Genera un informe de inventario.
    - Imprime el informe.