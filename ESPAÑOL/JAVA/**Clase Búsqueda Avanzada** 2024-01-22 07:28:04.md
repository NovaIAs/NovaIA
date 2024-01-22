**Clase BúsquedaAvanzada:**

```java
import java.util.ArrayList;
import java.util.List;

public class BúsquedaAvanzada {

    // Crear una lista de productos
    private List<Producto> productos = new ArrayList<>();

    // Añadir algunos productos a la lista
    public void añadirProductos() {
        productos.add(new Producto("Camisa", "Ropa", 10.0));
        productos.add(new Producto("Pantalones", "Ropa", 15.0));
        productos.add(new Producto("Zapatos", "Calzado", 20.0));
        productos.add(new Producto("Libro", "Libros", 5.0));
        productos.add(new Producto("Ordenador", "Electrónica", 1000.0));
    }

    // Buscar productos por nombre
    public List<Producto> buscarPorNombre(String nombre) {
        List<Producto> resultados = new ArrayList<>();
        for (Producto producto : productos) {
            if (producto.getNombre().toLowerCase().contains(nombre.toLowerCase())) {
                resultados.add(producto);
            }
        }
        return resultados;
    }

    // Buscar productos por categoría
    public List<Producto> buscarPorCategoría(String categoría) {
        List<Producto> resultados = new ArrayList<>();
        for (Producto producto : productos) {
            if (producto.getCategoría().toLowerCase().contains(categoría.toLowerCase())) {
                resultados.add(producto);
            }
        }
        return resultados;
    }

    // Buscar productos por precio máximo
    public List<Producto> buscarPorPrecioMáximo(double precioMáximo) {
        List<Producto> resultados = new ArrayList<>();
        for (Producto producto : productos) {
            if (producto.getPrecio() <= precioMáximo) {
                resultados.add(producto);
            }
        }
        return resultados;
    }

    // Buscar productos por rango de precios
    public List<Producto> buscarPorRangoDePrecios(double precioMínimo, double precioMáximo) {
        List<Producto> resultados = new ArrayList<>();
        for (Producto producto : productos) {
            if (producto.getPrecio() >= precioMínimo && producto.getPrecio() <= precioMáximo) {
                resultados.add(producto);
            }
        }
        return resultados;
    }

    // Clase anidada Producto
    private static class Producto {

        private String nombre;
        private String categoría;
        private double precio;

        public Producto(String nombre, String categoría, double precio) {
            this.nombre = nombre;
            this.categoría = categoría;
            this.precio = precio;
        }

        public String getNombre() {
            return nombre;
        }

        public String getCategoría() {
            return categoría;
        }

        public double getPrecio() {
            return precio;
        }
    }
}
```

**Clase principal:**

```java
public class Main {

    public static void main(String[] args) {
        // Crear una instancia de la clase BúsquedaAvanzada
        BúsquedaAvanzada búsquedaAvanzada = new BúsquedaAvanzada();

        // Añadir algunos productos a la lista
        búsquedaAvanzada.añadirProductos();

        // Buscar productos por nombre
        List<Producto> resultadosNombre = búsquedaAvanzada.buscarPorNombre("ro");
        System.out.println("Productos encontrados por nombre:");
        for (Producto producto : resultadosNombre) {
            System.out.println(producto.getNombre());
        }

        // Buscar productos por categoría
        List<Producto> resultadosCategoría = búsquedaAvanzada.buscarPorCategoría("ropa");
        System.out.println("Productos encontrados por categoría:");
        for (Producto producto : resultadosCategoría) {
            System.out.println(producto.getNombre());
        }

        // Buscar productos por precio máximo
        List<Producto> resultadosPrecioMáximo = búsquedaAvanzada.buscarPorPrecioMáximo(100.0);
        System.out.println("Productos encontrados por precio máximo:");
        for (Producto producto : resultadosPrecioMáximo) {
            System.out.println(producto.getNombre());
        }

        // Buscar productos por rango de precios
        List<Producto> resultadosRangoDePrecios = búsquedaAvanzada.buscarPorRangoDePrecios(10.0, 50.0);
        System.out.println("Productos encontrados por rango de precios:");
        for (Producto producto : resultadosRangoDePrecios) {
            System.out.println(producto.getNombre());
        }
    }
}
```

**Explicación:**

1. La clase `BúsquedaAvanzada` contiene una lista de productos y varios métodos para buscar productos en la lista.
2. El método `añadirProductos()` se utiliza para añadir algunos productos a la lista.
3. El método `buscarPorNombre()` busca productos por nombre.
4. El método `buscarPorCategoría()` busca productos por categoría.
5. El método `buscarPorPrecioMáximo()` busca productos por precio máximo.
6. El método `buscarPorRangoDePrecios()` busca productos por rango de precios.
7. La clase `Producto` es una clase anidada privada de la clase `BúsquedaAvanzada`. Representa un producto con un nombre, una categoría y un precio.
8. La clase `Main` crea una instancia de la clase `BúsquedaAvanzada`, añade algunos productos a la lista y luego busca productos utilizando los diferentes métodos de búsqueda.
9. Los resultados de la búsqueda se imprimen en la consola.