**Problema:**
Diseñar un programa en JAVA que gestione el inventario de una empresa. El programa debe permitir agregar, eliminar, actualizar y mostrar los productos del inventario, así como realizar búsquedas por nombre, precio y categoría.

**Solución:**

```java
import java.util.Scanner;
import java.util.ArrayList;

public class Inventario {

    private ArrayList<Producto> productos;

    public Inventario() {
        productos = new ArrayList<>();
    }

    public void agregarProducto(Producto producto) {
        productos.add(producto);
    }

    public void eliminarProducto(int codigo) {
        for (Producto producto : productos) {
            if (producto.getCodigo() == codigo) {
                productos.remove(producto);
                break;
            }
        }
    }

    public void actualizarProducto(Producto producto) {
        for (Producto producto_actual : productos) {
            if (producto_actual.getCodigo() == producto.getCodigo()) {
                producto_actual.setNombre(producto.getNombre());
                producto_actual.setPrecio(producto.getPrecio());
                producto_actual.setCategoria(producto.getCategoria());
                break;
            }
        }
    }

    public void mostrarProductos() {
        for (Producto producto : productos) {
            System.out.println(producto);
        }
    }

    public ArrayList<Producto> buscarProductosPorNombre(String nombre) {
        ArrayList<Producto> productos_encontrados = new ArrayList<>();
        for (Producto producto : productos) {
            if (producto.getNombre().contains(nombre)) {
                productos_encontrados.add(producto);
            }
        }
        return productos_encontrados;
    }

    public ArrayList<Producto> buscarProductosPorPrecio(double precio) {
        ArrayList<Producto> productos_encontrados = new ArrayList<>();
        for (Producto producto : productos) {
            if (producto.getPrecio() == precio) {
                productos_encontrados.add(producto);
            }
        }
        return productos_encontrados;
    }

    public ArrayList<Producto> buscarProductosPorCategoria(String categoria) {
        ArrayList<Producto> productos_encontrados = new ArrayList<>();
        for (Producto producto : productos) {
            if (producto.getCategoria().equals(categoria)) {
                productos_encontrados.add(producto);
            }
        }
        return productos_encontrados;
    }

    public static void main(String[] args) {
        Scanner entrada = new Scanner(System.in);
        Inventario inventario = new Inventario();

        while (true) {
            System.out.println("1. Agregar producto");
            System.out.println("2. Eliminar producto");
            System.out.println("3. Actualizar producto");
            System.out.println("4. Mostrar productos");
            System.out.println("5. Buscar productos por nombre");
            System.out.println("6. Buscar productos por precio");
            System.out.println("7. Buscar productos por categoría");
            System.out.println("8. Salir");

            int opcion = entrada.nextInt();

            switch (opcion) {
                case 1:
                    System.out.println("Ingrese el código del producto:");
                    int codigo = entrada.nextInt();
                    System.out.println("Ingrese el nombre del producto:");
                    String nombre = entrada.nextLine();
                    System.out.println("Ingrese el precio del producto:");
                    double precio = entrada.nextDouble();
                    System.out.println("Ingrese la categoría del producto:");
                    String categoria = entrada.nextLine();

                    Producto producto = new Producto(codigo, nombre, precio, categoria);
                    inventario.agregarProducto(producto);
                    break;
                case 2:
                    System.out.println("Ingrese el código del producto a eliminar:");
                    int codigo_eliminar = entrada.nextInt();
                    inventario.eliminarProducto(codigo_eliminar);
                    break;
                case 3:
                    System.out.println("Ingrese el código del producto a actualizar:");
                    int codigo_actualizar = entrada.nextInt();
                    System.out.println("Ingrese el nuevo nombre del producto:");
                    String nuevo_nombre = entrada.nextLine();
                    System.out.println("Ingrese el nuevo precio del producto:");
                    double nuevo_precio = entrada.nextDouble();
                    System.out.println("Ingrese la nueva categoría del producto:");
                    String nueva_categoria = entrada.nextLine();

                    Producto producto_actualizar = new Producto(codigo_actualizar, nuevo_nombre, nuevo_precio, nueva_categoria);
                    inventario.actualizarProducto(producto_actualizar);
                    break;
                case 4:
                    inventario.mostrarProductos();
                    break;
                case 5:
                    System.out.println("Ingrese el nombre del producto a buscar:");
                    String nombre_buscar = entrada.nextLine();
                    ArrayList<Producto> productos_encontrados_nombre = inventario.buscarProductosPorNombre(nombre_buscar);
                    if (productos_encontrados_nombre.isEmpty()) {
                        System.out.println("No se encontraron productos con ese nombre.");
                    } else {
                        System.out.println("Productos encontrados:");
                        for (Producto producto_encontrado : productos_encontrados_nombre) {
                            System.out.println(producto_encontrado);
                        }
                    }
                    break;
                case 6:
                    System.out.println("Ingrese el precio del producto a buscar:");
                    double precio_buscar = entrada.nextDouble();
                    ArrayList<Producto> productos_encontrados_precio = inventario.buscarProductosPorPrecio(precio_buscar);
                    if (productos_encontrados_precio.isEmpty()) {
                        System.out.println("No se encontraron productos con ese precio.");
                    } else {
                        System.out.println("Productos encontrados:");
                        for (Producto producto_encontrado : productos_encontrados_precio) {
                            System.out.println(producto_encontrado);
                        }
                    }
                    break;
                case 7:
                    System.out.println("Ingrese la categoría del producto a buscar:");
                    String categoria_buscar = entrada.nextLine();
                    ArrayList<Producto> productos_encontrados_categoria = inventario.buscarProductosPorCategoria(categoria_buscar);
                    if (productos_encontrados_categoria.isEmpty()) {
                        System.out.println("No se encontraron productos con esa categoría.");
                    } else {
                        System.out.println("Productos encontrados:");
                        for (Producto producto_encontrado : productos_encontrados_categoria) {
                            System.out.println(producto_encontrado);
                        }
                    }
                    break;
                case 8:
                    System.exit(0);
            }
        }
    }
}

class Producto {

    private int codigo;
    private String nombre;
    private double precio;
    private String categoria;

    public Producto() {}

    public Producto(int codigo, String nombre, double precio, String categoria) {
        this.codigo = codigo;
        this.nombre = nombre;
        this.precio = precio;
        this.categoria = categoria;
    }

    public int getCodigo() {
        return codigo;
    }

    public void setCodigo(int codigo) {
        this.codigo = codigo;
    }

    public String getNombre() {
        return nombre;
    }

    public void setNombre(String nombre) {
        this