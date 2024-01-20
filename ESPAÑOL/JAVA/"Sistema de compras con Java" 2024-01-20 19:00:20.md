```java
// Importar las librerías necesarias
import java.util.Scanner;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

// Crear una clase para representar un producto con sus propiedades
class Producto {
    private String nombre;
    private double precio;
    private int cantidad;

    public Producto(String nombre, double precio, int cantidad) {
        this.nombre = nombre;
        this.precio = precio;
        this.cantidad = cantidad;
    }

    public String getNombre() {
        return nombre;
    }

    public double getPrecio() {
        return precio;
    }

    public int getCantidad() {
        return cantidad;
    }

    public double getValorTotal() {
        return precio * cantidad;
    }
}

// Crear una clase para representar la lista de compras
class ListaDeCompras {
    private ArrayList<Producto> productos;

    public ListaDeCompras() {
        productos = new ArrayList<>();
    }

    public void agregarProducto(Producto producto) {
        productos.add(producto);
    }

    public ArrayList<Producto> getProductos() {
        return productos;
    }

    public double getValorTotal() {
        double valorTotal = 0;
        for (Producto producto : productos) {
            valorTotal += producto.getValorTotal();
        }
        return valorTotal;
    }
}

// Crear una clase para representar el carrito de compras
class CarritoDeCompras {
    private ListaDeCompras listaDeCompras;

    public CarritoDeCompras() {
        listaDeCompras = new ListaDeCompras();
    }

    public void agregarProducto(Producto producto) {
        listaDeCompras.agregarProducto(producto);
    }

    public ListaDeCompras getListaDeCompras() {
        return listaDeCompras;
    }

    public double getValorTotal() {
        return listaDeCompras.getValorTotal();
    }
}

// Crear una clase para representar la tienda
class Tienda {
    private ArrayList<Producto> productos;

    public Tienda() {
        productos = new ArrayList<>();
    }

    public void agregarProducto(Producto producto) {
        productos.add(producto);
    }

    public ArrayList<Producto> getProductos() {
        return productos;
    }

    public void mostrarProductos() {
        for (Producto producto : productos) {
            System.out.println(producto.getNombre() + " - $" + producto.getPrecio());
        }
    }
}

// Crear una clase principal para ejecutar el programa
public class Main {

    public static void main(String[] args) {
        // Crear la tienda
        Tienda tienda = new Tienda();

        // Crear algunos productos
        tienda.agregarProducto(new Producto("Manzanas", 1.50, 5));
        tienda.agregarProducto(new Producto("Peras", 2.00, 3));
        tienda.agregarProducto(new Producto("Uvas", 3.00, 2));
        tienda.agregarProducto(new Producto("Sandía", 5.00, 1));

        // Mostrar los productos de la tienda
        System.out.println("Productos de la tienda:");
        tienda.mostrarProductos();

        // Crear el carrito de compras
        CarritoDeCompras carritoDeCompras = new CarritoDeCompras();

        // Crear el scanner para leer la entrada del usuario
        Scanner scanner = new Scanner(System.in);

        // Solicitar al usuario que elija los productos que desea comprar
        System.out.println("Elige los productos que deseas comprar:");
        while (true) {
            String nombreProducto = scanner.nextLine();
            if (nombreProducto.equals("fin")) {
                break;
            }

            // Buscar el producto en la tienda
            Producto producto = null;
            for (Producto p : tienda.getProductos()) {
                if (p.getNombre().equals(nombreProducto)) {
                    producto = p;
                    break;
                }
            }

            // Si el producto no se encuentra en la tienda, mostrar un mensaje de error
            if (producto == null) {
                System.out.println("El producto no se encuentra en la tienda.");
            } else {
                // Solicitar al usuario la cantidad del producto que desea comprar
                System.out.println("¿Cuántos productos deseas comprar?");
                int cantidad = scanner.nextInt();

                // Agregar el producto al carrito de compras
                carritoDeCompras.agregarProducto(new Producto(producto.getNombre(), producto.getPrecio(), cantidad));
            }
        }

        // Mostrar el resumen del carrito de compras
        System.out.println("Resumen del carrito de compras:");
        ListaDeCompras listaDeCompras = carritoDeCompras.getListaDeCompras();
        for (Producto producto : listaDeCompras.getProductos()) {
            System.out.println(producto.getNombre() + " - $" + producto.getPrecio() + " - Cantidad: " + producto.getCantidad());
        }

        // Mostrar el valor total del carrito de compras
        System.out.println("Valor total: $" + carritoDeCompras.getValorTotal());

        // Ordenar la lista de productos por precio de