```java
// Este programa es un simulador de un sistema de gestión de inventario.
// Tiene las siguientes características:
// - Mantiene un registro de los productos en el inventario.
// - Permite añadir, eliminar y modificar productos.
// - Permite realizar pedidos de productos.
// - Permite generar informes sobre el inventario.

// El programa está dividido en varias clases:
// - Inventario: Esta clase representa el inventario. Contiene una lista de los productos que están en el inventario.
// - Producto: Esta clase representa un producto. Contiene información como el nombre, el precio y la cantidad del producto.
// - Pedido: Esta clase representa un pedido de productos. Contiene una lista de los productos que se han pedido.
// - Informe: Esta clase representa un informe sobre el inventario. Contiene información como la lista de los productos en el inventario y el valor total del inventario.

// El programa se ejecuta de la siguiente manera:
// 1. El usuario introduce una orden.
// 2. El programa procesa la orden y realiza las acciones necesarias.
// 3. El programa genera un informe sobre el inventario.

// El código del programa es el siguiente:

// Clase Inventario
public class Inventario {

    // Lista de los productos en el inventario.
    private List<Producto> productos;

    // Constructor de la clase.
    public Inventario() {
        this.productos = new ArrayList<Producto>();
    }

    // Método para añadir un producto al inventario.
    public void añadirProducto(Producto producto) {
        this.productos.add(producto);
    }

    // Método para eliminar un producto del inventario.
    public void eliminarProducto(Producto producto) {
        this.productos.remove(producto);
    }

    // Método para modificar un producto del inventario.
    public void modificarProducto(Producto producto) {
        int index = this.productos.indexOf(producto);
        this.productos.set(index, producto);
    }

    // Método para realizar un pedido de productos.
    public void realizarPedido(Pedido pedido) {
        for (Producto producto : pedido.getProductos()) {
            int index = this.productos.indexOf(producto);
            Producto productoEnInventario = this.productos.get(index);
            productoEnInventario.setCantidad(productoEnInventario.getCantidad() - pedido.getCantidad());
        }
    }

    // Método para generar un informe sobre el inventario.
    public Informe generarInforme() {
        Informe informe = new Informe();
        informe.setProductos(this.productos);
        informe.setValorTotal(this.calcularValorTotal());
        return informe;
    }

    // Método para calcular el valor total del inventario.
    private float calcularValorTotal() {
        float valorTotal = 0;
        for (Producto producto : this.productos) {
            valorTotal += producto.getPrecio() * producto.getCantidad();
        }
        return valorTotal;
    }
}

// Clase Producto
public class Producto {

    // Nombre del producto.
    private String nombre;

    // Precio del producto.
    private float precio;

    // Cantidad del producto.
    private int cantidad;

    // Constructor de la clase.
    public Producto(String nombre, float precio, int cantidad) {
        this.nombre = nombre;
        this.precio = precio;
        this.cantidad = cantidad;
    }

    // Métodos get y set para los atributos de la clase.
    public String getNombre() {
        return nombre;
    }

    public void setNombre(String nombre) {
        this.nombre = nombre;
    }

    public float getPrecio() {
        return precio;
    }

    public void setPrecio(float precio) {
        this.precio = precio;
    }

    public int getCantidad() {
        return cantidad;
    }

    public void setCantidad(int cantidad) {
        this.cantidad = cantidad;