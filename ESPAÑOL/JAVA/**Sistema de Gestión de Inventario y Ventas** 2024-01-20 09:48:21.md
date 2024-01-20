* **Introducción**

Este código en JAVA es un proyecto complejo y completo que implementa un sistema de gestión de inventario y ventas utilizando una interfaz gráfica de usuario (GUI). El sistema permite a los usuarios administrar productos, clientes, órdenes de compra y ventas, y generar informes.

* **Código**

```java
// Importamos las bibliotecas necesarias
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.sql.*;

// Creamos la clase principal de la aplicación
public class SistemaInventarioVentas {

    // Creamos la ventana principal de la aplicación
    private JFrame ventanaPrincipal;

    // Creamos los paneles de la aplicación
    private JPanel panelProductos;
    private JPanel panelClientes;
    private JPanel panelOrdenesCompra;
    private JPanel panelVentas;
    private JPanel panelInformes;

    // Creamos los componentes de la aplicación
    private JTable tablaProductos;
    private JTextField textFieldNombreProducto;
    private JTextField textFieldPrecioProducto;
    private JButton botonAgregarProducto;

    private JTable tablaClientes;
    private JTextField textFieldNombreCliente;
    private JTextField textFieldDireccionCliente;
    private JButton botonAgregarCliente;

    private JTable tablaOrdenesCompra;
    private JTextField textFieldNumeroOrdenCompra;
    private JTextField textFieldFechaOrdenCompra;
    private JButton botonAgregarOrdenCompra;

    private JTable tablaVentas;
    private JTextField textFieldNumeroVenta;
    private JTextField textFieldFechaVenta;
    private JButton botonAgregarVenta;

    private JTable tablaInformes;
    private JButton botonGenerarInforme;

    // Creamos la conexión a la base de datos
    private Connection conexion;

    // Creamos el constructor de la clase
    public SistemaInventarioVentas() {
        // Creamos la ventana principal de la aplicación
        ventanaPrincipal = new JFrame("Sistema de Gestión de Inventario y Ventas");
        ventanaPrincipal.setSize(800, 600);
        ventanaPrincipal.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        // Creamos los paneles de la aplicación
        panelProductos = new JPanel();
        panelClientes = new JPanel();
        panelOrdenesCompra = new JPanel();
        panelVentas = new JPanel();
        panelInformes = new JPanel();

        // Creamos los componentes de la aplicación
        tablaProductos = new JTable();
        textFieldNombreProducto = new JTextField();
        textFieldPrecioProducto = new JTextField();
        botonAgregarProducto = new JButton("Agregar producto");

        tablaClientes = new JTable();
        textFieldNombreCliente = new JTextField();
        textFieldDireccionCliente = new JTextField();
        botonAgregarCliente = new JButton("Agregar cliente");

        tablaOrdenesCompra = new JTable();
        textFieldNumeroOrdenCompra = new JTextField();
        textFieldFechaOrdenCompra = new JTextField();
        botonAgregarOrdenCompra = new JButton("Agregar orden de compra");

        tablaVentas = new JTable();
        textFieldNumeroVenta = new JTextField();
        textFieldFechaVenta = new JTextField();
        botonAgregarVenta = new JButton("Agregar venta");

        tablaInformes = new JTable();
        botonGenerarInforme = new JButton("Generar informe");

        // Añadimos los paneles a la ventana principal
        ventanaPrincipal.add(panelProductos);
        ventanaPrincipal.add(panelClientes);
        ventanaPrincipal.add(panelOrdenesCompra);
        ventanaPrincipal.add(panelVentas);
        ventanaPrincipal.add(panelInformes);

        // Añadimos los componentes a los paneles
        panelProductos.add(tablaProductos);
        panelProductos.add(textFieldNombreProducto);
        panelProductos.add(textFieldPrecioProducto);
        panelProductos.add(botonAgregarProducto);

        panelClientes.add(tablaClientes);
        panelClientes.add(textFieldNombreCliente);
        panelClientes.add(textFieldDireccionCliente);
        panelClientes.add(botonAgregarCliente);

        panelOrdenesCompra.add(tablaOrdenesCompra);
        panelOrdenesCompra.add(textFieldNumeroOrdenCompra);
        panelOrdenesCompra.add(textFieldFechaOrdenCompra);
        panelOrdenesCompra.add(botonAgregarOrdenCompra);

        panelVentas.add(tablaVentas);
        panelVentas.add(textFieldNumeroVenta);
        panelVentas.add(textFieldFechaVenta);
        panelVentas.add(botonAgregarVenta);

        panelInformes.add(tablaInformes);
        panelInformes.add(botonGenerarInforme);

        // Conectamos a la base de datos
        conexion = DriverManager.getConnection("jdbc:mysql://localhost:3306/sistema_inventario_ventas", "root", "");

        // Añadimos los listeners a los componentes
        botonAgregarProducto.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                // Obtenemos los datos del producto
                String nombreProducto = textFieldNombreProducto.getText();
                double precioProducto = Double.parseDouble(textFieldPrecioProducto.getText());

                // Creamos el producto
                Producto producto = new Producto(nombreProducto, precioProducto);

                // Insertamos el producto en la base de datos
                try {
                    PreparedStatement statement = conexion.prepareStatement("INSERT INTO productos (nombre, precio) VALUES (?, ?)");
                    statement.setString(1, producto.getNombre());
                    statement.setDouble(2, producto.getPrecio());
                    statement.executeUpdate();
                } catch (SQLException ex) {
                    JOptionPane.showMessageDialog(ventanaPrincipal, "Error al insertar el producto en la base de datos.", "Error", JOptionPane.ERROR_MESSAGE);
                }

                // Actualizamos la tabla de productos