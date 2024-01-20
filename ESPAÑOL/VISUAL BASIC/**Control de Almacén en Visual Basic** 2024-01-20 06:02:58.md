```visual basic

' Este código es un ejemplo de una aplicación de control de almacén en Visual Basic.

' Definición de variables
Dim productos As New Collection
Dim proveedores As New Collection
Dim clientes As New Collection

' Definición de clases
Class Producto
    Public nombre As String
    Public precio As Double
    Public stock As Integer

    ' Constructor
    Public Sub New(nombre As String, precio As Double, stock As Integer)
        Me.nombre = nombre
        Me.precio = precio
        Me.stock = stock
    End Sub

    ' Métodos
    Public Function ToString() As String
        Return nombre & ", " & precio & ", " & stock
    End Function
End Class

Class Proveedor
    Public nombre As String
    Public direccion As String
    Public telefono As String

    ' Constructor
    Public Sub New(nombre As String, direccion As String, telefono As String)
        Me.nombre = nombre
        Me.direccion = direccion
        Me.telefono = telefono
    End Sub

    ' Métodos
    Public Function ToString() As String
        Return nombre & ", " & direccion & ", " & telefono
    End Function
End Class

Class Cliente
    Public nombre As String
    Public direccion As String
    Public telefono As String

    ' Constructor
    Public Sub New(nombre As String, direccion As String, telefono As String)
        Me.nombre = nombre
        Me.direccion = direccion
        Me.telefono = telefono
    End Sub

    ' Métodos
    Public Function ToString() As String
        Return nombre & ", " & direccion & ", " & telefono
    End Function
End Class

' Definición de formularios
Public Class Form1
    Private Sub Form_Load(sender As Object, e As EventArgs) Handles Me.Load
        ' Cargar datos de prueba
        productos.Add(New Producto("Producto 1", 10.0, 10))
        productos.Add(New Producto("Producto 2", 20.0, 20))
        productos.Add(New Producto("Producto 3", 30.0, 30))

        proveedores.Add(New Proveedor("Proveedor 1", "Dirección 1", "12345678"))
        proveedores.Add(New Proveedor("Proveedor 2", "Dirección 2", "23456789"))
        proveedores.Add(New Proveedor("Proveedor 3", "Dirección 3", "34567890"))

        clientes.Add(New Cliente("Cliente 1", "Dirección 1", "12345678"))
        clientes.Add(New Cliente("Cliente 2", "Dirección 2", "23456789"))
        clientes.Add(New Cliente("Cliente 3", "Dirección 3", "34567890"))

        ' Mostrar datos en los controles
        lstProductos.DataSource = productos
        lstProductos.DisplayMember = "ToString"

        lstProveedores.DataSource = proveedores
        lstProveedores.DisplayMember = "ToString"

        lstClientes.DataSource = clientes
        lstClientes.DisplayMember = "ToString"
    End Sub
End Class

```

* **Clase `Producto`:** Representa un producto en el almacén. Tiene propiedades para el nombre, el precio y el stock.
* **Clase `Proveedor`:** Representa un proveedor del almacén. Tiene propiedades para el nombre, la dirección y el teléfono.
* **Clase `Cliente`:** Representa un cliente del almacén. Tiene propiedades para el nombre, la dirección y el teléfono.
* **Formulario `Form1`:** Es el formulario principal de la aplicación. Carga los datos de prueba en las colecciones de productos, proveedores y clientes. También muestra los datos en los controles del formulario.

Este es un ejemplo sencillo de una aplicación de control de almacén en Visual Basic. Se puede ampliar para incluir más funciones, como gestión de pedidos, gestión de facturas y gestión de pagos.