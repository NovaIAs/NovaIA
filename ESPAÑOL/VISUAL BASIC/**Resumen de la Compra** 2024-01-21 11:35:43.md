```visual basic
' Definir variables
Dim nombreCliente As String
Dim direccionCliente As String
Dim telefonoCliente As String
Dim correoCliente As String
Dim productosComprados As Collection
Dim totalCompra As Double

' Solicitar datos al cliente
nombreCliente = InputBox("Ingrese su nombre completo:")
direccionCliente = InputBox("Ingrese su dirección:")
telefonoCliente = InputBox("Ingrese su número de teléfono:")
correoCliente = InputBox("Ingrese su correo electrónico:")

' Crear una colección para almacenar los productos comprados
Set productosComprados = New Collection

' Solicitar datos de los productos comprados
Do
    Dim nombreProducto As String
    Dim cantidadProducto As Long
    Dim precioProducto As Double

    nombreProducto = InputBox("Ingrese el nombre del producto:")
    cantidadProducto = InputBox("Ingrese la cantidad del producto:")
    precioProducto = InputBox("Ingrese el precio del producto:")

    ' Calcular el total de la compra
    totalCompra = totalCompra + (cantidadProducto * precioProducto)
    
    ' Agregar el producto comprado a la colección
    productosComprados.Add nombreProducto & " (" & cantidadProducto & " unidad(es)) a $" & precioProducto & " cada una", nombreProducto

Loop Until InputBox("¿Desea agregar otro producto? (S/N)") = "N"

' Mostrar resumen de la compra
MsgBox "Resumen de la compra:" & vbCrLf & _
    "Cliente: " & nombreCliente & vbCrLf & _
    "Dirección: " & direccionCliente & vbCrLf & _
    "Teléfono: " & telefonoCliente & vbCrLf & _
    "Correo electrónico: " & correoCliente & vbCrLf & _
    "Productos comprados:" & vbCrLf

' Recorrer la colección de productos comprados y mostrarlos
For Each productoComprado In productosComprados
    MsgBox productoComprado
Next

' Mostrar el total de la compra
MsgBox "Total de la compra: $" & totalCompra
```

Este código crea una aplicación que permite a un usuario ingresar información sobre su compra, incluidos sus datos personales, los productos comprados y el total de la compra. La información ingresada se almacena en variables y se muestra en un cuadro de mensaje.

El código utiliza una colección para almacenar los productos comprados, lo que permite al usuario agregar varios productos a su compra. La colección se utiliza para recorrer y mostrar los productos comprados en el cuadro de mensaje.

El código también utiliza una variable para almacenar el total de la compra, que se calcula multiplicando la cantidad de cada producto por su precio y sumando los resultados. El total de la compra se muestra en un cuadro de mensaje.

Este código es un ejemplo de cómo utilizar variables, colecciones y cuadros de mensaje para crear una aplicación simple.