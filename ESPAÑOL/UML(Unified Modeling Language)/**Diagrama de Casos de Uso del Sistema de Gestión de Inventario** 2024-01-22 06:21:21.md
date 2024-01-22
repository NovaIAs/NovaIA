**Diagrama de Casos de Uso**

```
Sistema de Gestión de Inventario
```

**Actores:**

- Administrador del Sistema
- Empleado de Almacén
- Cliente

**Casos de Uso:**

- Gestionar Productos
- Gestionar Pedidos
- Gestionar Clientes
- Gestionar Proveedores
- Generar Informes

**Diagrama de Clases**

```
Sistema de Gestión de Inventario
```

**Clases:**

- Producto
- Pedido
- Cliente
- Proveedor
- Empleado de Almacén
- Administrador del Sistema

**Atributos:**

- Producto:
  - IdProducto
  - Nombre
  - Descripción
  - PrecioUnitario
  - CantidadEnStock
- Pedido:
  - IdPedido
  - FechaPedido
  - EstadoPedido
  - FechaEntrega
  - Cliente_IdCliente
  - Empleado_IdEmpleado
- Cliente:
  - IdCliente
  - Nombre
  - Apellido
  - Dirección
  - Teléfono
  - CorreoElectrónico
- Proveedor:
  - IdProveedor
  - Nombre
  - Apellido
  - Dirección
  - Teléfono
  - CorreoElectrónico
- Empleado de Almacén:
  - IdEmpleado
  - Nombre
  - Apellido
  - Dirección
  - Teléfono
  - CorreoElectrónico
- Administrador del Sistema:
  - IdAdministrador
  - Nombre
  - Apellido
  - Dirección
  - Teléfono
  - CorreoElectrónico

**Métodos:**

- Producto:
  - GetIdProducto()
  - GetNombre()
  - GetDescripción()
  - GetPrecioUnitario()
  - GetCantidadEnStock()
  - SetIdProducto()
  - SetNombre()
  - SetDescripción()
  - SetPrecioUnitario()
  - SetCantidadEnStock()
- Pedido:
  - GetIdPedido()
  - GetFechaPedido()
  - GetEstadoPedido()
  - GetFechaEntrega()
  - GetCliente_IdCliente()
  - GetEmpleado_IdEmpleado()
  - SetIdPedido()
  - SetFechaPedido()
  - SetEstadoPedido()
  - SetFechaEntrega()
  - SetCliente_IdCliente()
  - SetEmpleado_IdEmpleado()
- Cliente:
  - GetIdCliente()
  - GetNombre()
  - GetApellido()
  - GetDirección()
  - GetTeléfono()
  - GetCorreoElectrónico()
  - SetIdCliente()
  - SetNombre()
  - SetApellido()
  - SetDirección()
  - SetTeléfono()
  - SetCorreoElectrónico()
- Proveedor:
  - GetIdProveedor()
  - GetNombre()
  - GetApellido()
  - GetDirección()
  - GetTeléfono()
  - GetCorreoElectrónico()
  - SetIdProveedor()
  - SetNombre()
  - SetApellido()
  - SetDirección()
  - SetTeléfono()
  - SetCorreoElectrónico()
- Empleado de Almacén:
  - GetIdEmpleado()
  - GetNombre()
  - GetApellido()
  - GetDirección()
  - GetTeléfono()
  - GetCorreoElectrónico()
  - SetIdEmpleado()
  - SetNombre()
  - SetApellido()
  - SetDirección()
  - SetTeléfono()
  - SetCorreoElectrónico()
- Administrador del Sistema:
  - GetIdAdministrador()
  - GetNombre()
  - GetApellido()
  - GetDirección()
  - GetTeléfono()
  - GetCorreoElectrónico()
  - SetIdAdministrador()
  - SetNombre()
  - SetApellido()
  - SetDirección()
  - SetTeléfono()
  - SetCorreoElectrónico()

**Diagrama de Secuencia**

```
Sistema de Gestión de Inventario
```

**Objeto:**

- Cliente
- Empleado de Almacén
- Administrador del Sistema

**Mensajes:**

- Cliente:
  - SolicitarProducto()
  - RealizarPedido()
  - ConsultarEstadoPedido()
- Empleado de Almacén:
  - ProcesarPedido()
  - ActualizarInventario()
  - GenerarFactura()
- Administrador del Sistema:
  - GestionarProductos()
  - GestionarPedidos()
  - GestionarClientes()
  - GestionarProveedores()
  - GenerarInformes()

**Diagrama de Actividad**

```
Sistema de Gestión de Inventario
```

**Actividad inicial:**

- Cliente solicita producto

**Actividades:**

- Empleado de almacén procesa pedido
- Empleado de almacén actualiza inventario
- Empleado de almacén genera factura
- Administrador del sistema gestiona productos
- Administrador del sistema gestiona pedidos
- Administrador del sistema gestiona clientes
- Administrador del sistema gestiona proveedores
- Administrador del sistema genera informes

**Actividad final:**

- Cliente recibe producto