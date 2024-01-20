```sql
-- Definición del esquema de la base de datos

CREATE SCHEMA [mi_esquema];

-- Crear tabla de clientes

CREATE TABLE [mi_esquema].[Clientes] (
    [ClienteID] INT IDENTITY(1,1) PRIMARY KEY,
    [Nombre] NVARCHAR(50) NOT NULL,
    [Apellido] NVARCHAR(50) NOT NULL,
    [Dirección] NVARCHAR(255),
    [Teléfono] NVARCHAR(20),
    [CorreoElectrónico] NVARCHAR(100) UNIQUE
);

-- Crear tabla de productos

CREATE TABLE [mi_esquema].[Productos] (
    [ProductoID] INT IDENTITY(1,1) PRIMARY KEY,
    [Nombre] NVARCHAR(50) NOT NULL,
    [Descripción] NVARCHAR(255),
    [Precio] DECIMAL(10, 2) NOT NULL,
    [CantidadEnStock] INT DEFAULT 0
);

-- Crear tabla de pedidos

CREATE TABLE [mi_esquema].[Pedidos] (
    [PedidoID] INT IDENTITY(1,1) PRIMARY KEY,
    [ClienteID] INT NOT NULL,
    [FechaPedido] DATETIME NOT NULL,
    [FechaEntrega] DATETIME,
    [EstadoPedido] NVARCHAR(50) NOT NULL DEFAULT 'Pendiente',
    [Total] DECIMAL(10, 2) NOT NULL
);

-- Crear tabla de detalles de pedidos

CREATE TABLE [mi_esquema].[DetallesPedidos] (
    [DetallePedidoID] INT IDENTITY(1,1) PRIMARY KEY,
    [PedidoID] INT NOT NULL,
    [ProductoID] INT NOT NULL,
    [Cantidad] INT NOT NULL,
    [PrecioUnitario] DECIMAL(10, 2) NOT NULL,
    [Total] DECIMAL(10, 2) NOT NULL
);

-- Insertar datos en la tabla de clientes

INSERT INTO [mi_esquema].[Clientes] ([Nombre], [Apellido], [Dirección], [Teléfono], [CorreoElectrónico])
VALUES
    ('Juan', 'Pérez', 'Calle Real 123, Ciudad', '555-123-4567', 'juan.perez@correo.com'),
    ('María', 'García', 'Avenida del Sol 456, Ciudad', '555-234-5678', 'maria.garcia@correo.com'),
    ('Pedro', 'López', 'Calle de la Luna 789, Ciudad', '555-345-6789', 'pedro.lopez@correo.com');

-- Insertar datos en la tabla de productos

INSERT INTO [mi_esquema].[Productos] ([Nombre], [Descripción], [Precio], [CantidadEnStock])
VALUES
    ('Computadora portátil', 'Computadora portátil con procesador Intel Core i7, 8 GB de RAM y 1 TB de almacenamiento.', 1000.00, 10),
    ('Teléfono inteligente', 'Teléfono inteligente con pantalla de 6.5 pulgadas, cámara de 12 MP y batería de larga duración.', 500.00, 20),
    ('Televisor', 'Televisor LED de 50 pulgadas con resolución 4K Ultra HD.', 700.00, 5);

-- Insertar datos en la tabla de pedidos

INSERT INTO [mi_esquema].[Pedidos] ([ClienteID], [FechaPedido], [FechaEntrega], [EstadoPedido], [Total])
VALUES
    (1, '2023-02-15', '2023-02-22', 'Completado', 1500.00),
    (2, '2023-03-01', '2023-03-08', 'Pendiente', 1000.00),
    (3, '2023-03-15', '2023-03-22', 'Cancelado', 500.00);

-- Insertar datos en la tabla de detalles de pedidos

INSERT INTO [mi_esquema].[DetallesPedidos] ([PedidoID], [ProductoID], [Cantidad], [PrecioUnitario], [Total])
VALUES
    (1, 1, 1, 1000.00, 1000.00),
    (1, 2, 1, 500.00, 500.00),
    (2, 1, 2, 1000.00, 2000.00),
    (3, 3, 1, 700.00, 700.00);

-- Consulta para obtener todos los datos de los clientes y sus pedidos

SELECT
    c.[ClienteID],
    c.[Nombre],
    c.[Apellido],
    c.[Dirección],
    c.[Teléfono],
    c.[CorreoElectrónico],
    o.[PedidoID],
    o.[FechaPedido],
    o.[FechaEntrega],
    o.[EstadoPedido],
    o.[Total]
FROM
    [mi_esquema].[Clientes] c
JOIN
    [mi_esquema].[Pedidos] o ON c.[ClienteID] = o.[ClienteID];

-- Explicación del código:

-- 1. Definimos el esquema de la base de datos con el comando `CREATE SCHEMA`. Esto crea un nuevo esquema llamado `mi_esquema` donde almacenaremos las tablas y los datos.

-- 2. Creamos la tabla `Clientes` con los campos necesarios para almacenar la información de los clientes, como su nombre, apellido, dirección, teléfono y correo electrónico.

-- 3. Creamos la tabla `Productos` con los campos necesarios para almacenar la información de los productos, como su nombre, descripción, precio y cantidad en stock.

-- 4. Creamos la tabla `Pedidos` con los campos necesarios para almacenar la información de los pedidos, como el cliente que realizó el pedido, la fecha del pedido, la fecha de entrega, el estado del pedido y el total del pedido.

-- 5. Creamos la tabla `DetallesPedidos` con los campos necesarios para almacenar los detalles de los pedidos, como el producto que se заказал, la cantidad del producto, el precio unitario del producto y el total del producto.

-- 6. Insertamos datos en las tablas `Clientes`, `Productos`, `Pedidos` y `DetallesPedidos` utilizando el comando `INSERT INTO`.

-- 7. Por último, ejecutamos una consulta para obtener todos los datos de los clientes y sus pedidos utilizando las cláusulas `SELECT` y `JOIN`.

Este código crea una base de datos completa con múltiples tablas y relaciones entre ellas, y demuestra el uso de diferentes comandos SQL para crear tablas, insertar datos y consultar datos de una base de datos.