```sql
-- Creación de la base de datos "Ventas"
CREATE DATABASE Ventas;

-- Selección de la base de datos "Ventas"
USE Ventas;

-- Creación de la tabla "Productos"
CREATE TABLE Productos (
  idProducto INT NOT NULL AUTO_INCREMENT,
  nombreProducto VARCHAR(255) NOT NULL,
  precioProducto DECIMAL(10, 2) NOT NULL,
  stockProducto INT NOT NULL,
  PRIMARY KEY (idProducto)
);

-- Creación de la tabla "Clientes"
CREATE TABLE Clientes (
  idCliente INT NOT NULL AUTO_INCREMENT,
  nombreCliente VARCHAR(255) NOT NULL,
  direccionCliente VARCHAR(255) NOT NULL,
  telefonoCliente VARCHAR(255) NOT NULL,
  PRIMARY KEY (idCliente)
);

-- Creación de la tabla "Ventas"
CREATE TABLE Ventas (
  idVenta INT NOT NULL AUTO_INCREMENT,
  idProducto INT NOT NULL,
  idCliente INT NOT NULL,
  fechaVenta TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  cantidadVendida INT NOT NULL,
  precioTotal DECIMAL(10, 2) NOT NULL,
  PRIMARY KEY (idVenta),
  FOREIGN KEY (idProducto) REFERENCES Productos(idProducto),
  FOREIGN KEY (idCliente) REFERENCES Clientes(idCliente)
);

-- Creación de la tabla "Proveedores"
CREATE TABLE Proveedores (
  idProveedor INT NOT NULL AUTO_INCREMENT,
  nombreProveedor VARCHAR(255) NOT NULL,
  direccionProveedor VARCHAR(255) NOT NULL,
  telefonoProveedor VARCHAR(255) NOT NULL,
  PRIMARY KEY (idProveedor)
);

-- Creación de la tabla "Compras"
CREATE TABLE Compras (
  idCompra INT NOT NULL AUTO_INCREMENT,
  idProveedor INT NOT NULL,
  idProducto INT NOT NULL,
  fechaCompra TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  cantidadComprada INT NOT NULL,
  precioTotal DECIMAL(10, 2) NOT NULL,
  PRIMARY KEY (idCompra),
  FOREIGN KEY (idProveedor) REFERENCES Proveedores(idProveedor),
  FOREIGN KEY (idProducto) REFERENCES Productos(idProducto)
);

-- Creación de la vista "Ventas por Producto"
CREATE VIEW VentasPorProducto AS
SELECT
  p.nombreProducto,
  SUM(v.cantidadVendida) AS cantidadVendidaTotal,
  SUM(v.precioTotal) AS precioTotalVendido
FROM
  Productos p
JOIN
  Ventas v ON p.idProducto = v.idProducto
GROUP BY
  p.nombreProducto;

-- Creación de la vista "Clientes que más compran"
CREATE VIEW ClientesQueMasCompran AS
SELECT
  c.nombreCliente,
  COUNT(v.idCliente) AS cantidadCompras,
  SUM(v.precioTotal) AS montoTotalCompras
FROM
  Clientes c
JOIN
  Ventas v ON c.idCliente = v.idCliente
GROUP BY
  c.nombreCliente
ORDER BY
  cantidadCompras DESC, montoTotalCompras DESC;

-- Creación de la vista "Productos más vendidos"
CREATE VIEW ProductosMasVendidos AS
SELECT
  p.nombreProducto,
  SUM(v.cantidadVendida) AS cantidadVendidaTotal,
  SUM(v.precioTotal) AS precioTotalVendido
FROM
  Productos p
JOIN
  Ventas v ON p.idProducto = v.idProducto
GROUP BY
  p.nombreProducto
ORDER BY
  cantidadVendidaTotal DESC, precioTotalVendido DESC;

-- Creación de la vista "Proveedores que más venden"
CREATE VIEW ProveedoresQueMasVenden AS
SELECT
  pr.nombreProveedor,
  SUM(c.cantidadComprada) AS cantidadCompradaTotal,
  SUM(c.precioTotal) AS precioTotalComprado
FROM
  Proveedores pr
JOIN
  Compras c ON pr.idProveedor = c.idProveedor
GROUP BY
  pr.nombreProveedor
ORDER BY
  cantidadCompradaTotal DESC, precioTotalComprado DESC;

-- Creación de la vista "Productos con poco stock"
CREATE VIEW ProductosConPocoStock AS
SELECT
  p.nombreProducto,
  p.stockProducto
FROM
  Productos p
WHERE
  p.stockProducto < 10;

-- Creación del procedimiento almacenado "ActualizarStockProducto"
CREATE PROCEDURE ActualizarStockProducto(
  idProducto INT,
  cantidadVendida INT
)
AS
  UPDATE Productos
  SET stockProducto = stockProducto - cantidadVendida
  WHERE idProducto = idProducto;

-- Creación del procedimiento almacenado "ActualizarPrecioProducto"
CREATE PROCEDURE ActualizarPrecioProducto(
  idProducto INT,
  nuevoPrecio DECIMAL(10, 2)
)
AS
  UPDATE Productos
  SET precioProducto = nuevoPrecio
  WHERE idProducto = idProducto;

-- Creación de la función "CalcularDescuento"
CREATE FUNCTION CalcularDescuento(
  precioProducto DECIMAL(10, 2),
  porcentajeDescuento INT
)
RETURNS DECIMAL(10, 2)
AS
  RETURN precioProducto * (1 - (porcentajeDescuento / 100));
```

**Explicación del código:**

* Se crea la base de datos "Ventas" y se selecciona.
* Se crean las tablas "Productos", "Clientes", "Ventas", "Proveedores" y "Compras".
* Se crean las vistas "Ventas por Producto", "Clientes que más compran", "Productos más vendidos" y "Proveedores que más venden".
* Se crea el procedimiento almacenado "ActualizarStockProducto".
* Se crea la función "CalcularDescuento".

Este código crea una base de datos completa para gestionar un sistema de ventas. Incluye tablas para productos, clientes, ventas, proveedores y compras. También incluye vistas para analizar los datos de ventas y procedimientos almacenados para actualizar el stock de productos y los precios de los productos. Por último, incluye una función para calcular el descuento de un producto.