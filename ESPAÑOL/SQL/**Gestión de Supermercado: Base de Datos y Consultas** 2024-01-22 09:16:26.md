```
-- Creación de una base de datos llamada "Supermercado"
CREATE DATABASE Supermercado;

-- Selección de la base de datos "Supermercado"
USE Supermercado;

-- Creación de la tabla "Productos" con sus respectivas columnas
CREATE TABLE Productos (
    IdProducto INT PRIMARY KEY,
    NombreProducto VARCHAR(50) NOT NULL,
    PrecioProducto DECIMAL(10,2) NOT NULL,
    CantidadProducto INT NOT NULL,
    CategoriaProducto VARCHAR(30) NOT NULL
);

-- Inserción de algunos registros de productos en la tabla "Productos"
INSERT INTO Productos (IdProducto, NombreProducto, PrecioProducto, CantidadProducto, CategoriaProducto) VALUES
(1, 'Leche', 2.50, 10, 'Lácteos'),
(2, 'Pan', 1.20, 20, 'Panadería'),
(3, 'Huevos', 3.00, 15, 'Huevos'),
(4, 'Arroz', 4.00, 25, 'Arroz'),
(5, 'Frijoles', 3.50, 20, 'Legumbres'),
(6, 'Carne de res', 10.00, 12, 'Carnes'),
(7, 'Pollo', 8.00, 15, 'Carnes'),
(8, 'Pescado', 12.00, 10, 'Pescados y mariscos'),
(9, 'Frutas', 5.00, 25, 'Frutas'),
(10, 'Verduras', 4.50, 20, 'Verduras');

-- Creación de la tabla "Ventas" con sus respectivas columnas
CREATE TABLE Ventas (
    IdVenta INT PRIMARY KEY,
    FechaVenta DATE NOT NULL,
    TotalVenta DECIMAL(10,2) NOT NULL,
    ClienteVenta VARCHAR(50) NOT NULL
);

-- Inserción de algunos registros de ventas en la tabla "Ventas"
INSERT INTO Ventas (IdVenta, FechaVenta, TotalVenta, ClienteVenta) VALUES
(1, '2023-03-08', 150.00, 'Juan Pérez'),
(2, '2023-03-10', 200.00, 'María García'),
(3, '2023-03-12', 120.00, 'Pedro Sánchez'),
(4, '2023-03-15', 180.00, 'Ana López'),
(5, '2023-03-17', 160.00, 'José Rodríguez');

-- Creación de la tabla "Ventas_Productos" con sus respectivas columnas
CREATE TABLE Ventas_Productos (
    IdVentaProducto INT PRIMARY KEY,
    IdVenta INT NOT NULL,
    IdProducto INT NOT NULL,
    CantidadProducto INT NOT NULL
);

-- Inserción de algunos registros en la tabla "Ventas_Productos"
INSERT INTO Ventas_Productos (IdVentaProducto, IdVenta, IdProducto, CantidadProducto) VALUES
(1, 1, 1, 10),
(2, 1, 2, 5),
(3, 1, 3, 3),
(4, 2, 4, 10),
(5, 2, 5, 5),
(6, 2, 6, 2),
(7, 3, 7, 3),
(8, 3, 8, 2),
(9, 4, 9, 5),
(10, 4, 10, 3),
(11, 5, 1, 8),
(12, 5, 2, 4),
(13, 5, 3, 2);

-- Consulta para obtener el nombre de los productos y el total vendido de cada uno
SELECT p.NombreProducto, SUM(vp.CantidadProducto) AS TotalVendido
FROM Productos p
INNER JOIN Ventas_Productos vp ON p.IdProducto = vp.IdProducto
GROUP BY p.NombreProducto;

-- Consulta para obtener el nombre del cliente y el total de sus compras
SELECT v.ClienteVenta, SUM(v.TotalVenta) AS TotalCompras
FROM Ventas v
GROUP BY v.ClienteVenta;

-- Consulta para obtener el nombre del producto más vendido
SELECT p.NombreProducto
FROM Productos p
INNER JOIN Ventas_Productos vp ON p.IdProducto = vp.IdProducto
GROUP BY p.NombreProducto
ORDER BY SUM(vp.CantidadProducto) DESC
LIMIT 1;

-- Consulta para obtener el nombre del cliente que más ha comprado
SELECT v.ClienteVenta
FROM Ventas v
GROUP BY v.ClienteVenta
ORDER BY SUM(v.TotalVenta) DESC
LIMIT 1;
```

Explicación del código:

* Se crea una base de datos llamada "Supermercado".
* Se selecciona la base de datos "Supermercado".
* Se crean las tablas "Productos", "Ventas" y "Ventas_Productos" con sus respectivas columnas.
* Se insertan algunos registros de productos en la tabla "Productos".
* Se insertan algunos registros de ventas en la tabla "Ventas".
* Se insertan algunos registros en la tabla "Ventas_Productos".
* Se realizan varias consultas para obtener información sobre los productos, las ventas y los clientes.