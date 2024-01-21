```sql
-- Crear una tabla temporal para almacenar datos intermedios
CREATE TEMPORARY TABLE DatosIntermedios AS
(
    SELECT
        c.ClienteID,
        c.Nombre AS NombreCliente,
        c.Ciudad AS CiudadCliente,
        p.ProductoID,
        p.Nombre AS NombreProducto,
        v.VentaID,
        v.Fecha AS FechaVenta,
        v.Cantidad AS CantidadVendida,
        v.Precio AS PrecioUnitario
    FROM
        Clientes c
    JOIN
        Productos p ON c.ClienteID = p.ClienteID
    JOIN
        Ventas v ON p.ProductoID = v.ProductoID
);

-- Calcular el total de ventas para cada cliente
CREATE TEMPORARY TABLE TotalVentasCliente AS
(
    SELECT
        ClienteID,
        SUM(CantidadVendida * PrecioUnitario) AS TotalVentas
    FROM
        DatosIntermedios
    GROUP BY
        ClienteID
);

-- Calcular el total de ventas para cada producto
CREATE TEMPORARY TABLE TotalVentasProducto AS
(
    SELECT
        ProductoID,
        SUM(CantidadVendida * PrecioUnitario) AS TotalVentas
    FROM
        DatosIntermedios
    GROUP BY
        ProductoID
);

-- Calcular el total de ventas para cada ciudad
CREATE TEMPORARY TABLE TotalVentasCiudad AS
(
    SELECT
        CiudadCliente,
        SUM(CantidadVendida * PrecioUnitario) AS TotalVentas
    FROM
        DatosIntermedios
    GROUP BY
        CiudadCliente
);

-- Calcular el total de ventas para cada mes
CREATE TEMPORARY TABLE TotalVentasMes AS
(
    SELECT
        strftime('%Y-%m', FechaVenta) AS Mes,
        SUM(CantidadVendida * PrecioUnitario) AS TotalVentas
    FROM
        DatosIntermedios
    GROUP BY
        strftime('%Y-%m', FechaVenta)
);

-- Obtener el cliente con mayor total de ventas
SELECT
    NombreCliente,
    CiudadCliente,
    TotalVentas
FROM
    TotalVentasCliente
JOIN
    Clientes c ON TotalVentasCliente.ClienteID = c.ClienteID
ORDER BY
    TotalVentas DESC
LIMIT 1;

-- Obtener el producto con mayor total de ventas
SELECT
    NombreProducto,
    TotalVentas
FROM
    TotalVentasProducto
JOIN
    Productos p ON TotalVentasProducto.ProductoID = p.ProductoID
ORDER BY
    TotalVentas DESC
LIMIT 1;

-- Obtener la ciudad con mayor total de ventas
SELECT
    CiudadCliente,
    TotalVentas
FROM
    TotalVentasCiudad
ORDER BY
    TotalVentas DESC
LIMIT 1;

-- Obtener el mes con mayor total de ventas
SELECT
    Mes,
    TotalVentas
FROM
    TotalVentasMes
ORDER BY
    TotalVentas DESC
LIMIT 1;

-- Obtener los 5 clientes con mayor total de ventas
SELECT
    NombreCliente,
    CiudadCliente,
    TotalVentas
FROM
    TotalVentasCliente
JOIN
    Clientes c ON TotalVentasCliente.ClienteID = c.ClienteID
ORDER BY
    TotalVentas DESC
LIMIT 5;

-- Obtener los 5 productos con mayor total de ventas
SELECT
    NombreProducto,
    TotalVentas
FROM
    TotalVentasProducto
JOIN
    Productos p ON TotalVentasProducto.ProductoID = p.ProductoID
ORDER BY
    TotalVentas DESC
LIMIT 5;

-- Obtener las 5 ciudades con mayor total de ventas
SELECT
    CiudadCliente,
    TotalVentas
FROM
    TotalVentasCiudad
ORDER BY
    TotalVentas DESC
LIMIT 5;

-- Obtener los 5 meses con mayor total de ventas
SELECT
    Mes,
    TotalVentas
FROM
    TotalVentasMes
ORDER BY
    TotalVentas DESC
LIMIT 5;

-- Mostrar los resultados en un formato amigable para el usuario
PRAGMA table_info(TotalVentasCliente);
PRAGMA table_info(TotalVentasProducto);
PRAGMA table_info(TotalVentasCiudad);
PRAGMA table_info(TotalVentasMes);

SELECT * FROM TotalVentasCliente;
SELECT * FROM TotalVentasProducto;
SELECT * FROM TotalVentasCiudad;
SELECT * FROM TotalVentasMes;
```

Este código SQL realiza un análisis completo de los datos de ventas de una empresa. Calcula el total de ventas para cada cliente, producto, ciudad y mes. También identifica al cliente, producto, ciudad y mes con mayor total de ventas. Además, muestra los 5 clientes, productos, ciudades y meses con mayor total de ventas. El código utiliza tablas temporales para almacenar los datos intermedios y realiza varias consultas complejas para obtener los resultados deseados.