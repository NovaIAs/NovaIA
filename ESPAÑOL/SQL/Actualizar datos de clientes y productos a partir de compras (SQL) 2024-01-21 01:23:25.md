```sql
-- Crear una tabla temporal llamada "ComprasTemp" para almacenar los datos de las compras
CREATE TEMP TABLE ComprasTemp AS
SELECT
    c.ClienteID,
    c.Nombre AS NombreCliente,
    c.Email AS EmailCliente,
    p.ProductoID,
    p.Nombre AS NombreProducto,
    p.Precio AS PrecioProducto,
    COUNT(*) AS CantidadComprada
FROM
    Compras c
JOIN
    Productos p ON c.ProductoID = p.ProductoID
GROUP BY
    c.ClienteID, c.Nombre, c.Email, p.ProductoID, p.Nombre, p.Precio;

-- Crear una tabla temporal llamada "ClientesConCompras" para identificar los clientes con al menos una compra
CREATE TEMP TABLE ClientesConCompras AS
SELECT
    ClienteID
FROM
    ComprasTemp
GROUP BY
    ClienteID;

-- Actualizar la tabla "Clientes" con los datos de las compras temporales
UPDATE
    Clientes
SET
    TotalCompras = (SELECT SUM(CantidadComprada) FROM ComprasTemp WHERE ClienteID = Clientes.ClienteID),
    ÚltimaCompra = (SELECT MAX(FechaCompra) FROM Compras WHERE ClienteID = Clientes.ClienteID)
WHERE
    Clientes.ClienteID IN (SELECT ClienteID FROM ClientesConCompras);

-- Crear una tabla temporal llamada "ProductosVendidos" para contar las ventas de cada producto
CREATE TEMP TABLE ProductosVendidos AS
SELECT
    ProductoID,
    SUM(CantidadComprada) AS CantidadVendida
FROM
    ComprasTemp
GROUP BY
    ProductoID;

-- Actualizar la tabla "Productos" con los datos de las ventas temporales
UPDATE
    Productos
SET
    VentasTotales = (SELECT CantidadVendida FROM ProductosVendidos WHERE ProductoID = Productos.ProductoID)
WHERE
    Productos.ProductoID IN (SELECT ProductoID FROM ProductosVendidos);

-- Crear una tabla temporal llamada "ClientesValiosos" para identificar a los clientes con más de una compra
CREATE TEMP TABLE ClientesValiosos AS
SELECT
    ClienteID
FROM
    ComprasTemp
GROUP BY
    ClienteID
HAVING
    COUNT(*) > 1;

-- Actualizar la tabla "Clientes" con la marca de "Cliente Valioso"
UPDATE
    Clientes
SET
    EsClienteValioso = 1
WHERE
    Clientes.ClienteID IN (SELECT ClienteID FROM ClientesValiosos);

-- Mostrar los clientes valiosos con sus datos de compras
SELECT
    c.ClienteID,
    c.Nombre AS NombreCliente,
    c.Email AS EmailCliente,
    c.TotalCompras,
    c.ÚltimaCompra,
    GROUP_CONCAT(p.Nombre) AS ProductosComprados,
    GROUP_CONCAT(ct.CantidadComprada) AS CantidadesCompradas
FROM
    Clientes c
JOIN
    ComprasTemp ct ON c.ClienteID = ct.ClienteID
JOIN
    Productos p ON ct.ProductoID = p.ProductoID
WHERE
    c.EsClienteValioso = 1
GROUP BY
    c.ClienteID, c.Nombre, c.Email, c.TotalCompras, c.ÚltimaCompra;
```

Explicación:

1. Se crean dos tablas temporales, `ComprasTemp` y `ProductosVendidos`, para almacenar datos intermedios necesarios para los cálculos.
2. Se actualiza la tabla `Clientes` con el total de compras y la última fecha de compra para cada cliente.
3. Se actualiza la tabla `Productos` con el total de ventas para cada producto.
4. Se crea una tabla temporal, `ClientesValiosos`, para identificar a los clientes con más de una compra.
5. Se actualiza la tabla `Clientes` para marcar a los clientes valiosos con un indicador booleano.
6. Se realiza una consulta para mostrar los datos de los clientes valiosos, incluyendo sus compras y los productos adquiridos.