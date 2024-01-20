```sql
CREATE PROCEDURE [dbo].[AnalisisAvanzadoDeVentas]
(
    @FechaInicio DATE,
    @FechaFin DATE,
    @CategoriaID INT,
    @RegionID INT
)
AS
BEGIN
    WITH DatosDeVentas AS (
        SELECT
            p.ProductID,
            p.UnitPrice,
            s.Quantity,
            s.Discount,
            o.OrderDate,
            c.CategoryName,
            r.RegionName
        FROM Sales s
        JOIN Products p ON s.ProductID = p.ProductID
        JOIN Orders o ON s.OrderID = o.OrderID
        JOIN Categories c ON p.CategoryID = c.CategoryID
        JOIN Customers cu ON o.CustomerID = cu.CustomerID
        JOIN Region r ON cu.RegionID = r.RegionID
        WHERE
            o.OrderDate BETWEEN @FechaInicio AND @FechaFin
            AND (c.CategoryID = @CategoriaID OR @CategoriaID IS NULL)
            AND (r.RegionID = @RegionID OR @RegionID IS NULL)
    ),
    TotalDeVentasPorProducto AS (
        SELECT
            ProductID,
            SUM(UnitPrice * Quantity * (1 - Discount)) AS TotalVentas
        FROM DatosDeVentas
        GROUP BY ProductID
    ),
    ProductosMasVendidos AS (
        SELECT
            ProductID,
            TotalVentas,
            ROW_NUMBER() OVER (ORDER BY TotalVentas DESC) AS Ranking
        FROM TotalDeVentasPorProducto
    ),
    Top5ProductosMasVendidos AS (
        SELECT
            ProductID,
            TotalVentas,
            CategoryName,
            RegionName
        FROM ProductosMasVendidos
        JOIN DatosDeVentas ON ProductosMasVendidos.ProductID = DatosDeVentas.ProductID
        WHERE Ranking <= 5
    )
    SELECT
        *
    FROM Top5ProductosMasVendidos;
END
```

Explicación:

1. **DatosDeVentas**: Esta consulta crea una vista temporal llamada `DatosDeVentas` que contiene información detallada de las ventas, uniendo datos de múltiples tablas, incluidos detalles de productos, pedidos, clientes y regiones. Filtra las ventas dentro del rango de fechas especificado, por categoría y región si se proporcionan.

2. **TotalDeVentasPorProducto**: Esta consulta agrega las ventas por producto, calculando el total de ingresos para cada producto vendiendo.

3. **ProductosMasVendidos**: Agrega los totales de ventas por producto y luego usa la función `ROW_NUMBER()` para clasificar los productos en orden descendiente de ventas totales.

4. **Top5ProductosMasVendidos**: Finalmente, esta consulta selecciona los 5 productos más vendidos junto con su categoría y región.

Este código proporciona un análisis avanzado de ventas, identificando los productos más vendidos dentro de un rango de fechas específico, por categoría y región.