```sql
-- Crear una función que tome una tabla y un campo y devuelva una lista única de valores del campo

CREATE FUNCTION [dbo].[ObtenerValoresUnicos]
(
    @Tabla NVARCHAR(128),
    @Campo NVARCHAR(128)
)
RETURNS TABLE
AS
RETURN (
    SELECT DISTINCT
        t.[Campo]
    FROM
        [dbo].[Tabla] AS t
);

-- Crear una vista que muestre el número de pedidos de cada cliente

CREATE VIEW [dbo].[vw_NumeroPedidosPorCliente]
AS
SELECT
    c.ClienteID,
    c.Nombre AS NombreCliente,
    COUNT(o.PedidoID) AS NumeroPedidos
FROM
    [dbo].[Clientes] AS c
LEFT JOIN
    [dbo].[Pedidos] AS o
ON
    c.ClienteID = o.ClienteID
GROUP BY
    c.ClienteID, c.Nombre;

-- Crear un procedimiento almacenado que tome un rango de fechas y devuelva las ventas totales para ese rango de fechas

CREATE PROCEDURE [dbo].[ObtenerVentasPorRangoFechas]
(
    @FechaInicio DATE,
    @FechaFin DATE
)
AS
BEGIN
    DECLARE @VentasTotales NUMERIC(18, 2);

    SELECT @VentasTotales = SUM(o.Total)
    FROM
        [dbo].[Pedidos] AS o
    WHERE
        o.FechaPedido BETWEEN @FechaInicio AND @FechaFin;

    SELECT @VentasTotales AS VentasTotales;
END;

-- Crear un disparador que actualice automáticamente el campo ÚltimaModificación de una tabla cuando se modifique un registro

CREATE TRIGGER [dbo].[trg_ActualizarUltimaModificacion]
ON
    [dbo].[Tabla]
FOR
    UPDATE
AS
BEGIN
    UPDATE
        [dbo].[Tabla]
    SET
        UltimaModificacion = GETDATE()
    WHERE
        ID = OLD.ID;
END;
```

Explicación del código:

* La función `ObtenerValoresUnicos` toma una tabla y un campo como parámetros y devuelve una tabla con una lista única de valores del campo especificado.
* La vista `vw_NumeroPedidosPorCliente` muestra el número de pedidos de cada cliente.
* El procedimiento almacenado `ObtenerVentasPorRangoFechas` toma un rango de fechas como parámetros y devuelve las ventas totales para ese rango de fechas.
* El disparador `trg_ActualizarUltimaModificacion` actualiza automáticamente el campo `ÚltimaModificación` de una tabla cuando se modifica un registro.