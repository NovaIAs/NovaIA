**Objetivo:** Obtener un listado detallado de la información de clientes, pedidos, productos y pedidos pendientes.

```sql
WITH ClientePedidoProducto AS (
    SELECT
        c.ClienteID,
        c.Nombre AS NombreCliente,
        c.Email,
        p.PedidoID,
        p.FechaPedido,
        pr.ProductoID,
        pr.Nombre AS NombreProducto,
        pr.Precio,
        ped.Cantidad
    FROM
        Clientes c
    JOIN
        Pedidos p ON c.ClienteID = p.ClienteID
    JOIN
        PedidosProductos ped ON p.PedidoID = ped.PedidoID
    JOIN
        Productos pr ON ped.ProductoID = pr.ProductoID
),

PedidosPendientes AS (
    SELECT
        ClienteID,
        NombreCliente,
        Email,
        FechaPedido,
        PedidoID,
        GROUP_CONCAT(NombreProducto) AS ProductosPendientes
    FROM
        ClientePedidoProducto
    WHERE
        Cantidad > 1
    GROUP BY
        ClienteID,
        NombreCliente,
        Email,
        FechaPedido,
        PedidoID
)

SELECT
    ClienteID,
    NombreCliente,
    Email,
    PedidosPendientes.FechaPedido,
    PedidosPendientes.PedidoID,
    ProductosPendientes,
    SUM(Precio * Cantidad) AS TotalPedido
FROM
    ClientePedidoProducto
JOIN
    PedidosPendientes ON ClientePedidoProducto.ClienteID = PedidosPendientes.ClienteID
    AND ClientePedidoProducto.FechaPedido = PedidosPendientes.FechaPedido
    AND ClientePedidoProducto.PedidoID = PedidosPendientes.PedidoID
GROUP BY
    ClienteID,
    NombreCliente,
    Email,
    PedidosPendientes.FechaPedido,
    PedidosPendientes.PedidoID,
    ProductosPendientes;
```

**Explicación:**

1. **ClientePedidoProducto:** Esta subconsulta une las tablas `Clientes`, `Pedidos`, `PedidosProductos` y `Productos` para obtener una vista que incluye los datos de clientes, pedidos, productos y cantidades pedidas.

2. **PedidosPendientes:** Esta subconsulta filtra los registros de `ClientePedidoProducto` para identificar los pedidos con más de un artículo en la cantidad pedida. También agrega los nombres de los productos pendientes para cada pedido utilizando `GROUP_CONCAT()`.

3. **Consulta principal:** La consulta principal une `ClientePedidoProducto` y `PedidosPendientes` en base a los campos `ClienteID`, `FechaPedido` y `PedidoID` para obtener los pedidos pendientes para cada cliente. Además, calcula el total de cada pedido multiplicando el precio de cada producto por la cantidad pedida.

Este código proporciona una visión detallada de los clientes, pedidos, productos y pedidos pendientes en un solo resultado.