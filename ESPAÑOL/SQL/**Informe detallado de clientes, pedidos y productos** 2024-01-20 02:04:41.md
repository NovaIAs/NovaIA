```sql
-- Crear una tabla temporal con los datos de los clientes y sus pedidos
CREATE TEMP TABLE ClientesPedidos AS
SELECT
    c.idCliente,
    c.nombreCliente,
    c.emailCliente,
    p.idPedido,
    p.fechaPedido,
    p.totalPedido
FROM
    Clientes c
JOIN
    Pedidos p ON c.idCliente = p.idCliente;

-- Crear una tabla temporal con los detalles de los pedidos
CREATE TEMP TABLE DetallesPedidos AS
SELECT
    p.idPedido,
    d.idProducto,
    d.nombreProducto,
    d.cantidadProducto,
    d.precioProducto
FROM
    Pedidos p
JOIN
    DetallesPedido d ON p.idPedido = d.idPedido;

-- Obtener un informe detallado de los clientes, pedidos y productos
SELECT
    cp.idCliente,
    cp.nombreCliente,
    cp.emailCliente,
    cp.idPedido,
    cp.fechaPedido,
    cp.totalPedido,
    dp.idProducto,
    dp.nombreProducto,
    dp.cantidadProducto,
    dp.precioProducto
FROM
    ClientesPedidos cp
JOIN
    DetallesPedidos dp ON cp.idPedido = dp.idPedido
ORDER BY
    cp.idCliente, cp.idPedido;

-- Eliminar las tablas temporales
DROP TEMP TABLE ClientesPedidos;
DROP TEMP TABLE DetallesPedidos;
```

Explicación:

1. Creamos una tabla temporal llamada `ClientesPedidos` con los datos de los clientes y sus pedidos. Esta tabla se crea usando una consulta `JOIN` para unir las tablas `Clientes` y `Pedidos`.


2. A continuación, creamos otra tabla temporal llamada `DetallesPedidos` con los detalles de los pedidos. Esta tabla también se crea usando una consulta `JOIN` para unir las tablas `Pedidos` y `DetallesPedido`.


3. Una vez que tenemos las dos tablas temporales creadas, podemos obtener un informe detallado de los clientes, pedidos y productos usando una consulta `SELECT`. Esta consulta utiliza una cláusula `JOIN` para unir las tablas `ClientesPedidos` y `DetallesPedidos`.


4. El resultado de la consulta es un informe que muestra la información de los clientes, pedidos y productos en una sola tabla. El informe incluye los siguientes campos:

    * `idCliente`
    * `nombreCliente`
    * `emailCliente`
    * `idPedido`
    * `fechaPedido`
    * `totalPedido`
    * `idProducto`
    * `nombreProducto`
    * `cantidadProducto`
    * `precioProducto`


5. Por último, usamos la cláusula `ORDER BY` para ordenar los resultados del informe por `idCliente` y `idPedido`.


6. Después de obtener el informe, eliminamos las tablas temporales `ClientesPedidos` y `DetallesPedidos` usando la instrucción `DROP TEMP TABLE`.