```sql
WITH DatosOriginales AS (
    SELECT
        id_cliente,
        nombre_cliente,
        correo_cliente,
        fecha_nacimiento_cliente,
        direccion_cliente,
        telefono_cliente
    FROM
        clientes
),
ComprasCliente AS (
    SELECT
        id_cliente,
        id_producto,
        cantidad_comprada,
        fecha_compra
    FROM
        compras
),
ProductosVendidos AS (
    SELECT
        id_producto,
        nombre_producto,
        precio_producto,
        categoria_producto
    FROM
        productos
),
MejoresClientes AS (
    SELECT
        id_cliente,
        SUM(cantidad_comprada) AS total_comprado
    FROM
        ComprasCliente
    GROUP BY
        id_cliente
    ORDER BY
        total_comprado DESC
    LIMIT 10
),
ProductosMasVendidos AS (
    SELECT
        id_producto,
        SUM(cantidad_comprada) AS total_vendido
    FROM
        ComprasCliente
    GROUP BY
        id_producto
    ORDER BY
        total_vendido DESC
    LIMIT 10
),
ClientesCompradores AS (
    SELECT
        DatosOriginales.id_cliente,
        DatosOriginales.nombre_cliente,
        DatosOriginales.correo_cliente,
        DatosOriginales.fecha_nacimiento_cliente,
        DatosOriginales.direccion_cliente,
        DatosOriginales.telefono_cliente,
        ComprasCliente.id_producto,
        ProductosVendidos.nombre_producto,
        ProductosVendidos.precio_producto,
        ProductosVendidos.categoria_producto,
        ComprasCliente.cantidad_comprada,
        ComprasCliente.fecha_compra
    FROM
        DatosOriginales
    INNER JOIN
        ComprasCliente ON DatosOriginales.id_cliente = ComprasCliente.id_cliente
    INNER JOIN
        ProductosVendidos ON ComprasCliente.id_producto = ProductosVendidos.id_producto
    WHERE
        DatosOriginales.id_cliente IN (SELECT id_cliente FROM MejoresClientes)
    ORDER BY
        DatosOriginales.id_cliente, ComprasCliente.id_producto
)
SELECT
    ClientesCompradores.id_cliente,
    ClientesCompradores.nombre_cliente,
    ClientesCompradores.correo_cliente,
    ClientesCompradores.fecha_nacimiento_cliente,
    ClientesCompradores.direccion_cliente,
    ClientesCompradores.telefono_cliente,
    ClientesCompradores.id_producto,
    ClientesCompradores.nombre_producto,
    ClientesCompradores.precio_producto,
    ClientesCompradores.categoria_producto,
    ClientesCompradores.cantidad_comprada,
    ClientesCompradores.fecha_compra
FROM
    ClientesCompradores
WHERE
    ClientesCompradores.id_producto IN (SELECT id_producto FROM ProductosMasVendidos);
```

Este código SQL realiza un análisis detallado de los datos de los clientes, sus compras y los productos vendidos en una tienda. Se enfoca en identificar a los mejores clientes y los productos más vendidos, y luego combina esta información para proporcionar una vista detallada de los clientes que han comprado los productos más populares.

A continuación, se desglosa el código paso a paso:

**DatosOriginales:**

- Esta subconsulta selecciona información básica de los clientes de la tabla "clientes", incluyendo su ID, nombre, correo electrónico, fecha de nacimiento, dirección y teléfono.

**ComprasCliente:**

- Esta subconsulta selecciona datos relacionados con las compras de los clientes de la tabla "compras", incluyendo el ID del cliente, el ID del producto, la cantidad comprada y la fecha de compra.

**ProductosVendidos:**

- Esta subconsulta selecciona información sobre los productos vendidos de la tabla "productos", incluyendo el ID del producto, el nombre del producto, el precio del producto y la categoría del producto.

**MejoresClientes:**

- Esta subconsulta identifica a los 10 mejores clientes con base en el total de compras. Calcula el total de compras para cada cliente y luego ordena los resultados en orden descendente.

**ProductosMasVendidos:**

- Esta subconsulta identifica los 10 productos más vendidos con base en la cantidad total vendida. Calcula el total de ventas para cada producto y luego ordena los resultados en orden descendente.

**ClientesCompradores:**

- Esta subconsulta combina la información de las subconsultas anteriores para obtener una vista detallada de los mejores clientes y los productos más vendidos. Esto incluye información sobre el cliente, como su nombre, correo electrónico y dirección, así como información sobre el producto, como su nombre, precio y categoría. También incluye información sobre la compra, como la cantidad comprada y la fecha de compra.

**La consulta final:**

- Esta consulta selecciona los datos de la subconsulta "ClientesCompradores" y filtra los resultados para incluir solo aquellos productos que están entre los más vendidos. Esto proporciona una lista de los clientes que han comprado los productos más populares, junto con información detallada sobre sus compras.