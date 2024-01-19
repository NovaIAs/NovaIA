```sql
-- Crear una tabla temporal para almacenar datos intermedios
CREATE TEMP TABLE TablaIntermedia (
    IdProducto INT NOT NULL,
    Precio FLOAT NOT NULL,
    Cantidad INT NOT NULL,
    Total FLOAT NOT NULL
);

-- Insertar datos en la tabla temporal
INSERT INTO TablaIntermedia (IdProducto, Precio, Cantidad, Total)
SELECT P.IdProducto, P.Precio, V.Cantidad, P.Precio * V.Cantidad
FROM Productos P
INNER JOIN Ventas V ON P.IdProducto = V.IdProducto;

-- Crear una tabla temporal para almacenar los productos más vendidos
CREATE TEMP TABLE ProductosMasVendidos (
    IdProducto INT NOT NULL,
    NombreProducto VARCHAR(255) NOT NULL,
    TotalVendido FLOAT NOT NULL
);

-- Obtener los productos más vendidos
INSERT INTO ProductosMasVendidos (IdProducto, NombreProducto, TotalVendido)
SELECT TI.IdProducto, P.NombreProducto, SUM(TI.Total)
FROM TablaIntermedia TI
INNER JOIN Productos P ON TI.IdProducto = P.IdProducto
GROUP BY TI.IdProducto, P.NombreProducto
ORDER BY SUM(TI.Total) DESC;

-- Crear una tabla temporal para almacenar los productos menos vendidos
CREATE TEMP TABLE ProductosMenosVendidos (
    IdProducto INT NOT NULL,
    NombreProducto VARCHAR(255) NOT NULL,
    TotalVendido FLOAT NOT NULL
);

-- Obtener los productos menos vendidos
INSERT INTO ProductosMenosVendidos (IdProducto, NombreProducto, TotalVendido)
SELECT TI.IdProducto, P.NombreProducto, SUM(TI.Total)
FROM TablaIntermedia TI
INNER JOIN Productos P ON TI.IdProducto = P.IdProducto
GROUP BY TI.IdProducto, P.NombreProducto
ORDER BY SUM(TI.Total) ASC;

-- Obtener el total de ventas por producto
SELECT TI.IdProducto, P.NombreProducto, SUM(TI.Total) AS TotalVendido
FROM TablaIntermedia TI
INNER JOIN Productos P ON TI.IdProducto = P.IdProducto
GROUP BY TI.IdProducto, P.NombreProducto;

-- Obtener el promedio de ventas por producto
SELECT TI.IdProducto, P.NombreProducto, AVG(TI.Total) AS PromedioVendido
FROM TablaIntermedia TI
INNER JOIN Productos P ON TI.IdProducto = P.IdProducto
GROUP BY TI.IdProducto, P.NombreProducto;

-- Obtener la desviación estándar de las ventas por producto
SELECT TI.IdProducto, P.NombreProducto, STDDEV(TI.Total) AS DesviacionEstandarVendido
FROM TablaIntermedia TI
INNER JOIN Productos P ON TI.IdProducto = P.IdProducto
GROUP BY TI.IdProducto, P.NombreProducto;

-- Mostrar los resultados
SELECT * FROM ProductosMasVendidos;
SELECT * FROM ProductosMenosVendidos;
SELECT * FROM TablaIntermedia;
```

Explicación:

1. **Crear tablas temporales:** Se crean tres tablas temporales para almacenar datos intermedios y resultados.

2. **Insertar datos en las tablas temporales:** Se insertan datos en las tablas temporales desde las tablas `Productos` y `Ventas`.

3. **Obtener los productos más vendidos:** Se obtiene una lista de los productos más vendidos calculando el total vendido para cada producto y ordenando los resultados en orden descendente.

4. **Obtener los productos menos vendidos:** Se obtiene una lista de los productos menos vendidos calculando el total vendido para cada producto y ordenando los resultados en orden ascendente.

5. **Obtener el total de ventas por producto:** Se calcula el total de ventas para cada producto sumando el total vendido para cada venta.

6. **Obtener el promedio de ventas por producto:** Se calcula el promedio de ventas para cada producto calculando el promedio del total vendido para cada venta.

7. **Obtener la desviación estándar de las ventas por producto:** Se calcula la desviación estándar de las ventas para cada producto calculando la desviación estándar del total vendido para cada venta.

8. **Mostrar los resultados:** Se muestran los resultados de las consultas anteriores.