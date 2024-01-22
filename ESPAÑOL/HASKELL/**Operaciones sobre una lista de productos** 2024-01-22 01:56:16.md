```haskell
import Data.List (groupBy, sort)
import Data.Ord (comparing)
import Prelude hiding (head, last)

-- Definición de una clase de producto.
class Producto a where
  nombre :: a -> String
  precio :: a -> Double

-- Definición de una instancia de la clase de producto para la estructura de datos `Producto`.
instance Producto Producto where
  nombre (Producto nombre precio) = nombre
  precio (Producto nombre precio) = precio

-- Definición de una función para obtener el producto más barato de una lista de productos.
productoMasBarato :: [Producto] -> Producto
productoMasBarato = head . sortOn precio

-- Definición de una función para obtener el producto más caro de una lista de productos.
productoMasCaro :: [Producto] -> Producto
productoMasCaro = last . sortOn precio

-- Definición de una función para obtener todos los productos de una lista de productos que tienen un precio mayor que un precio dado.
productosMayoresQue :: Double -> [Producto] -> [Producto]
productosMayoresQue precio = filter (\producto -> precio < precio producto)

-- Definición de una función para obtener todos los productos de una lista de productos que tienen un precio menor que un precio dado.
productosMenoresQue :: Double -> [Producto] -> [Producto]
productosMenoresQue precio = filter (\producto -> precio > precio producto)

-- Definición de una función para obtener todos los productos de una lista de productos que tienen un precio igual que un precio dado.
productosIgualesQue :: Double -> [Producto] -> [Producto]
productosIgualesQue precio = filter (\producto -> precio == precio producto)

-- Definición de una función para agrupar los productos de una lista de productos por su nombre.
productosPorNombre :: [Producto] -> [[Producto]]
productosPorNombre = groupBy (\producto1 producto2 -> nombre producto1 == nombre producto2)

-- Definición de una función para ordenar los productos de una lista de productos por su precio.
productosPorPrecio :: [Producto] -> [Producto]
productosPorPrecio = sortOn precio

-- Definición de la estructura de datos `Producto`.
data Producto = Producto {
  nombre :: String,
  precio :: Double
}

-- Definición de una lista de productos.
productos :: [Producto] = [
  Producto "Manzana" 1.50,
  Producto "Naranja" 2.00,
  Producto "Plátano" 1.25,
  Producto "Uva" 2.50,
  Producto "Pera" 1.75,
  Producto "Melón" 3.00,
  Producto "Sandía" 4.00
]

-- Impresión del producto más barato.
print (productoMasBarato productos)

-- Impresión del producto más caro.
print (productoMasCaro productos)

-- Impresión de todos los productos que tienen un precio mayor que 2.00.
print (productosMayoresQue 2.00 productos)

-- Impresión de todos los productos que tienen un precio menor que 2.00.
print (productosMenoresQue 2.00 productos)

-- Impresión de todos los productos que tienen un precio igual que 2.00.
print (productosIgualesQue 2.00 productos)

-- Impresión de todos los productos agrupados por su nombre.
print (productosPorNombre productos)

-- Impresión de todos los productos ordenados por su precio.
print (productosPorPrecio productos)
```

Explicación del código:

1. Se define una clase de producto que tiene dos métodos: `nombre` y `precio`.
2. Se define una instancia de la clase de producto para la estructura de datos `Producto`.
3. Se define una función para obtener el producto más barato de una lista de productos.
4. Se define una función para obtener el producto más caro de una lista de productos.
5. Se define una función para obtener todos los productos de una lista de productos que tienen un precio mayor que un precio dado.
6. Se define una función para obtener todos los productos de una lista de productos que tienen un precio menor que un precio dado.
7. Se define una función para obtener todos los productos de una lista de productos que tienen un precio igual que un precio dado.
8. Se define una función para agrupar los productos de una lista de productos por su nombre.
9. Se define una función para ordenar los productos de una lista de productos por su precio.
10. Se define una estructura de datos `Producto` que tiene dos campos: `nombre` y `precio`.
11. Se define una lista de productos.
12. Se imprimen el producto más barato, el producto más caro, todos los productos que tienen un precio mayor que 2.00, todos los productos que tienen un precio menor que 2.00, todos los productos que tienen un precio igual que 2.00, todos los productos agrupados por su nombre y todos los productos ordenados por su precio.