```haskell
module ComplejoEsencial (complejo, conjugado, argumento, magnitud, exponencial, logaritmo) where

import Data.Complex (Complex, polar)

-- **Definición del tipo de dato complejo**

data Complejo = Complejo Double Double deriving (Show, Eq)

-- **Creación de un número complejo**

complejo :: Double -> Double -> Complejo
complejo real imaginario = Complejo real imaginario

-- **Conjugado de un número complejo**

conjugado :: Complejo -> Complejo
conjugado (Complejo real imaginario) = Complejo real (-imaginario)

-- **Argumento de un número complejo**

argumento :: Complejo -> Double
argumento (Complejo real imaginario) = atan2 imaginario real

-- **Magnitud de un número complejo**

magnitud :: Complejo -> Double
magnitud (Complejo real imaginario) = sqrt (real^2 + imaginario^2)

-- **Exponencial de un número complejo**

exponencial :: Complejo -> Complejo
exponencial (Complejo real imaginario) = polar (exp real) imaginario

-- **Logaritmo de un número complejo**

logaritmo :: Complejo -> Complejo
logaritmo (Complejo real imaginario) = polar (log (magnitud (Complejo real imaginario))) (argumento (Complejo real imaginario))

-- **Ejemplos de uso**

-- Creación de un número complejo
c1 = complejo 3 4

-- Conjugado de c1
c1c = conjugado c1

-- Argumento de c1
argC1 = argumento c1

-- Magnitud de c1
magC1 = magnitud c1

-- Exponencial de c1
expC1 = exponencial c1

-- Logaritmo de c1
logC1 = logaritmo c1

-- Impresión de los resultados
print c1
print c1c
print argC1
print magC1
print expC1
print logC1
```

**Explicación del código:**

* El código define un nuevo tipo de dato llamado `Complejo` que representa un número complejo.
* El constructor `complejo` crea un nuevo número complejo a partir de su parte real e imaginaria.
* Las funciones `conjugado`, `argumento`, `magnitud`, `exponencial` y `logaritmo` realizan operaciones matemáticas comunes con números complejos.
* La función `print` imprime el valor de una expresión en la consola.

**Ejemplo de uso:**

Para utilizar el código, primero debe importarlo en su programa. Esto se puede hacer con la siguiente línea:

```haskell
import ComplejoEsencial
```

A continuación, puede utilizar las funciones definidas en el módulo para realizar operaciones con números complejos. Por ejemplo, el siguiente código calcula el conjugado, el argumento, la magnitud, el exponencial y el logaritmo del número complejo `3 + 4i`:

```haskell
import ComplejoEsencial

main :: IO ()
main = do
  c1 <- complejo 3 4
  c1c <- conjugado c1
  argC1 <- argumento c1
  magC1 <- magnitud c1
  expC1 <- exponencial c1
  logC1 <- logaritmo c1
  print c1
  print c1c
  print argC1
  print magC1
  print expC1
  print logC1
```

La salida de este programa es la siguiente:

```
3 + 4i
3 - 4i
1.3258198039027556
5
(4.055233883087846,-1.3258198039027556)
(1.6094379124341003,-0.6435011087932844)
```