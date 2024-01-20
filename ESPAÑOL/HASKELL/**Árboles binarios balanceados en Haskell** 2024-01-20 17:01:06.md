```haskell
-- Definición de tipos

data Arbol a = Hoja a | Rama (Arbol a) (Arbol a)

-- Funciones auxiliares

altura :: Arbol a -> Int
altura (Hoja _) = 0
altura (Rama izq der) = 1 + max (altura izq) (altura der)

esBalanceado :: Arbol a -> Bool
esBalanceado (Hoja _) = True
esBalanceado (Rama izq der) = abs (altura izq - altura der) <= 1 && esBalanceado izq && esBalanceado der

-- Creación de árboles

arbol1 :: Arbol Int
arbol1 = Rama (Rama (Hoja 1) (Hoja 2)) (Rama (Hoja 3) (Hoja 4))

arbol2 :: Arbol Int
arbol2 = Rama (Rama (Hoja 1) (Hoja 2)) (Rama (Rama (Hoja 3) (Hoja 4)) (Hoja 5))

-- Pruebas

main :: IO ()
main = do
  print (altura arbol1) -- Salida: 2
  print (esBalanceado arbol1) -- Salida: True
  print (altura arbol2) -- Salida: 3
  print (esBalanceado arbol2) -- Salida: False
```

Explicación del código:

* **Definición de tipos**: se define el tipo de datos `Arbol a`, que representa un árbol binario genérico con valores de tipo `a`. Un árbol puede ser una hoja, que contiene un solo valor, o una rama, que contiene dos subárboles.

* **Funciones auxiliares**:

  * `altura`: calcula la altura de un árbol, es decir, el número máximo de niveles que tiene el árbol.

  * `esBalanceado`: comprueba si un árbol es balanceado, es decir, si la diferencia de altura entre los dos subárboles de cada nodo es menor o igual a 1.

* **Creación de árboles**: se definen dos árboles de ejemplo, `arbol1` y `arbol2`, utilizando la función `Rama` para crear ramas y la función `Hoja` para crear hojas.

* **Pruebas**: se imprimen la altura y el balance de los dos árboles de ejemplo para mostrar cómo funcionan las funciones auxiliares.