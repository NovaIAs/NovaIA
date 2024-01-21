```haskell
-- Definición del tipo de datos "ArbolBinario"
data ArbolBinario a =
    Hoja a
  | Rama (ArbolBinario a) (ArbolBinario a)

-- Función para crear un árbol binario vacío
arbolVacio :: ArbolBinario a
arbolVacio = Hoja ()

-- Función para insertar un elemento en un árbol binario
insertar :: Ord a => a -> ArbolBinario a -> ArbolBinario a
insertar x arbol =
    case arbol of
        Hoja () -> Rama (Hoja x) (Hoja ())
        Rama izquierda derecha ->
            if x <= raiz izquierda then
                Rama (insertar x izquierda) derecha
            else
                Rama izquierda (insertar x derecha)

-- Función para buscar un elemento en un árbol binario
buscar :: Ord a => a -> ArbolBinario a -> Bool
buscar x arbol =
    case arbol of
        Hoja () -> False
        Rama izquierda derecha ->
            if x == raiz izquierda || x == raiz derecha then
                True
            else
                buscar x izquierda || buscar x derecha

-- Función para eliminar un elemento de un árbol binario
eliminar :: Ord a => a -> ArbolBinario a -> ArbolBinario a
eliminar x arbol =
    case arbol of
        Hoja () -> arbolVacio
        Rama izquierda derecha ->
            if x == raiz izquierda then
                derecha
            else if x == raiz derecha then
                izquierda
            else
                Rama (eliminar x izquierda) (eliminar x derecha)

-- Función para obtener la raíz de un árbol binario
raiz :: ArbolBinario a -> a
raiz arbol =
    case arbol of
        Hoja x -> x
        Rama _ _ -> error "No se puede obtener la raíz de un árbol vacío"

-- Función para obtener el tamaño de un árbol binario
tamano :: ArbolBinario a -> Int
tamano arbol =
    case arbol of
        Hoja () -> 0
        Rama izquierda derecha -> 1 + tamano izquierda + tamano derecha


-- Función para imprimir un árbol binario en forma de cadena de caracteres
imprimirArbol :: ArbolBinario a -> String
imprimirArbol arbol =
    case arbol of
        Hoja x -> "(" ++ show x ++ ")"
        Rama izquierda derecha ->
            "(" ++ imprimirArbol izquierda ++ "," ++ imprimirArbol derecha ++ ")"

-- Función principal
main :: IO ()
main = do
    -- Crear un árbol binario
    arbol :: ArbolBinario Int
    arbol = insertar 10 (insertar 5 (insertar 2 (arbolVacio::ArbolBinario Int)))

    -- Buscar un elemento en el árbol binario
    buscar 5 arbol >>= print

    -- Eliminar un elemento del árbol binario
    eliminar 5 arbol >>= print

    -- Obtener la raíz del árbol binario
    print (raiz arbol)

    -- Obtener el tamaño del árbol binario
    print (tamano arbol)

    -- Imprimir el árbol binario en forma de cadena de caracteres
    print (imprimirArbol arbol)

```

Explicación del código:

* El código define un tipo de datos llamado "ArbolBinario" que representa un árbol binario. Un árbol binario es una estructura de datos que consta de un nodo raíz y dos subárboles, llamados rama izquierda y rama derecha.
* La definición del tipo de datos "ArbolBinario" incluye dos constructores: "Hoja" y "Rama". El constructor "Hoja" representa un árbol vacío o un árbol con un único nodo. El constructor "Rama" representa un árbol con un nodo raíz y dos subárboles.
* El código también define varias funciones para trabajar con árboles binarios, como "insertar", "buscar", "eliminar", "raiz", "tamano" e "imprimirArbol".
* La función "insertar" inserta un elemento en un árbol binario.
* La función "buscar" busca un elemento en un árbol binario.
* La función "eliminar" elimina un elemento de un árbol binario.
* La función "raiz" devuelve la raíz de un árbol binario.
* La función "tamano" devuelve el tamaño de un árbol binario.
* La función "imprimirArbol" imprime un árbol binario en forma de cadena de caracteres.
* La función "main" es la función principal del programa. Esta función crea un árbol binario, busca un elemento en el árbol, elimina un elemento del árbol, obtiene la raíz del árbol, obtiene el tamaño del árbol e imprime el árbol en forma de cadena de caracteres.