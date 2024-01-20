```lua
-- Lua Complejo: Creando un Árbol Binario de Búsqueda en Español

-- Definición del Nodo
nodo = {
    valor = nil,
    izquierda = nil,
    derecha = nil
}

-- Función para Crear un Nuevo Nodo
function crear_nodo(valor)
    nuevo_nodo = nodo({
        valor = valor,
        izquierda = nil,
        derecha = nil
    })
    return nuevo_nodo
end

-- Función para Insertar un Nodo en el Árbol
function insertar(árbol, valor)
    if árbol == nil then
        return crear_nodo(valor)
    elseif valor < árbol.valor then
        árbol.izquierda = insertar(árbol.izquierda, valor)
    elseif valor > árbol.valor then
        árbol.derecha = insertar(árbol.derecha, valor)
    else
        print("Error: El valor ya existe en el árbol.")
    end

    return árbol
end

-- Función para Buscar un Nodo en el Árbol
function buscar(árbol, valor)
    if árbol == nil then
        return nil
    elseif valor == árbol.valor then
        return árbol
    elseif valor < árbol.valor then
        return buscar(árbol.izquierda, valor)
    elseif valor > árbol.valor then
        return buscar(árbol.derecha, valor)
    else
        print("Error: El valor no existe en el árbol.")
    end

    return nil
end

-- Función para Imprimir el Árbol en Preorden
function imprimir_preorden(árbol)
    if árbol == nil then
        return
    end
    
    print("Valor: ".. árbol.valor)
    imprimir_preorden(árbol.izquierda)
    imprimir_preorden(árbol.derecha)
end

-- Función para Imprimir el Árbol en Inorden
function imprimir_inorden(árbol)
    if árbol == nil then
        return
    end

    imprimir_inorden(árbol.izquierda)
    print("Valor: ".. árbol.valor)
    imprimir_inorden(árbol.derecha)
end

-- Función para Imprimir el Árbol en Postorden
function imprimir_postorden(árbol)
    if árbol == nil then
        return
    end

    imprimir_postorden(árbol.izquierda)
    imprimir_postorden(árbol.derecha)
    print("Valor: ".. árbol.valor)
end

-- Ejemplos de Uso
árbol = nil
árbol = insertar(árbol, 50)
árbol = insertar(árbol, 30)
árbol = insertar(árbol, 70)
árbol = insertar(árbol, 20)
árbol = insertar(árbol, 40)
árbol = insertar(árbol, 60)
árbol = insertar(árbol, 80)

print("Árbol en Preorden:")
imprimir_preorden(árbol)

print("Árbol en Inorden:")
imprimir_inorden(árbol)

print("Árbol en Postorden:")
imprimir_postorden(árbol)

valor_a_buscar = 40

resultado = buscar(árbol, valor_a_buscar)

if resultado == nil then
    print("El valor", valor_a_buscar, "no existe en el árbol.")
else
    print("El valor", valor_a_buscar, "existe en el árbol.")
end
```

Explicación:

* **Nodo:** Es la estructura de datos que representa un nodo del árbol binario de búsqueda. Contiene el valor del nodo y referencias a sus hijos izquierdo y derecho.
* **Crear nodo:** Crea un nuevo nodo e inicializa sus campos con los valores especificados.
* **Insertar:** Inserta un nuevo valor en el árbol binario de búsqueda en su posición correcta.
* **Buscar:** Busca un valor en el árbol binario de búsqueda y devuelve una referencia al nodo que contiene el valor, o nil si el valor no existe.
* **Imprimir Preorden:** Imprime los valores del árbol binario de búsqueda en orden preorden (raíz, izquierda, derecha).
* **Imprimir Inorden:** Imprime los valores del árbol binario de búsqueda en orden inorden (izquierda, raíz, derecha).
* **Imprimir Postorden:** Imprime los valores del árbol binario de búsqueda en orden postorden (izquierda, derecha, raíz).

Este código demuestra la creación e impresión de un árbol binario de búsqueda en Lua. El código está documentado en español y utiliza una estructura de datos de nodo personalizada para representar los nodos del árbol. El código también incluye ejemplos de cómo insertar, buscar e imprimir el árbol.