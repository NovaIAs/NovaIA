```lua
-- Requerir módulos necesarios.
require("kalle")
require("stack")

-- Definir función mapea.
function mapea(f, xs)
    local ys = {}
    for x in xs do
        ys[#ys + 1] = f(x)
    end
    return ys
end

-- Definir función reduce.
function reduce(f, x0, xs)
    local x = x0
    for i, y in ipairs(xs) do
        x = f(x, y)
    end
    return x
end

-- Definir tipo de dato árbol.
Arbol = {}
function Arbol:new(dato, hijos)
    self.dato = dato
    self.hijos = {}
    for i, hijo in ipairs(hijos) do
        self.hijos[i] = hijo
    end
end

-- Sobrecargar función tostring para árboles.
function tostring(arbol)
    local str = "Arbol:{" .. arbol.dato
    for _, hijo in ipairs(arbol.hijos) do
        str = str .. ", " .. tostring(hijo)
    end
    str = str .. "}"
    return str
end

-- Definir función tomar-primeros para árboles.
function tomar-primeros(n, arbol)
    if arbol ~= nil then
        if n == 1 then
            return {arbol.dato}
        else
            local hijos = mapea(tomar-primeros, arbol.hijos)
            return reduce(concat, {}, hijos)
        end
    else
        return {}
    end
end

-- Definir función filtrar para árboles.
function filtrar(f, arbol)
    if arbol ~= nil then
        local datos = mapea(function(a)
            if f(a.dato) then
                return a.dato
            else
                return nil
            end
        end, arbol.hijos)
        local hijos = mapea(function(a)
            if f(a.dato) then
                return nil
            else
                return filtrar(f, a)
            end
        end, arbol.hijos)
        local ys = {}
        for i, dato in ipairs(datos) do
            if dato ~= nil then
                ys[#ys + 1] = dato
            end
        end
        local zs = {}
        for i, hijo in ipairs(hijos) do
            if hijo ~= nil then
                zs[#zs + 1] = hijo
            end
        end

        return Arbol:new(arbol.dato, zs)
    else
        return nil
    end
end

-- Definir función es-lista.
function es-lista(arbol)
    return (#arbol.hijos == 1 and arbol.hijos[1] ~= nil and arbol.hijos[1]:es-lista())
end

-- Definir transformar-en-lista para árboles.
function transformar-en-lista(arbol)
    if arbol ~= nil and es-lista(arbol) then
        return {arbol.dato} .. transformar-en-lista(arbol.hijos[1])
    elseif arbol ~= nil then
        return "(" .. arbol.dato .. " " .. transformar-en-lista(arbol.hijos[1]) .. ")" .. transformar-en-lista(arbol.hijos[2])
    else
        return ""
    end
end

-- Definir función imprimir-árbol.
function imprimir-árbol(arbol)
    if arbol ~= nil then
        print(transformar-en-lista(arbol))
    end
end

-- Definir árbol de ejemplo.
ejemplo = Arbol:new("+", {
    Arbol:new("*", {
        Arbol:new("a"),
        Arbol:new("b")
    }),
    Arbol:new("/", {
        Arbol:new("c"),
        Arbol:new("d")
    })
})

-- Imprimir árbol de ejemplo.
imprimir-árbol(ejemplo)
```

Explicación:

Este código define un tipo de dato árbol y varias funciones que operan sobre árboles.

Primero, se requieren los módulos "kalle" y "stack", que proporcionan algunas funciones básicas.

Luego, se define la función "mapea", que aplica una función dada a una lista de elementos y devuelve una nueva lista con los resultados. Por ejemplo, "mapea(function(x) return x * 2 end, {1, 2, 3})" devuelve la lista "{2, 4, 6}".

A continuación, se define la función "reduce", que reduce una lista de elementos a un valor único aplicando una función dada a dos elementos a la vez. Por ejemplo, "reduce(function(x, y) return x + y end, 0, {1, 2, 3})" devuelve el valor 6.

Luego, se define el tipo de dato árbol, que consiste en un dato y una lista de hijos. También se sobrecarga la función "tostring" para los árboles para que se impriman en un formato legible.

Después, se define la función "tomar-primeros", que toma los primeros "n" elementos de un árbol. Por ejemplo, "tomar-primeros(2, ejemplo)" devuelve el árbol "{+, {*, {a}, {b}}}".

A continuación, se define la función "filtrar", que devuelve un nuevo árbol con solo los elementos que satisfacen una condición dada. Por ejemplo, "filtrar(function(x) return x > 0 end, ejemplo)" devuelve el árbol "{+, {*, {a}, {b}}}".

Luego, se define la función "es-lista", que comprueba si un árbol es una lista. Una lista es un árbol que tiene un solo hijo y ese hijo también es una lista.

Después, se define la función "transformar-en-lista", que convierte un árbol en una lista. Por ejemplo, "transformar-en-lista(ejemplo)" devuelve la cadena "(+ (* a b) (/ c d))".

Finalmente, se define la función "imprimir-árbol", que imprime un árbol en la consola.

El código termina definiendo un árbol de ejemplo y luego llam