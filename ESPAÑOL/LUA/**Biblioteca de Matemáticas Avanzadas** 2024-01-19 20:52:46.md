```lua
-- Módulo de Matemáticas Avanzadas

-- Calcular el factorial de un número
function factorial(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n-1)
    end
end

-- Encontrar los divisores de un número
function divisores(n)
    local divisores = {}
    for i = 1, n do
        if n % i == 0 then
            divisores[#divisores + 1] = i
        end
    end
    return divisores
end

-- Comprobar si un número es primo
function esPrimo(n)
    if n <= 1 then
        return false
    end

    for i = 2, math.sqrt(n) do
        if n % i == 0 then
            return false
        end
    end

    return true
end

-- Encontrar el máximo común divisor de dos números
function mcd(a, b)
    if b == 0 then
        return a
    else
        return mcd(b, a % b)
    end
end

-- Encontrar el mínimo común múltiplo de dos números
function mcm(a, b)
    return (a * b) / mcd(a, b)
end

-- Generar una lista de números primos menores que un número dado
function primos(n)
    local primos = {}
    for i = 2, n do
        if esPrimo(i) then
            primos[#primos + 1] = i
        end
    end
    return primos
end

-- Calcular la sucesión de Fibonacci
function fibonacci(n)
    if n <= 1 then
        return n
    else
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

--Resolver ecuaciones cuadráticas
function resolverCuadratica(a, b, c)
    local discriminante = b^2 - 4*a*c
    if discriminante < 0 then
        return "No tiene soluciones reales"
    elseif discriminante == 0 then
        return (-b) / (2*a)
    else
        local x1 = (-b + math.sqrt(discriminante)) / (2*a)
        local x2 = (-b - math.sqrt(discriminante)) / (2*a)
        return {x1, x2}
    end
end

--Aplicar el método de Newton-Raphson para encontrar raíces de funciones
function newtonRaphson(f, df, x0, tolerancia)
    local x1 = x0
    while math.abs(f(x1)) > tolerancia do
        x1 = x1 - f(x1) / df(x1)
    end
    return x1
end

--Imprimir una tabla en formato bonito
function imprimirTabla(tabla)
    for clave, valor in pairs(tabla) do
        print(clave, "=", valor)
    end
end

--Ordenar una tabla por sus valores
function ordenarPorValores(tabla)
    local tablaOrdenada = {}
    for clave, valor in pairs(tabla) do
        tablaOrdenada[clave] = valor
    end
    table.sort(tablaOrdenada, function(a, b) return a < b end)
    return tablaOrdenada
end

--Exportar una tabla a un archivo CSV
function exportarCSV(tabla, nombreArchivo)
    local archivo = io.open(nombreArchivo, "w")
    for clave, valor in pairs(tabla) do
        archivo:write(clave .. "," .. valor .. "\n")
    end
    archivo:close()
end

--Importar una tabla desde un archivo CSV
function importarCSV(nombreArchivo)
    local tabla = {}
    local archivo = io.open(nombreArchivo, "r")
    local lineas = archivo:readAll():split("\n")
    for i, linea in ipairs(lineas) do
        local clave, valor = linea:split(",")
        tabla[clave] = valor
    end
    archivo:close()
    return tabla
end

--Crear una gráfica de barras
function crearGraficaBarras(datos, etiquetas, titulo, nombreArchivo)
    local grafica = {}
    grafica.datos = datos
    grafica.etiquetas = etiquetas
    grafica.titulo = titulo
    local archivo = io.open(nombreArchivo, "w")
    archivo:write("<html><body><h1>" .. titulo .. "</h1><svg width=\"600\" height=\"400\">")
    for i, dato in ipairs(datos) do
        archivo:write("<rect x=\"" .. (i-1)*50 + 50 .. "\" y=\"" .. (400-dato) .. "\" width=\"40\" height=\"" .. dato .. "\" fill=\"blue\" />")
    end
    archivo:write("<text x=\"50\" y=\"420\" font-size=\"12\">" .. etiquetas[1] .. "</text>")
    archivo:write("<text x=\"150\" y=\"420\" font-size=\"12\">" .. etiquetas[2] .. "</text>")
    archivo:write("<text x=\"250\" y=\"420\" font-size=\"12\">" .. etiquetas[3] .. "</text>")
    archivo:write("<text x=\"350\" y=\"420\" font-size=\"12\">" .. etiquetas[4] .. "</text>")
    archivo:write("<text x=\"450\" y=\"420\" font-size=\"12\">" .. etiquetas[5] .. "</text>")
    archivo:write("</svg></body></html>")
    archivo:close()
end

--Crear una gráfica de líneas
function crearGraficaLineas(datos, etiquetas, titulo, nombreArchivo)
    local grafica = {}
    grafica.datos = datos
    grafica.etiquetas = etiquetas
    grafica.titulo = titulo
    local archivo = io.open(nombreArchivo, "w")
    archivo:write("<html><body><h1>" .. titulo .. "</h1><svg width=\"600\" height=\"400\">")
    local puntos = ""
    for i, dato in ipairs(datos) do
        puntos = puntos .. (i-1)*50 + 50 .. "," .. (400-dato) .. " "
    end
    archivo:write("<polyline points=\"" .. puntos .. "\" stroke=\"blue\" stroke-width=\"2\" fill=\"none\" />")
    archivo:write("<text x=\"50\" y=\"420\" font-size=\"12\">" .. etiquetas[1] .. "</text>")
    archivo:write("<text x=\"150\" y=\"420\" font-size=\"12\">" .. etiquetas[2] .. "</text>")
    archivo:write("<text x=\"250\" y=\"420\" font-size=\"12\">" .. etiquetas[3] .. "</text>")
    archivo:write("<text x=\"350\" y=\"420\" font-size=\"12\">" .. etiquetas[4] .. "</text>")
    archivo:write("<text x=\"450\" y=\"420\" font-size=\"12\">" .. etiquetas[5] .. "</text>")
    archivo:write("</svg></body></html>")
    archivo:close()
end

--Crear una gráfica de tarta
function crearGraficaTarta(datos, etiquetas, titulo, nombreArchivo)
    local grafica = {}
    grafica.datos = datos
    grafica.etiquetas = etiquetas
    grafica.titulo = titulo
    local archivo = io.open(nombreArchivo, "w")