```julia
# Función que calcula el factorial de un número.
function factorial(n)
    if n < 0
        throw(ArgumentError("El número debe ser no negativo."))
    end
    if n == 0
        return 1
    else
        return n * factorial(n-1)
    end
end

# Función que comprueba si un número es primo.
function esPrimo(n)
    if n < 2
        return false
    end
    for i = 2:floor(sqrt(n))
        if mod(n, i) == 0
            return false
        end
    end
    return true
end

# Función que calcula la media de una lista de números.
function media(lista)
    suma = 0
    for x in lista
        suma += x
    end
    return suma / length(lista)
end

# Función que calcula la desviación estándar de una lista de números.
function desviacionEstandar(lista)
    m = media(lista)
    varianza = 0
    for x in lista
        varianza += (x - m)^2
    end
    varianza /= length(lista)
    return sqrt(varianza)
end

# Función que calcula la distribución normal de una lista de números.
function distribuciónNormal(lista)
    m = media(lista)
    s = desviacionEstandar(lista)
    for x in lista
        y = (x - m) / s
        return pdf(Normal(m, s), y)
    end
end

# Función que calcula la correlación entre dos listas de números.
function correlación(lista1, lista2)
    if length(lista1) != length(lista2)
        throw(ArgumentError("Las listas deben tener la misma longitud."))
    end
    m1 = media(lista1)
    m2 = media(lista2)
    covarianza = 0
    for i = 1:length(lista1)
        covarianza += (lista1[i] - m1) * (lista2[i] - m2)
    end
    covarianza /= length(lista1)
    return covarianza / (desviacionEstandar(lista1) * desviacionEstandar(lista2))
end

# Función que calcula la regresión lineal entre dos listas de números.
function regresiónLineal(lista1, lista2)
    if length(lista1) != length(lista2)
        throw(ArgumentError("Las listas deben tener la misma longitud."))
    end
    m1 = media(lista1)
    m2 = media(lista2)
    covarianza = 0
    for i = 1:length(lista1)
        covarianza += (lista1[i] - m1) * (lista2[i] - m2)
    end
    covarianza /= length(lista1)
    b = covarianza / desviacionEstandar(lista1)^2
    a = m2 - b * m1
    return a, b
end

# Función que calcula el valor p de una prueba de hipótesis.
function valorP(t, df)
    if t < 0
        throw(ArgumentError("El valor de t debe ser no negativo."))
    end
    if df < 1
        throw(ArgumentError("Los grados de libertad deben ser mayores que 0."))
    end
    return 1 - tcdf(t, df)
end

# Función que calcula el intervalo de confianza de una media.
function intervaloConfianza(lista, nivelConfianza)
    if nivelConfianza < 0 or nivelConfianza > 1
        throw(ArgumentError("El nivel de confianza debe estar entre 0 y 1."))
    end
    m = media(lista)
    s = desviacionEstandar(lista)
    n = length(lista)
    t = tinv(nivelConfianza / 2, n-1)
    margenError = t * s / sqrt(n)
    return m - margenError, m + margenError
end

# Función que calcula el intervalo de confianza de una proporción.
function intervaloConfianzaProporción(x, n, nivelConfianza)
    if x < 0 or x > n
        throw(ArgumentError("El número de éxitos debe estar entre 0 y el tamaño de la muestra."))
    end
    if nivelConfianza < 0 or nivelConfianza > 1
        throw(ArgumentError("El nivel de confianza debe estar entre 0 y 1."))
    end
    p = x / n
    z = norminv(nivelConfianza / 2)
    margenError = z * sqrt(p * (1 - p) / n)
    return p - margenError, p + margenError
end

# Función que calcula la prueba de chi-cuadrado de independencia.
function pruebaChiCuadradoIndependencia(tablaContingencia)
    filas = size(tablaContingencia, 1)
    columnas = size(tablaContingencia, 2)
    chiCuadrado = 0
    for i = 1:filas
        for j = 1:columnas
            o = tablaContingencia[i, j]
            e = (sum(tablaContingencia[i, :]) * sum(tablaContingencia[:, j])) / sum(tablaContingencia)
            chiCuadrado += (o - e