```lua
-- Definir variables globales
tablaPaises = {
  ["España"] = {capital = "Madrid", poblacion = 47.431.226},
  ["México"] = {capital = "Ciudad de México", poblacion = 128.932.477},
  ["Argentina"] = {capital = "Buenos Aires", poblacion = 45.376.763},
  ["Chile"] = {capital = "Santiago de Chile", poblacion = 19.116.221}
}

-- Función para calcular el PIB per cápita de un país
function calcularPIBPerCapita(pais)
  poblacion = getPoblacionPais(pais)
  pib = getPIBPais(pais)
  return pib / poblacion
end

-- Función para obtener la población de un país
function getPoblacionPais(pais)
  return tablaPaises[pais]["poblacion"]
end

-- Función para obtener el PIB de un país
function getPIBPais(pais)
  pib = 0
  for año = 2010, 2022 do
    pibAnual = getPIBAnualPais(pais, año)
    pib = pib + pibAnual
  end
  return pib
end

-- Función para obtener el PIB anual de un país en un año determinado
function getPIBAnualPais(pais, año)
  if año == 2010 then
    return 1000000000 -- PIB de España en 2010 (valor de ejemplo)
  elseif año == 2011 then
    return 1100000000 -- PIB de España en 2011 (valor de ejemplo)
  elseif año == 2012 then
    return 1200000000 -- PIB de España en 2012 (valor de ejemplo)
  elseif año == 2013 then
    return 1300000000 -- PIB de España en 2013 (valor de ejemplo)
  elseif año == 2014 then
    return 1400000000 -- PIB de España en 2014 (valor de ejemplo)
  elseif año == 2015 then
    return 1500000000 -- PIB de España en 2015 (valor de ejemplo)
  elseif año == 2016 then
    return 1600000000 -- PIB de España en 2016 (valor de ejemplo)
  elseif año == 2017 then
    return 1700000000 -- PIB de España en 2017 (valor de ejemplo)
  elseif año == 2018 then
    return 1800000000 -- PIB de España en 2018 (valor de ejemplo)
  elseif año == 2019 then
    return 1900000000 -- PIB de España en 2019 (valor de ejemplo)
  elseif año == 2020 then
    return 2000000000 -- PIB de España en 2020 (valor de ejemplo)
  elseif año == 2021 then
    return 2100000000 -- PIB de España en 2021 (valor de ejemplo)
  elseif año == 2022 then
    return 2200000000 -- PIB de España en 2022 (valor de ejemplo)
  end
end

-- Obtener el PIB per cápita de España
pibPerCapitaEspaña = calcularPIBPerCapita("España")
print("El PIB per cápita de España es de " .. pibPerCapitaEspaña)

-- Obtener el PIB per cápita de México
pibPerCapitaMéxico = calcularPIBPerCapita("México")
print("El PIB per cápita de México es de " .. pibPerCapitaMéxico)

-- Obtener el PIB per cápita de Argentina
pibPerCapitaArgentina = calcularPIBPerCapita("Argentina")
print("El PIB per cápita de Argentina es de " .. pibPerCapitaArgentina)

-- Obtener el PIB per cápita de Chile
pibPerCapitaChile = calcularPIBPerCapita("Chile")
print("El PIB per cápita de Chile es de " .. pibPerCapitaChile)
```

Explicación del código:

* La tabla `tablaPaises` contiene información sobre cuatro países: España, México, Argentina y Chile. Cada país tiene dos campos: `capital` y `poblacion`.
* La función `calcularPIBPerCapita` se utiliza para calcular el PIB per cápita de un país dado. La función toma un país como argumento y devuelve el PIB per cápita del país.
* La función `getPoblacionPais` se utiliza para obtener la población de un país dado. La función toma un país como argumento y devuelve la población del país.
* La función `getPIBPais` se utiliza para obtener el PIB de un país dado. La función toma un país como argumento y devuelve el PIB del país.
* La función `getPIBAnualPais` se utiliza para obtener el PIB anual de un país dado en un año determinado. La función toma un país y un año como argumentos y devuelve el PIB anual del país en ese año.
* El código principal llama a las funciones `calcularPIBPerCapita` para obtener el PIB per cápita de España, México, Argentina y Chile. El código también llama a las funciones `getPoblacionPais`, `getPIBPais` y `getPIBAnualPais` para obtener información adicional sobre los países.
* El código imprime el PIB per cápita de cada país en la consola.