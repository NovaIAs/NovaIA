```julia

# Importación de librerías
using Plots, DataFrames, Statistics

# Creación de un DataFrame
datos = DataFrame(nombre = ["Juan", "María", "Pedro", "Ana"],
                  edad = [20, 25, 30, 35],
                  sexo = ["Masculino", "Femenino", "Masculino", "Femenino"])

println(datos)

# Filtrado de datos
datos_filtrados = filter(x -> x.edad > 25, datos)

println(datos_filtrados)

# Agrupación de datos
datos_agrupados = groupby(datos, :sexo)

println(datos_agrupados)

# Cálculo de estadísticas
media_edad = mean(datos.edad)

maximum = maximum(datos.edad)

print("Media de edad:", media_edad)
print("Edad máxima:", maximum)

# Creación de un gráfico
plot(datos.edad, datos.nombre, title="Edades por Nombre",
     xlabel="Edad", ylabel="Nombre")

```

Explicación del código:

1. Importación de librerías: Se importan las librerías necesarias para el análisis de datos y la creación de gráficos.

2. Creación de un DataFrame: Se crea un DataFrame llamado "datos" que contiene información sobre nombre, edad y sexo de cuatro personas.

3. Filtrado de datos: Se filtra el DataFrame "datos" para obtener solo las filas donde la edad es mayor de 25 años. El resultado se guarda en la variable "datos_filtrados".

4. Agrupación de datos: Se agrupa el DataFrame "datos" por sexo. El resultado se guarda en la variable "datos_agrupados".

5. Cálculo de estadísticas: Se calculan la media y el máximo de la edad en el DataFrame "datos". Los resultados se guardan en las variables "media_edad" y "maximum", respectivamente.

6. Creación de un gráfico: Se crea un gráfico de dispersión que muestra la relación entre la edad y el nombre de las personas en el DataFrame "datos". El título del gráfico es "Edades por Nombre", el eje X se llama "Edad" y el eje Y se llama "Nombre".