```
#!/bin/bash

# Configuración inicial

# Definir variables de entorno
export LC_ALL=es_ES.UTF-8
export LANG=es_ES.UTF-8

# Cargar módulos necesarios
module load python/3.8.5
module load numpy

# Crear directorio de trabajo
mkdir /tmp/mi_proyecto

# Descargar datos

# Descargar datos de ejemplo
wget https://raw.githubusercontent.com/jupyter/datasets/master/weatherAUS.csv -O /tmp/mi_proyecto/weatherAUS.csv

# Descargar bibliotecas adicionales

# Instalar la biblioteca pandas
pip install pandas

# Instalar la biblioteca matplotlib
pip install matplotlib

# Preprocesamiento de datos

# Importar las bibliotecas necesarias
import pandas as pd
import numpy as np

# Leer los datos del archivo CSV
datos = pd.read_csv('/tmp/mi_proyecto/weatherAUS.csv')

# Limpiar los datos
datos = datos.dropna()

# Crear nuevas columnas

# Crear una columna con la temperatura media
datos['temperatura_media'] = datos['Temp3pm'] + datos['Temp9am'] / 2

# Crear una columna con la presión atmosférica media
datos['presion_atmosferica_media'] = datos['Pressure9am'] + datos['Pressure3pm'] / 2

# Crear una columna con la humedad relativa media
datos['humedad_relativa_media'] = datos['Humidity9am'] + datos['Humidity3pm'] / 2

# Transformar los datos

# Convertir la columna 'Date' a formato fecha
datos['Date'] = pd.to_datetime(datos['Date'])

# Crear una columna con el año
datos['año'] = datos['Date'].dt.year

# Crear una columna con el mes
datos['mes'] = datos['Date'].dt.month

# Crear una columna con el día
datos['día'] = datos['Date'].dt.day

# Análisis de datos

# Agrupar los datos por año
datos_agrupados_por_año = datos.groupby('año')

# Calcular la temperatura media por año
temperatura_media_por_año = datos_agrupados_por_año['temperatura_media'].mean()

# Calcular la presión atmosférica media por año
presion_atmosferica_media_por_año = datos_agrupados_por_año['presion_atmosferica_media'].mean()

# Calcular la humedad relativa media por año
humedad_relativa_media_por_año = datos_agrupados_por_año['humedad_relativa_media'].mean()

# Generar gráficos

# Crear un gráfico de barras con la temperatura media por año
plt.bar(temperatura_media_por_año.index, temperatura_media_por_año.values)
plt.xlabel('Año')
plt.ylabel('Temperatura media (°C)')
plt.title('Temperatura media por año')
plt.show()

# Crear un gráfico de barras con la presión atmosférica media por año
plt.bar(presion_atmosferica_media_por_año.index, presion_atmosferica_media_por_año.values)
plt.xlabel('Año')
plt.ylabel('Presión atmosférica media (hPa)')
plt.title('Presión atmosférica media por año')
plt.show()

# Crear un gráfico de barras con la humedad relativa media por año
plt.bar(humedad_relativa_media_por_año.index, humedad_relativa_media_por_año.values)
plt.xlabel('Año')
plt.ylabel('Humedad relativa media (%)')
plt.title('Humedad relativa media por año')
plt.show()

# Guardar los resultados

# Guardar los datos preprocesados en un archivo CSV
datos.to_csv('/tmp/mi_proyecto/datos_preprocesados.csv', index=False)

# Guardar los gráficos en formato PNG
plt.savefig('/tmp/mi_proyecto/temperatura_media_por_año.png')
plt.savefig('/tmp/mi_proyecto/presion_atmosferica_media_por_año.png')
plt.savefig('/tmp/mi_proyecto/humedad_relativa_media_por_año.png')

# Enviar una notificación por correo electrónico

# Importar la biblioteca smtplib
import smtplib

# Configurar los parámetros del servidor de correo electrónico
smtp_server = 'smtp.gmail.com'
smtp_port = 587

# Crear una conexión segura con el servidor de correo electrónico
smtp_connection = smtplib.SMTP(smtp_server, smtp_port)
smtp_connection.starttls()

# Iniciar sesión en el servidor de correo electrónico
smtp_connection.login('mi_correo_electronico', 'mi_contraseña')

# Crear el mensaje de correo electrónico
mensaje = """Asunto: Resultados del análisis de datos meteorológicos

Estimados señores,

Adjunto les envío los resultados del análisis de datos meteorológicos que me solicitaron.

Atentamente,

[Su nombre]"""

# Enviar el mensaje de correo electrónico
smtp_connection.sendmail('mi_correo_electronico', 'destinatario@dominio.com', mensaje)

# Cerrar la conexión con el servidor de correo electrónico
smtp_connection.quit()

```

Este código realiza un análisis de datos meteorológicos utilizando el conjunto de datos weatherAUS.csv. El código se divide en varias secciones:

* **Configuración inicial:**

    * Se configuran las variables de entorno necesarias para que el código se ejecute correctamente.
    * Se cargan los módulos necesarios para el análisis de datos.
    * Se crea un directorio de trabajo para almacenar los resultados del análisis.

* **Descarga de datos:**

    * Se descargan los datos de ejemplo del conjunto de datos weatherAUS.csv.

* **Descarga de bibliotecas adicionales:**

    * Se instalan las bibliotecas pandas y matplotlib, que son necesarias para el análisis y la visualización de datos.

* **Preprocesamiento de datos:**

    * Se importan las bibliotecas necesarias.
    * Se leen los datos del archivo CSV.
    * Se limpian los datos, eliminando las filas con valores faltantes.
    * Se crean nuevas columnas con la temperatura media, la presión atmosférica media y la humedad relativa media.
    * Se transforman los datos, convirtiendo la columna 'Date' a formato fecha y creando columnas con el año, el mes y el día.

* **Análisis de datos:**

    * Se agrupan los datos por año.
    * Se calculan la temperatura media, la presión atmosférica media y la humedad relativa media por año.

* **Generación de gráficos:**

    * Se crean gráficos de barras con la temperatura media, la presión atmosférica media y la humedad relativa media por año.

* **Guardar los resultados:**

    * Se guardan los datos preprocesados en un archivo CSV.
    * Se guardan los gráficos en formato PNG.

* **Enviar una notificación por correo electrónico:**

    * Se importa la biblioteca smtplib.
    * Se configuran los parámetros del servidor de correo electrónico.
    * Se crea una conexión segura con el servidor de correo electrónico.
    * Se inicia sesión en el servidor de correo electrónico.
    * Se crea el mensaje de correo electrónico.
    * Se envía el mensaje de correo electrónico.
    * Se cierra la conexión con el servidor de correo electrónico.