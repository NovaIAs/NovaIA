```shell
#!/bin/bash

# Este script automatiza la generación de informes financieros para una empresa.

# Definir variables.
EMPRESA="Ejemplo S.A."
FECHA=$(date +%d/%m/%Y)
AÑO=$(date +%Y)

# Crear el directorio de informes si no existe.
mkdir -p informes

# Generar el informe de ventas.
grep "Ventas" datos.csv > ventas.csv
csv2html --delimiter=',' ventas.csv > informes/informe_ventas.html

# Generar el informe de gastos.
grep "Gastos" datos.csv > gastos.csv
csv2html --delimiter=',' gastos.csv > informes/informe_gastos.html

# Generar el informe de ganancias.
grep "Ganancias" datos.csv > ganancias.csv
csv2html --delimiter=',' ganancias.csv > informes/informe_ganancias.html

# Generar el informe financiero general.
cat informes/informe_ventas.html informes/informe_gastos.html informes/informe_ganancias.html > informes/informe_financiero.html

# Agregar portada al informe financiero general.
echo "<html>
<head>
<title>Informe Financiero $EMPRESA - $AÑO</title>
</head>
<body>
<h1>Informe Financiero $EMPRESA - $AÑO</h1>
<p>Fecha: $FECHA</p>" > informes/portada.html

cat informes/portada.html informes/informe_financiero.html > informes/informe_financiero_completo.html

# Enviar el informe financiero completo por correo electrónico.
echo "Asunto: Informe Financiero $EMPRESA - $AÑO" | sendmail -v -t

# Eliminar los archivos temporales.
rm ventas.csv gastos.csv ganancias.csv informes/informe_ventas.html informes/informe_gastos.html informes/informe_ganancias.html informes/portada.html

# Terminar el script.
exit 0
```

Este script automatiza la generación de informes financieros para una empresa. El script realiza las siguientes tareas:

1. Define las variables necesarias, como el nombre de la empresa, la fecha y el año.
2. Crea el directorio de informes si no existe.
3. Genera el informe de ventas, el informe de gastos y el informe de ganancias.
4. Genera el informe financiero general combinando los tres informes anteriores.
5. Agrega una portada al informe financiero general.
6. Envía el informe financiero completo por correo electrónico.
7. Elimina los archivos temporales.
8. Termina el script.