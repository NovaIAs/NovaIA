```forth

: Crear matriz
CRIAR Numero_Filas INTEGER
CRIAR Numero_Columnas INTEGER
CRIAR Matriz MATRIZ
[ Numero_Filas ] [ Numero_Columnas ] MALLOC

: Mostrar matriz
[ Matriz ] CMDR
LOOP CMDR ;

: Modificar matriz
[ Matriz ] [ Nuevo_Valor ] ENDO

: Sumar matrices
[ Matriz1 ] [ Matriz2 ] [ Matriz_Suma ] INICIO
[ Numero_Filas ] [ Numero_Columnas ] [ Matriz_Suma ] DUPLICAR ENUMERAR DUPLICAR F!
[ Numero_Filas ] [ Numero_Columnas ] [ Matriz_Suma ] [ Matriz1 ] [ Matriz2 ] DUPLICAR ENUMERAR DUPLICAR F+!
F! ;


: Pedir numero de filas
CRIAR Entrada STRING 
CRIAR Numero_Filas INTEGER
ESCRIBIR "Introduce el número de filas de la matriz:"
LEER Entrada
TRATAR
IPD
DEJAR Numero_Filas
EXC '+entero' ALMACENAR
FIN TRATAR ;


: Pedir numero de columnas
CRIAR Entrada STRING 
CRIAR Numero_Columnas INTEGER
ESCRIBIR "Introduce el número de columnas de la matriz:"
LEER Entrada
TRATAR
IPD
DEJAR Numero_Columnas
EXC '+entero' ALMACENAR
FIN TRATAR ;

: Pedir valor para la matriz
CRIAR Entrada STRING
CRIAR Nuevo_Valor INTEGER
ESCRIBIR "Introduce un nuevo valor para la matriz:"
LEER Entrada
TRATAR
IPD
DEJAR Nuevo_Valor
EXC '+entero' ALMACENAR
FIN TRATAR ;

: Calcular media matriz
[ Matriz ] [ Numero_Filas ] [ Numero_Columnas ] [ Media ] INICIO
[ Numero_Filas ] [ Numero_Columnas ] [ Matriz ] SUMA [ Media ] F/
FIN

: Pedir número de filas y columnas
EN CASO DE PEDIR_NUMERO_FILAS Pedir numero de filas
EN CASO DE PEDIR_NUMERO_COLUMNAS Pedir numero de columnas
FIN EN CASO DE ;

: Crear matriz inicializada
CRIAR Numero_Filas INTEGER
CRIAR Numero_Columnas INTEGER
CRIAR Matriz MATRIZ
CRIAR Nuevo_Valor INTEGER
PEDIR_NUMERO_FILAS
IPD ENUMERAR 
PEDIR_NUMERO_COLUMNAS
IPD ENUMERAR
MALLOC
PEDIR_VALOR_PARA_LA_MATRIZ
IPD ENUMERAR 
ENDO
ENDO ;


: Mostrar matriz en formato de tabla
[ Matriz ] [ Numero_Filas ] [ Numero_Columnas ] INICIO 
DO 
  [ Numero_Columnas ] [ Matriz ] F@ 
  CMDR 
LOOP
CRLF ;

: Iniciar sistema
IPD 
DEJAR CRT
: 
IPD 
DEJAR LCD

IPD 
DEJAR CAL
CRT 0 USAR
LCD 16 TEXT
LCD ESCALAR 1 

PEDIR_NUMERO_FILAS 
IPD DEJAR NUM_FILAS

PEDIR_NUMERO_COLUMNAS 
IPD DEJAR NUM_COLUMNAS

CREAR_MATRIZ_INICIALIZADA 
DEJAR MATRIZ

MOSTRAR_MATRIZ_EN_FORMATO_DE_TABLA 
DEJAR MATRIZ 

CAL 60 40 0 90 0 0 0 16 255 255 255 0 0 15 ARRIBA
DEJAR LINEA

CAL 16 40 DIBUJO LINEA
CAL 16 40 ARRIBA LINEA DIBUJO ;

```

Explicación del código:

* La primera parte del código define una serie de variables y constantes que se utilizarán en el programa.
* La función `crear matriz` crea una matriz de tamaño especificado.
* La función `mostrar matriz` muestra el contenido de una matriz.
* La función `modificar matriz` modifica el contenido de una matriz.
* La función `sumar matrices` suma dos matrices y devuelve el resultado.
* La función `pedir número de filas` pide al usuario el número de filas de la matriz.
* La función `pedir número de columnas` pide al usuario el número de columnas de la matriz.
* La función `pedir valor para la matriz` pide al usuario un valor para la matriz.
* La función `calcular media matriz` calcula la media de los valores de una matriz.
* La función `pedir número de filas y columnas` llama a las funciones `pedir número de filas` y `pedir número de columnas` para obtener el número de filas y columnas de la matriz.
* La función `crear matriz inicializada` crea una matriz inicializada con los valores especificados por el usuario.
* La función `mostrar matriz en formato de tabla` muestra el contenido de una matriz en formato de tabla.
* La función `iniciar sistema` inicializa el sistema y configura el entorno de ejecución.

El programa principal crea una matriz de tamaño especificado por el usuario y luego muestra el contenido de la matriz. A continuación, el programa pide al usuario un valor para la matriz y modifica el contenido de la matriz. Finalmente, el programa muestra el contenido de la matriz modificada.