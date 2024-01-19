```fortran
PROGRAMA MANEJO_ARCHIVOS

! DECLARACIÓN DE VARIABLES
CHARACTER(LEN=100) :: NOMBRE_ARCHIVO
CHARACTER(LEN=100) :: LINEA
INTEGER :: I, J, K
REAL :: NUMERO

! SOLICITAR NOMBRE DEL ARCHIVO
PRINT *, "INGRESE EL NOMBRE DEL ARCHIVO A ABRIR:"
READ *, NOMBRE_ARCHIVO

! INTENTAR ABRIR EL ARCHIVO
OPEN(UNIT=10, FILE=NOMBRE_ARCHIVO, STATUS='OLD', ACTION='READ')

! LEER Y PROCESAR LÍNEAS DEL ARCHIVO
DO
  READ(10, *) LINEA

  ! DIVIDIR LA LÍNEA EN CAMPOS
  CALL SPLIT(LINEA, ',', J)

  ! GUARDAR EL PRIMER CAMPO EN LA VARIABLE I
  I = J(1)

  ! GUARDAR EL SEGUNDO CAMPO EN LA VARIABLE NUMERO
  NUMERO = J(2)

  ! REALIZAR CÁLCULOS Y OPERACIONES CON I Y NUMERO

  ! ESCRIBIR RESULTADOS EN PANTALLA
  PRINT *, I, NUMERO

  ! LEER LA SIGUIENTE LÍNEA
  READ(10, *, IOSTAT=K) LINEA
END DO

! CERRAR EL ARCHIVO
CLOSE(10)

END PROGRAMA MANEJO_ARCHIVOS

SUBROUTINE SPLIT(LINEA, DELIMITADOR, CAMPOS)

! DECLARACIÓN DE VARIABLES
CHARACTER(LEN=100) :: LINEA, DELIMITADOR
INTEGER, DIMENSION(*) :: CAMPOS

! INICIALIZAR ÍNDICE DE CAMPOS
L = 1

! RECORRER LA CADENA LINEA
DO I = 1, LEN_TRIM(LINEA)

  ! SI EL ACTUAL ES UN CARÁCTER DELIMITADOR
  IF(LINEA(I:I) == DELIMITADOR) THEN

    ! GUARDAR EL CAMPO EN EL ARRAY CAMPOS
    CAMPOS(L) = I-1

    ! AUMENTAR EL ÍNDICE DE CAMPOS
    L = L + 1

  END IF

END DO

! GUARDAR EL ÚLTIMO CAMPO EN EL ARRAY CAMPOS
CAMPOS(L) = LEN_TRIM(LINEA)

END SUBROUTINE SPLIT
```

Explicación:

* El programa principal `MANEJO_ARCHIVOS` solicita al usuario el nombre de un archivo de texto que contiene datos.

* Intenta abrir el archivo con la unidad 10 en modo de lectura.

* Utiliza un bucle `DO` para leer cada línea del archivo.

* La subrutina `SPLIT` divide cada línea en campos separados por una coma.

* El programa principal realiza cálculos y operaciones con los datos extraídos de cada línea y los muestra en pantalla.

* Finalmente, cierra el archivo y termina el programa.

La subrutina `SPLIT` funciona de la siguiente manera:

* Inicializa el índice de campos `L` a 1.

* Recorre la cadena de entrada `LINEA` carácter por carácter.

* Si el carácter actual es un delimitador (en este caso, una coma), guarda el índice del campo actual en el array `CAMPOS` e incrementa el índice de campos `L`.

* Al final del bucle, guarda el índice del último campo en el array `CAMPOS`.

Este código es complejo porque combina diferentes conceptos de programación, incluyendo la manipulación de archivos, la división de cadenas y los cálculos matemáticos. También utiliza una subrutina para dividir las líneas del archivo en campos, lo que hace que el código sea más modular y fácil de mantener.