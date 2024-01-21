```fortran
PROGRAMA PRINCIPAL

IMPLICITO NINGUNO
REAL, DIMENSION(100) :: A, B, C

! LEER DATOS

ABIERTO(10, ARCHIVO='DATOS.TXT')
LEER(10, *) A
LEER(10, *) B
CERRAR(10)

! PROCESAR DATOS

DO I = 1, 100
    C(I) = A(I) + B(I)
END DO

! ESCRIBIR RESULTADOS

ABIERTO(20, ARCHIVO='RESULTADOS.TXT')
ESCRIBIR(20, *) C
CERRAR(20)

! FIN DEL PROGRAMA

FIN PROGRAMA PRINCIPAL
```

**Explicación del código:**

* El programa principal se llama `PROGRAMA PRINCIPAL`.
* La instrucción `IMPLICITO NINGUNO` indica que no se utilizarán tipos de datos implícitos.
* La instrucción `REAL, DIMENSION(100) :: A, B, C` declara tres arrays de 100 elementos de tipo real.
* La instrucción `ABIERTO(10, ARCHIVO='DATOS.TXT')` abre el archivo `DATOS.TXT` para lectura.
* La instrucción `LEER(10, *) A` lee los datos del archivo `DATOS.TXT` y los almacena en el array `A`.
* La instrucción `LEER(10, *) B` lee los datos del archivo `DATOS.TXT` y los almacena en el array `B`.
* La instrucción `CERRAR(10)` cierra el archivo `DATOS.TXT`.
* El bucle `DO I = 1, 100` itera sobre los 100 elementos de los arrays `A`, `B` y `C`.
* La instrucción `C(I) = A(I) + B(I)` suma los elementos `A(I)` y `B(I)` y almacena el resultado en `C(I)`.
* La instrucción `ABIERTO(20, ARCHIVO='RESULTADOS.TXT')` abre el archivo `RESULTADOS.TXT` para escritura.
* La instrucción `ESCRIBIR(20, *) C` escribe los datos del array `C` en el archivo `RESULTADOS.TXT`.
* La instrucción `CERRAR(20)` cierra el archivo `RESULTADOS.TXT`.
* La instrucción `FIN PROGRAMA PRINCIPAL` finaliza el programa.