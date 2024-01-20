```fortran
PROGRAMA CALCULADORA

IMPRIME("BIENVENIDO A LA CALCULADORA")
!DECLARACIÓN DE VARIABLES

REAL::NUM1, NUM2, RESULTADO

!SE SOLICITAN LOS NÚMEROS AL USUARIO

IMPRIME("INGRESA EL PRIMER NÚMERO:")
LEE(INPUT,*)NUM1
IMPRIME("INGRESA EL SEGUNDO NÚMERO:")
LEE(INPUT,*)NUM2

!SE REALIZA LA SUMA DE LOS NÚMEROS

RESULTADO = NUM1 + NUM2

!SE MUESTRA EL RESULTADO

IMPRIME("EL RESULTADO DE LA SUMA ES:", RESULTADO)

!SE SOLICITA AL USUARIO UNA OPCIÓN

IMPRIME("¿DESEA REALIZAR OTRA OPERACIÓN? (S/N)")
LEE(INPUT,*)OPCION

!SE EVALÚA LA OPCIÓN INGRESADA POR EL USUARIO

SI(OPCION EQ "S" O OPCION EQ "s")ENTONCES
  !SE REPETIRÁ EL PROGRAMA

  GOTO 10
SINO
  !SE FINALIZA EL PROGRAMA

  IMPRIME("GRACIAS POR USAR LA CALCULADORA")
  PARA

10 CONTINÚA
!FIN DEL PROGRAMA

EXPLICACIÓN DEL CÓDIGO

* PROGRAMA CALCULADORA: Esta línea indica el inicio del programa y su nombre.
* IMPRIME: Esta instrucción se utiliza para mostrar mensajes en la pantalla.
* LEE: Esta instrucción se utiliza para leer datos introducidos por el usuario.
* REAL::NUM1, NUM2, RESULTADO: Esta línea declara las variables NUM1, NUM2 y RESULTADO como números reales.
* GOTO 10: Esta instrucción hace que el programa vuelva a la línea 10, repitiendo así el programa.
* PARA: Esta instrucción finaliza el programa.
* SI(CONDICIÓN)ENTONCES: Esta instrucción evalúa una condición y ejecuta el código dentro del bloque ENTONCES si la condición es verdadera.
* SINO: Esta instrucción ejecuta el código dentro del bloque SINO si la condición del bloque SI es falsa.
* CONTINÚA: Esta instrucción continúa la ejecución del programa desde la siguiente línea de código.
```