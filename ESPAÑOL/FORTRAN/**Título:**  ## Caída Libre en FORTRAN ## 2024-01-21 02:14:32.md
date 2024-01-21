```fortran
PROGRAMA PRINCIPAL

IMPLICIT NONE

! Constantes
REAL :: PI = 3.141592653589793
REAL :: G = 9.80665

! Variables
REAL :: m, v, h, t
CHARACTER(LEN=20) :: nombre_fichero

! Abrir fichero para escribir
OPEN(UNIT=10, FILE=nombre_fichero, STATUS='UNKNOWN')

! Pedir datos al usuario
WRITE(*, '(A,A)') 'Introduzca el nombre del fichero de salida: ', &
                     '$'
READ(*, '(A)') nombre_fichero

WRITE(*, '(A,A)') 'Introduzca la masa del objeto (kg): ', &
                     '$'
READ(*, *) m

WRITE(*, '(A,A)') 'Introduzca la velocidad inicial (m/s): ', &
                     '$'
READ(*, *) v

WRITE(*, '(A,A)') 'Introduzca la altura inicial (m): ', &
                     '$'
READ(*, *) h

! Calcular el tiempo de caída
t = SQRT(2. * h / G)

! Calcular la energía cinética inicial
Ek_inicial = 0.5 * m * v**2

! Calcular la energía potencial inicial
Ep_inicial = m * G * h

! Calcular la energía total inicial
E_inicial = Ek_inicial + Ep_inicial

! Calcular la energía cinética final
Ek_final = 0.5 * m * v**2

! Calcular la energía potencial final
Ep_final = m * G * 0.

! Calcular la energía total final
E_final = Ek_final + Ep_final

! Escribir los resultados en el fichero
WRITE(10, '(A,F10.2)') 'Masa del objeto (kg): ', m
WRITE(10, '(A,F10.2)') 'Velocidad inicial (m/s): ', v
WRITE(10, '(A,F10.2)') 'Altura inicial (m): ', h
WRITE(10, '(A,F10.2)') 'Tiempo de caída (s): ', t
WRITE(10, '(A,F10.2)') 'Energía cinética inicial (J): ', &
                     Ek_inicial
WRITE(10, '(A,F10.2)') 'Energía potencial inicial (J): ', &
                     Ep_inicial
WRITE(10, '(A,F10.2)') 'Energía total inicial (J): ', E_inicial
WRITE(10, '(A,F10.2)') 'Energía cinética final (J): ', &
                     Ek_final
WRITE(10, '(A,F10.2)') 'Energía potencial final (J): ', &
                     Ep_final
WRITE(10, '(A,F10.2)') 'Energía total final (J): ', E_final

! Cerrar el fichero
CLOSE(10)

END PROGRAM PRINCIPAL
```

**Explicación del código:**

El programa principal es un programa en FORTRAN que calcula la caída de un objeto desde una altura determinada.

El programa primero pide al usuario que introduzca el nombre del fichero de salida, la masa del objeto, la velocidad inicial y la altura inicial.

El programa calcula el tiempo de caída usando la ecuación `t = SQRT(2. * h / G)`.

El programa calcula la energía cinética inicial y potencial inicial usando las ecuaciones `Ek_inicial = 0.5 * m * v**2` y `Ep_inicial = m * G * h`, respectivamente.

El programa calcula la energía total inicial sumando la energía cinética inicial y la energía potencial inicial.

El programa calcula la energía cinética final y potencial final usando las ecuaciones `Ek_final = 0.5 * m * v**2` y `Ep_final = m * G * 0.`, respectivamente.

El programa calcula la energía total final sumando la energía cinética final y la energía potencial final.

El programa escribe los resultados en el fichero de salida.

El programa cierra el fichero de salida.