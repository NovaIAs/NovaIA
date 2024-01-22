```
PROGRAMA RUTA_OPTIMA

! Declaración de Variables

INTEGER, PARAMETER :: TAMANO_CIUDAD = 10
REAL, DIMENSION(TAMANO_CIUDAD, TAMANO_CIUDAD) :: DISTANCIA
INTEGER, DIMENSION(TAMANO_CIUDAD) :: VISITADO
REAL :: DISTANCIA_TOTAL
INTEGER :: CIUDAD_ACTUAL, CIUDAD_SIGUIENTE, CIUDAD_INICIAL

! Inicialización de Variables

DISTANCIA = 0.0
VISITADO = 0
DISTANCIA_TOTAL = 0.0
CIUDAD_INICIAL = 1

! Lectura de Datos

PRINT *, "Ingrese las distancias entre las ciudades:"
DO I = 1, TAMANO_CIUDAD
   DO J = 1, TAMANO_CIUDAD
      READ *, DISTANCIA(I, J)
   END DO
END DO

! Búsqueda de la Ruta Óptima

VISITADO(CIUDAD_INICIAL) = 1
DO I = 1, TAMANO_CIUDAD-1
   CIUDAD_SIGUIENTE = 0
   DISTANCIA_MINIMA = 1.0E30

   DO J = 1, TAMANO_CIUDAD
      IF (VISITADO(J) == 0 .AND. DISTANCIA(CIUDAD_ACTUAL, J) < DISTANCIA_MINIMA) THEN
         CIUDAD_SIGUIENTE = J
         DISTANCIA_MINIMA = DISTANCIA(CIUDAD_ACTUAL, J)
      END IF
   END DO

   DISTANCIA_TOTAL = DISTANCIA_TOTAL + DISTANCIA_MINIMA
   VISITADO(CIUDAD_SIGUIENTE) = 1
   CIUDAD_ACTUAL = CIUDAD_SIGUIENTE
END DO

DISTANCIA_TOTAL = DISTANCIA_TOTAL + DISTANCIA(CIUDAD_ACTUAL, CIUDAD_INICIAL)

! Impresión de Resultados

PRINT *, "La distancia total de la ruta óptima es: ", DISTANCIA_TOTAL

END PROGRAMA RUTA_OPTIMA
```

Este programa en FORTRAN resuelve el problema del viajante en una matriz de distancias entre ciudades. El objetivo es encontrar la ruta más corta que visita cada ciudad exactamente una vez y regresa al punto de partida. El programa utiliza un bucle para recorrer todas las ciudades y un bucle anidado para calcular la distancia entre cada par de ciudades. Una vez que se han calculado todas las distancias, el programa utiliza un algoritmo codicioso para encontrar la ruta más corta. El algoritmo codicioso comienza en una ciudad arbitraria y luego visita la ciudad más cercana que aún no se ha visitado. Este proceso continúa hasta que todas las ciudades han sido visitadas y el programa regresa al punto de partida. El programa imprime la distancia total de la ruta óptima al final.