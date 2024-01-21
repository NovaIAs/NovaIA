```fortran
PROGRAMA PRINCIPAL

! Este programa resuelve el problema de los tres cuerpos.

! Constantes físicas
REAL, PARAMETER :: G = 6.674e-11  ! Constante de gravitación universal (N·m^2/kg^2)
REAL, PARAMETER :: M1 = 5.972e24  ! Masa de la Tierra (kg)
REAL, PARAMETER :: M2 = 7.348e22  ! Masa de la Luna (kg)
REAL, PARAMETER :: M3 = 1.989e30  ! Masa del Sol (kg)

! Variables
REAL, DIMENSION(3) :: r1, v1, f1  ! Posición, velocidad y fuerza sobre la Tierra
REAL, DIMENSION(3) :: r2, v2, f2  ! Posición, velocidad y fuerza sobre la Luna
REAL, DIMENSION(3) :: r3, v3, f3  ! Posición, velocidad y fuerza sobre el Sol

! Condiciones iniciales
r1 = [1.5e8, 0.0, 0.0]  ! Posición inicial de la Tierra (m)
v1 = [0.0, 30000.0, 0.0]  ! Velocidad inicial de la Tierra (m/s)
r2 = [3.844e8, 0.0, 0.0]  ! Posición inicial de la Luna (m)
v2 = [0.0, 1022.0, 0.0]  ! Velocidad inicial de la Luna (m/s)
r3 = [0.0, 0.0, 0.0]  ! Posición inicial del Sol (m)
v3 = [0.0, 0.0, 0.0]  ! Velocidad inicial del Sol (m/s)

! Bucle temporal
DO t = 0.0, 1e8, 3600.0  ! Tiempo de simulación (s)

  ! Calcular las fuerzas sobre cada cuerpo
  f1 = -G * M2 * (r1 - r2) / NORM(r1 - r2)**3 - G * M3 * (r1 - r3) / NORM(r1 - r3)**3
  f2 = -G * M1 * (r2 - r1) / NORM(r2 - r1)**3 - G * M3 * (r2 - r3) / NORM(r2 - r3)**3
  f3 = -G * M1 * (r3 - r1) / NORM(r3 - r1)**3 - G * M2 * (r3 - r2) / NORM(r3 - r2)**3

  ! Actualizar las posiciones y velocidades de los cuerpos
  v1 = v1 + f1 * t / M1
  r1 = r1 + v1 * t
  v2 = v2 + f2 * t / M2
  r2 = r2 + v2 * t
  v3 = v3 + f3 * t / M3
  r3 = r3 + v3 * t

END DO

! Imprimir los resultados
PRINT *, 'Posición final de la Tierra:', r1
PRINT *, 'Velocidad final de la Tierra:', v1
PRINT *, 'Posición final de la Luna:', r2
PRINT *, 'Velocidad final de la Luna:', v2
PRINT *, 'Posición final del Sol:', r3
PRINT *, 'Velocidad final del Sol:', v3

END PROGRAMA PRINCIPAL
```

Explicación del código:

* El programa principal es `PROGRAMA PRINCIPAL`.
* Las constantes físicas se definen usando declaraciones `REAL, PARAMETER ::`.
* Las variables se definen usando declaraciones `REAL, DIMENSION(3) ::`.
* Las condiciones iniciales se establecen en la sección `! Condiciones iniciales`.
* El bucle temporal se ejecuta desde `t = 0.0` hasta `t = 1e8` con un paso de tiempo de `3600.0` segundos.
* En cada iteración del bucle temporal, se calculan las fuerzas sobre cada cuerpo, se actualizan las posiciones y velocidades de los cuerpos y se imprimen los resultados.