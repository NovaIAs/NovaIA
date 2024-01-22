```fortran
PROGRAMA Analisis_Estructural

! Declaración de variables
IMPLICIT NONE
INTEGER, PARAMETER :: Nodos = 100                       ! Número máximo de nodos
INTEGER, PARAMETER :: Elementos = 200                      ! Número máximo de elementos
INTEGER, PARAMETER :: Grados_Libertad = 3                  ! Número de grados de libertad por nodo
REAL(KIND=8), PARAMETER :: Tolerancia = 1.0E-12            ! Tolerancia para la convergencia de la solución
REAL(KIND=8), PARAMETER :: Carga_Gravedad = 9.81           ! Aceleración de la gravedad (m/s^2)

TYPE(Nodo) :: Nodos(Nodos)                               ! Arreglo de nodos
TYPE(Elemento) :: Elementos(Elementos)                      ! Arreglo de elementos

! Tipo de datos para los nodos
TYPE Nodo
    REAL(KIND=8) :: X, Y, Z                                 ! Coordenadas del nodo (m)
    REAL(KIND=8), DIMENSION(Grados_Libertad) :: U           ! Desplazamientos del nodo (m)
    REAL(KIND=8), DIMENSION(Grados_Libertad) :: F           ! Fuerzas aplicadas al nodo (N)
END TYPE Nodo

! Tipo de datos para los elementos
TYPE Elemento
    INTEGER, DIMENSION(2) :: Nodos                         ! Nodos que forman el elemento
    REAL(KIND=8) :: A                                      ! Área de la sección transversal del elemento (m^2)
    REAL(KIND=8) :: E                                      ! Módulo de elasticidad del material (N/m^2)
    REAL(KIND=8) :: L                                      ! Longitud del elemento (m)
END TYPE Elemento

! Procedimiento para leer los datos del problema
SUBROUTINE Leer_Datos()

    ! Abrir el archivo de entrada
    OPEN(10, FILE='entrada.txt')

    ! Leer el número de nodos y elementos
    READ(10, *) Nodos, Elementos

    ! Leer las coordenadas de los nodos
    DO I = 1, Nodos
        READ(10, *) Nodos(I)%X, Nodos(I)%Y, Nodos(I)%Z
    END DO

    ! Leer los datos de los elementos
    DO I = 1, Elementos
        READ(10, *) Elementos(I)%Nodos, Elementos(I)%A, Elementos(I)%E, Elementos(I)%L
    END DO

    ! Leer las condiciones de contorno
    DO I = 1, Nodos
        READ(10, *) Nodos(I)%U, Nodos(I)%F
    END DO

    ! Cerrar el archivo de entrada
    CLOSE(10)

END SUBROUTINE Leer_Datos

! Procedimiento para ensamblar la matriz de rigidez global
SUBROUTINE Ensamblar_Matriz_Rigidez_Global()

    ! Inicializar la matriz de rigidez global
    REAL(KIND=8), DIMENSION(Nodos*Grados_Libertad, Nodos*Grados_Libertad) :: K_Global = 0.0

    ! Recorrer los elementos
    DO I = 1, Elementos

        ! Obtener los nodos del elemento
        N1 = Elementos(I)%Nodos(1)
        N2 = Elementos(I)%Nodos(2)

        ! Calcular la matriz de rigidez del elemento
        REAL(KIND=8), DIMENSION(Grados_Libertad, Grados_Libertad) :: K_Elemento

        ! Calcular la matriz de rigidez del elemento
        K_Elemento = Calcular_Matriz_Rigidez_Elemento(Elementos(I))

        ! Ensamblar la matriz de rigidez global
        DO J = 1, Grados_Libertad
            DO K = 1, Grados_Libertad
                K_Global((N1-1)*Grados_Libertad + J, (N2-1)*Grados_Libertad + K) = K_Global((N1-1)*Grados_Libertad + J, (N2-1)*Grados_Libertad + K) + K_Elemento(J, K)
                K_Global((N2-1)*Grados_Libertad + J, (N1-1)*Grados_Libertad + K) = K_Global((N2-1)*Grados_Libertad + J, (N1-1)*Grados_Libertad + K) + K_Elemento(J, K)
            END DO
        END DO

    END DO

    ! Aplicar las condiciones de contorno
    DO I = 1, Nodos
        DO J = 1, Grados_Libertad
            IF (Nodos(I)%U(J) /= 0.0) THEN
                DO K = 1, Nodos*Grados_Libertad
                    K_Global(I*Grados_Libertad - (J-1), K) = 0.0
                END DO
                K_Global(I*Grados_Libertad - (J-1), I*Grados_Libertad - (J-1)) = 1.0
            END IF
        END DO
    END DO

END SUBROUTINE Ensamblar_Matriz_Rigidez_Global

! Procedimiento para calcular la matriz de rigidez de un elemento
FUNCTION Calcular_Matriz_Rigidez_Elemento(Elemento)

    IMPLICIT NONE

    ! Obtener los datos del elemento
    A = Elemento%A
    E = Elemento%E
    L = Elemento%L

    ! Calcular la matriz de rigidez del elemento
    REAL(KIND=8), DIMENSION(Grados_Libertad, Grados_Libertad) :: K_Elemento

    ! Calcular la matriz de rigidez del elemento
    K_Elemento = (A*E/L) * [1.0, -1.0;
                             -1.0, 1.0]

    RETURN

END FUNCTION Calcular_Matriz_Rigidez_Elemento

! Procedimiento para resolver el sistema de ecuaciones lineales
SUBROUTINE Resolver_Sistema_Ecuaciones_Lineales(K_Global, F_Global, U_Global)

    ! Obtener el tamaño de la matriz
    INTEGER :: N = SIZE(K_Global, 1)

    ! Crear el vector de desplazamientos
    REAL(KIND=8), DIMENSION(N) :: U_Global

    ! Crear el vector de fuerzas
    REAL(KIND=8), DIMENSION(N) :: F_Global

    ! Crear el vector de desplazamientos
    U_Global = 0.0

    ! Crear el vector de fuerzas
    F_Global = 0.0

    ! Recorrer los nodos
    DO I = 1, Nodos

        ! Obtener los desplazamientos del nodo
        U = Nodos(I)%U

        ! Obtener las fuerzas aplicadas al nodo
        F = Nodos(I)%F

        ! Obtener el índice del nodo
        IN