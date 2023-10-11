CERTO! Aqui está um exemplo de código em FORTRAN que implementa o método de integração numérica conhecido como Regra dos Trapézios:

```
PROGRAM Regra_dos_Trapezios

    IMPLICIT NONE

    INTEGER :: i, n
    REAL :: a, b, h, integral, x
    REAL, DIMENSION(:), ALLOCATABLE :: f

    ! Definir a função f(x) que será integrada
    CONTAINS
        FUNCTION f(x)
            REAL, INTENT(IN) :: x
            f = x**2 + 2*x + 1
        END FUNCTION f

    ! Entrada dos limites de integração e número de subintervalos
    WRITE(*,*) "Digite o limite inferior de integração (a):"
    READ(*,*) a
    WRITE(*,*) "Digite o limite superior de integração (b):"
    READ(*,*) b
    WRITE(*,*) "Digite o número de subintervalos (n):"
    READ(*,*) n

    ! Cálculo do tamanho do subintervalo
    h = (b - a) / n

    ! Alocação do vetor de valores de f(x)
    ALLOCATE(f(n+1))

    ! Cálculo dos valores de f(x) nos pontos do intervalo
    DO i = 0, n
        x = a + i * h
        f(i+1) = f(x)
    END DO

    ! Cálculo da integral usando a Regra dos Trapézios
    integral = 0.0
    DO i = 1, n
        integral = integral + (f(i) + f(i+1)) * h / 2
    END DO

    ! Exibição do resultado
    WRITE(*,*) "O valor aproximado da integral é:", integral

    ! Desalocação do vetor f(x)
    DEALLOCATE(f)

END PROGRAM Regra_dos_Trapezios
```

Este código em FORTRAN implementa o método da Regra dos Trapézios para calcular a integral de uma função f(x) em um intervalo [a, b]. O número de subintervalos é definido pelo usuário.

A função f(x) é definida como f(x) = x^2 + 2x + 1 neste exemplo, mas você pode modificar essa função de acordo com suas necessidades.

O código começa solicitando ao usuário que insira o limite inferior de integração (a), o limite superior de integração (b) e o número de subintervalos (n).

Em seguida, o código calcula o tamanho do subintervalo (h) e aloca um vetor de tamanho n+1 para armazenar os valores de f(x) nos pontos do intervalo.

Depois disso, o código itera sobre os subintervalos e calcula o valor da integral usando a fórmula da Regra dos Trapézios.

Por fim, o código exibe o valor aproximado da integral.

Espero que este código atenda às suas expectativas! Lembre-se de que este é apenas um exemplo e você pode adaptá-lo ou modificar os parâmetros de acordo com suas necessidades.