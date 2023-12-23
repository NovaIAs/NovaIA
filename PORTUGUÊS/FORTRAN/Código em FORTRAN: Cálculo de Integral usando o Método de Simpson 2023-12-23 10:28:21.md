Claro! Aqui está um código em FORTRAN que realiza o cálculo de uma integral definida usando o método de Simpson. O código foi escrito em português e possui comentários explicativos para facilitar a compreensão:

```fortran
program integral_simpson
    implicit none
    integer :: n, i
    real :: a, b, h, integral
    real, dimension(:), allocatable :: x, f

    ! Solicita ao usuário os limites de integração
    write(*,*) "Digite o limite inferior da integral (a):"
    read(*,*) a
    write(*,*) "Digite o limite superior da integral (b):"
    read(*,*) b

    ! Solicita ao usuário o número de subintervalos
    write(*,*) "Digite o número de subintervalos (n):"
    read(*,*) n

    ! Calcula o tamanho de cada subintervalo
    h = (b - a) / n

    ! Aloca memória para os vetores
    allocate(x(n+1))
    allocate(f(n+1))

    ! Preenche os vetores com os pontos x e f(x)
    do i = 0, n
        x(i) = a + i * h
        f(i) = funcao(x(i))
    end do

    ! Calcula a integral usando o método de Simpson
    integral = 0.0
    do i = 0, n-1, 2
        integral = integral + h/3 * (f(i) + 4*f(i+1) + f(i+2))
    end do

    ! Exibe o valor da integral calculada
    write(*,*) "O valor da integral é:", integral

    ! Libera a memória alocada
    deallocate(x)
    deallocate(f)

contains

    ! Função que representa a função f(x) a ser integrada
    function funcao(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x**2 + 2*x + 1
    end function funcao

end program integral_simpson
```

Este código em FORTRAN solicita ao usuário o limite inferior (a) e o limite superior (b) da integral, bem como o número de subintervalos (n) desejado para a discretização. Em seguida, ele calcula o tamanho de cada subintervalo (h) e aloca memória para os vetores que armazenarão os pontos x e f(x).

Em seguida, o código preenche os vetores com os valores correspondentes aos pontos x e f(x) utilizando um loop do tipo DO. A função `funcao(x)` é definida como `x**2 + 2*x + 1` para ilustrar um exemplo simples.

Após preencher os vetores, o código calcula a integral usando o método de Simpson através de outro loop DO. O resultado da integral é armazenado na variável `integral`.

Por fim, o valor da integral é exibido na tela e a memória alocada é liberada.

Espero que esse código atenda às suas expectativas! Se você tiver mais alguma solicitação, é só me pedir.