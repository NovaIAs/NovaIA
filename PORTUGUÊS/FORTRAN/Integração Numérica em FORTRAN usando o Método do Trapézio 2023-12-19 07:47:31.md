Claro! Aqui está um exemplo de código em FORTRAN que realiza a integração numérica de uma função usando o método do trapézio:

```fortran
program IntegracaoNumerica
    implicit none
    integer, parameter :: n = 1000 ! Número de pontos
    real :: a, b, h, x, integral
    real, dimension(n) :: f

    ! Defina a função que deseja integrar
    real function funcao(x)
        real :: x
        funcao = sin(x)
    end function funcao

    ! Defina os limites de integração
    a = 0.0
    b = 2.0*pi

    ! Calcule o tamanho do intervalo
    h = (b - a) / n

    ! Calcule os valores da função em cada ponto
    do i = 1, n
        x = a + (i-1) * h
        f(i) = funcao(x)
    end do

    ! Calcule a integral usando o método do trapézio
    integral = 0.5 * (f(1) + f(n))
    do i = 2, n-1
        integral = integral + f(i)
    end do
    integral = integral * h

    ! Imprima o resultado
    print *, "O valor da integral da função sin(x) de", a, "a", b, "é", integral

end program IntegracaoNumerica
```

Neste código, a função `funcao(x)` representa a função que você deseja integrar. Neste exemplo, eu usei a função seno (`sin(x)`), mas você pode substituí-la por qualquer função desejada.

O programa começa definindo o número de pontos `n` para a discretização do intervalo de integração. Em seguida, os limites de integração `a` e `b` são definidos.

O tamanho do intervalo `h` é calculado dividindo a diferença entre `b` e `a` pelo número de pontos `n`.

Em seguida, o programa calcula os valores da função `f(x)` em cada ponto do intervalo, utilizando um loop `do`.

Após calcular os valores da função, o programa utiliza o método do trapézio para calcular a integral. A fórmula básica do método do trapézio é aplicada em um loop `do` para somar a área de cada trapézio formado pelos pontos da função.

Por fim, o resultado da integral é impresso na tela.

Espero que este código atenda às suas necessidades!