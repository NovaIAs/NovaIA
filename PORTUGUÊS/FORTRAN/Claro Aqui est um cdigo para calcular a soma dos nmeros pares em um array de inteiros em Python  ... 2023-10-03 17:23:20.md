Claro! Aqui está um código em FORTRAN que envolve um cálculo complexo de integração numérica usando o método de Simpson para uma função específica.

```fortran
program integra_simpson
  implicit none
  integer :: i, n
  real :: a, b, h, x, integral
  real, dimension(100) :: f

  ! Defina a função que deseja integrar
  contains
    real function funcao(x)
      real, intent(in) :: x
      funcao = sin(x) * exp(-x)
    end function funcao
  end program integra_simpson

  ! Limite inferior e superior da integral
  a = 0.0
  b = 1.0

  ! Número de subintervalos para dividir a integral
  n = 100

  ! Calcula o tamanho do intervalo
  h = (b - a) / real(n)

  ! Calcula os valores da função em cada ponto do intervalo
  do i = 1, n+1
    x = a + (i - 1) * h
    f(i) = funcao(x)
  end do

  ! Realiza o cálculo da integração usando o método de Simpson
  integral = f(1) + f(n+1)
  do i = 2, n, 2
    integral = integral + 4.0 * f(i)
  end do
  do i = 3, n-1, 2
    integral = integral + 2.0 * f(i)
  end do
  integral = integral * h / 3.0

  ! Imprime o resultado da integração
  print *, "O valor da integral é:", integral
end program integra_simpson
```

Neste código, realizamos a integração numérica de uma função específica, representada pela função `funcao(x) = sin(x) * exp(-x)`. Os limites inferior (`a`) e superior (`b`) da integral são definidos como 0.0 e 1.0, respectivamente.

Em seguida, o número de subintervalos (`n`) é definido como 100. Quanto maior o valor de `n`, maior será a precisão do cálculo da integral.

O tamanho do intervalo (`h`) é calculado dividindo a diferença entre `b` e `a` por `n`.

Depois disso, o código calcula os valores da função em cada ponto do intervalo, armazenando-os no array `f`.

A integração propriamente dita é realizada usando o método de Simpson. Primeiro, somamos os valores da função nos pontos inicial e final. Em seguida, percorremos os pontos intermediários, somando 4 vezes o valor da função nos pontos pares e 2 vezes o valor da função nos pontos ímpares. Por fim, multiplicamos o resultado por `h/3` para obter a integral aproximada.

O resultado final da integração é impresso na tela.

Espero que este código atenda às suas expectativas!