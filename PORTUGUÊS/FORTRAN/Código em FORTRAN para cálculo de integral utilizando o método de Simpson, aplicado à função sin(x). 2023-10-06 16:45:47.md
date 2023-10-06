Claro! Aqui está um código complexo em FORTRAN que realiza a integração numérica utilizando o método de Simpson para uma função específica:

```fortran
program integracao_simpson
  implicit none
  
  integer, parameter :: ndiv = 1000  ! Número de divisões
  real, parameter :: a = 0.0          ! Limite inferior de integração
  real, parameter :: b = 1.0          ! Limite superior de integração
  real :: h, x, integral
  integer :: i
  
  real function f(x)
    real, intent(in) :: x
    f = sin(x)  ! Função a ser integrada
  end function f

  h = (b - a) / ndiv
  integral = f(a) + f(b)
  
  do i = 1, ndiv-1, 2
    x = a + i * h
    integral = integral + 4.0 * f(x)
  end do

  do i = 2, ndiv-1, 2
    x = a + i * h
    integral = integral + 2.0 * f(x)
  end do
  
  integral = integral * h / 3.0
  
  write(*,*) 'O valor da integral da função sin(x) de', a, 'a', b, 'é:', integral

end program integracao_simpson
```

Este código em FORTRAN implementa o método de Simpson para calcular a integral numérica da função `sin(x)` no intervalo de `0` a `1`. 

A variável `ndiv` define o número de divisões para calcular a integral, ou seja, quanto maior o valor de `ndiv`, maior será a precisão do resultado, mas também maior será o tempo de execução. 

As variáveis `a` e `b` definem os limites inferior e superior de integração, respectivamente.

A função `f(x)` representa a função a ser integrada, que neste caso é `sin(x)`.

O código utiliza dois loops `do` para calcular a soma dos termos do método de Simpson. O primeiro loop soma os termos com fator 4 e o segundo loop soma os termos com fator 2.

Por fim, a integral é multiplicada pelo fator `h/3.0` e o resultado é exibido na saída padrão.

Espero que este exemplo atenda às suas expectativas!