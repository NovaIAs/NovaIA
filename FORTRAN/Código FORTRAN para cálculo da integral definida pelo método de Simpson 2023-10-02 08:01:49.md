Claro! Aqui está um exemplo de código em FORTRAN que calcula a integral definida de uma função utilizando o método de Simpson:

```
program calcula_integral
  implicit none
  
  real :: a, b, h, x, soma
  integer :: n, i
  
  ! Definição da função a ser integrada
  real function f(x)
    real :: x
    f = x**2 + 2*x + 1
  end function f
  
  ! Entrada dos valores de a, b e n
  write(*,*) "Digite o valor de a:"
  read(*,*) a
  
  write(*,*) "Digite o valor de b:"
  read(*,*) b
  
  write(*,*) "Digite o valor de n:"
  read(*,*) n
  
  ! Cálculo do valor de h
  h = (b - a) / n
  
  ! Cálculo da soma dos valores da função
  soma = f(a) + f(b)
  
  do i = 1, n-1, 2
    x = a + i*h
    soma = soma + 4*f(x)
  end do
  
  do i = 2, n-1, 2
    x = a + i*h
    soma = soma + 2*f(x)
  end do
  
  ! Cálculo final da integral
  soma = soma * (h / 3)
  
  ! Exibição do resultado
  write(*,*) "O valor da integral é:", soma
  
end program calcula_integral
```

Esse código pedirá ao usuário para inserir os valores de `a` (limite inferior da integral), `b` (limite superior da integral) e `n` (número de subintervalos). Em seguida, ele calculará a integral definida da função f(x) = x^2 + 2x + 1 utilizando o método de Simpson e exibirá o resultado.