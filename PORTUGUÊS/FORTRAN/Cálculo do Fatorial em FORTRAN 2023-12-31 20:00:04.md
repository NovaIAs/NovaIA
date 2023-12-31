Claro! Aqui está um código em FORTRAN que realiza o cálculo do fatorial de um número fornecido pelo usuário. Ele utiliza uma abordagem recursiva para calcular o fatorial e inclui comentários explicativos em português para facilitar a compreensão:

```fortran
program fatorial
  implicit none

  integer :: numero, resultado

  ! Solicita ao usuário um número inteiro positivo
  write(*,*) "Digite um número inteiro positivo:"
  read(*,*) numero

  ! Verifica se o número fornecido é válido
  if (numero < 0) then
    write(*,*) "Número inválido! Digite um número inteiro positivo."
    stop
  end if

  ! Chamada à função recursiva que calcula o fatorial
  resultado = calcularFatorial(numero)

  ! Exibe o resultado
  write(*,*) "O fatorial de", numero, "é", resultado

contains

  recursive function calcularFatorial(n) result(fatorial)
    integer, intent(in) :: n
    integer :: fatorial

    ! Caso base: fatorial de 0 é 1
    if (n == 0) then
      fatorial = 1
    else
      ! Chamada recursiva para calcular o fatorial de n-1
      fatorial = n * calcularFatorial(n - 1)
    end if

  end function calcularFatorial

end program fatorial
```

Espero que este código em FORTRAN atenda às suas expectativas! Caso tenha mais alguma solicitação, estou à disposição.