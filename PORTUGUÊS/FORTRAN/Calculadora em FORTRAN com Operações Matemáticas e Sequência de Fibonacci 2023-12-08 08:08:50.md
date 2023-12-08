Certamente! Aqui está um código complexo em FORTRAN, com breve explicação de cada parte:

```fortran
program Calculadora
  implicit none

  integer :: opcao, a, b, resultado
  integer :: fibonacci(100)
  integer :: i

  ! Função para calcular o fatorial de um número
  recursive function fatorial(n) result(res)
    integer, intent(in) :: n
    integer :: res

    if (n == 0) then
      res = 1
    else
      res = n * fatorial(n - 1)
    end if
  end function fatorial

  ! Sub-rotina para calcular os primeiros 100 números da sequência de Fibonacci
  subroutine calcular_fibonacci(fib)
    integer, intent(out) :: fib(:)
    integer :: i

    fib(1) = 0
    fib(2) = 1
  
    do i = 3, 100
      fib(i) = fib(i-1) + fib(i-2)
    end do
  end subroutine calcular_fibonacci

  ! Início do programa principal
  write(*, *)
  write(*, *) "Bem-vindo à Calculadora!"
  write(*, *) "Selecione a operação que deseja realizar:"
  write(*, *)
  write(*, *) "1. Soma"
  write(*, *) "2. Subtração"
  write(*, *) "3. Multiplicação"
  write(*, *) "4. Divisão"
  write(*, *) "5. Fatorial"
  write(*, *) "6. Sequência de Fibonacci"
  write(*, *) "0. Sair"
  write(*, *)
  write(*, *) "Digite o número correspondente à opção desejada:"
  read(*, *) opcao

  do while (opcao /= 0)
    select case (opcao)
      case (1) ! Soma
        write(*, *)
        write(*, *) "Digite dois números para realizar a soma:"
        read(*, *) a, b
        resultado = a + b
        write(*, *) "O resultado da soma de", a, "e", b, "é:", resultado

      case (2) ! Subtração
        write(*, *)
        write(*, *) "Digite dois números para realizar a subtração:"
        read(*, *) a, b
        resultado = a - b
        write(*, *) "O resultado da subtração de", a, "e", b, "é:", resultado

      case (3) ! Multiplicação
        write(*, *)
        write(*, *) "Digite dois números para realizar a multiplicação:"
        read(*, *) a, b
        resultado = a * b
        write(*, *) "O resultado da multiplicação de", a, "e", b, "é:", resultado

      case (4) ! Divisão
        write(*, *)
        write(*, *) "Digite dois números para realizar a divisão:"
        read(*, *) a, b
        if (b == 0) then
          write(*, *) "Erro: Divisão por zero!"
        else
          resultado = a / b
          write(*, *) "O resultado da divisão de", a, "por", b, "é:", resultado
        end if

      case (5) ! Fatorial
        write(*, *)
        write(*, *) "Digite um número para calcular o seu fatorial:"
        read(*, *) a
        resultado = fatorial(a)
        write(*, *) "O fatorial de", a, "é:", resultado

      case (6) ! Sequência de Fibonacci
        write(*, *)
        write(*, *) "Calculando os primeiros 100 números da sequência de Fibonacci..."
        call calcular_fibonacci(fibonacci)
        do i = 1, 100
          write(*, *) "Fib(", i, ") =", fibonacci(i)
        end do

      case default
        write(*, *)
        write(*, *) "Erro: Opção inválida!"

    end select

    write(*, *)
    write(*, *) "Selecione a próxima operação que deseja realizar:"
    read(*, *) opcao
  end do

  write(*, *)
  write(*, *) "Obrigado por utilizar a Calculadora! Até mais!"
  write(*, *)

end program Calculadora
```

Este código em FORTRAN é uma calculadora que permite realizar as operações de soma, subtração, multiplicação, divisão, cálculo de fatorial e cálculo dos primeiros 100 números da sequência de Fibonacci.

A estrutura do código consiste em um programa principal chamado "Calculadora", que contém uma função recursiva `fatorial` para calcular o fatorial de um número e uma sub-rotina `calcular_fibonacci` para calcular os primeiros 100 números da sequência de Fibonacci.

No programa principal, é exibido um menu de opções para o usuário selecionar a operação desejada. Cada operação é implementada em um bloco `case` dentro de uma estrutura `select case`.

Além disso, são utilizadas declarações de variáveis, entradas e saídas com `write` e `read`, condicionais `if-then-else` e um loop `do while` para repetir o menu até que o usuário selecione a opção de sair.

Espero que este código complexo atenda às suas expectativas e proporcione uma compreensão mais aprofundada da linguagem FORTRAN.