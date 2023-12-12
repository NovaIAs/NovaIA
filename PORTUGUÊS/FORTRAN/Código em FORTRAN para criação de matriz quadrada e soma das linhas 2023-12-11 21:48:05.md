Aqui está um exemplo de código em FORTRAN que cria uma matriz quadrada de tamanho N e a preenche com números aleatórios. Em seguida, ele calcula a soma de cada linha da matriz e exibe o resultado.

```fortran
program matriz_quadrada
  implicit none
  integer, parameter :: N = 5
  integer :: i, j
  real :: matriz(N, N)
  real :: soma_linha

  ! Define a semente para gerar números aleatórios
  call random_seed()

  ! Preenche a matriz com números aleatórios entre 0 e 1
  call random_number(matriz)

  ! Calcula a soma de cada linha da matriz e exibe o resultado
  do i = 1, N
    soma_linha = 0.0
    do j = 1, N
      soma_linha = soma_linha + matriz(i, j)
    end do
    write(*, '(A, I2, A, F6.2)') 'A soma da linha ', i, ' é: ', soma_linha
  end do

end program matriz_quadrada
```

Este código começa declarando a variável N, que representa o tamanho da matriz quadrada desejada (neste caso, 5). A matriz é declarada como uma matriz de números reais usando a declaração `real :: matriz(N, N)`.

Em seguida, a função `random_seed` é chamada para definir a semente para a geração de números aleatórios. Isso garante que cada execução do programa produza resultados diferentes.

A função `random_number` é então usada para preencher a matriz com números aleatórios entre 0 e 1.

O loop `do i = 1, N` itera sobre cada linha da matriz. Dentro desse loop, outro loop `do j = 1, N` itera sobre cada elemento da linha atual. A soma de cada linha é calculada adicionando cada elemento à variável `soma_linha`.

Por fim, a função `write` é usada para exibir a soma de cada linha. A formatação `'(A, I2, A, F6.2)'` é usada para formatar a saída, onde `A` representa uma string, `I2` representa um inteiro de 2 dígitos e `F6.2` representa um número de ponto flutuante com 6 dígitos no total e 2 casas decimais.

Espero que este exemplo tenha sido útil para você! Se tiver alguma outra solicitação, fique à vontade para pedir.