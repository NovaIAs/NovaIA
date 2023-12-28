Claro! Aqui está um código complexo em FORTRAN que realiza a análise de um conjunto de dados e determina a média, o desvio padrão e a mediana dos valores:

```fortran
program analise_dados
  implicit none
  integer, parameter :: tamanho = 1000
  real, dimension(tamanho) :: dados
  real :: media, desvio_padrao, mediana
  integer :: i
  
  ! Gera dados aleatórios
  call random_seed()
  do i = 1, tamanho
     call random_number(dados(i))
  end do
  
  ! Calcula a média dos dados
  media = 0.0
  do i = 1, tamanho
     media = media + dados(i)
  end do
  media = media / real(tamanho)
  
  ! Calcula o desvio padrão dos dados
  desvio_padrao = 0.0
  do i = 1, tamanho
     desvio_padrao = desvio_padrao + (dados(i) - media)**2
  end do
  desvio_padrao = sqrt(desvio_padrao / real(tamanho))
  
  ! Ordena os dados
  call quicksort(dados, 1, tamanho)
  
  ! Calcula a mediana dos dados
  if (mod(tamanho, 2) == 0) then
     mediana = (dados(tamanho/2) + dados(tamanho/2 + 1)) / 2.0
  else
     mediana = dados((tamanho + 1) / 2)
  end if
  
  ! Imprime os resultados
  print*, "Média: ", media
  print*, "Desvio Padrão: ", desvio_padrao
  print*, "Mediana: ", mediana
  
contains

  subroutine quicksort(arr, low, high)
    real, dimension(:) :: arr
    integer :: low, high, i, j
    real :: pivot, temp

    if (low < high) then
       pivot = arr(high)
       i = low - 1
       do j = low, high - 1
          if (arr(j) <= pivot) then
             i = i + 1
             temp = arr(i)
             arr(i) = arr(j)
             arr(j) = temp
         end if
      end do
      temp = arr(i + 1)
      arr(i + 1) = arr(high)
      arr(high) = temp

      call quicksort(arr, low, i)
      call quicksort(arr, i + 2, high)
   end if

  end subroutine quicksort

end program analise_dados
```

Este código em FORTRAN realiza a análise de um conjunto de dados aleatórios de tamanho 1000. Primeiro, ele gera os dados aleatórios usando a função `random_number`. Em seguida, calcula a média dos dados, o desvio padrão e a mediana.

A média é calculada somando todos os valores e dividindo pelo tamanho do conjunto de dados. O desvio padrão é calculado somando o quadrado da diferença entre cada valor e a média, dividindo pelo tamanho e tirando a raiz quadrada. A mediana é obtida ordenando os dados e selecionando o valor do meio, ou a média dos dois valores do meio se o tamanho for par.

O código usa o algoritmo de ordenação `quicksort` para ordenar os dados de forma eficiente. O algoritmo de ordenação é implementado como uma subrotina `quicksort`.