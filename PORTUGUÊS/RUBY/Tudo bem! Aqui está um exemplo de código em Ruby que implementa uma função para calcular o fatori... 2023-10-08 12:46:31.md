Claro! Abaixo está um exemplo de um código em Ruby que utiliza um algoritmo de ordenação chamado Quicksort.

```ruby
def quicksort(array)
  return array if array.length <= 1

  pivot = array.sample
  less = []
  equal = []
  greater = []

  array.each do |x|
    if x < pivot
      less << x
    elsif x == pivot
      equal << x
    else
      greater << x
    end
  end

  return quicksort(less) + equal + quicksort(greater)
end

array = [9, 7, 5, 11, 12, 2, 14, 10]
sorted_array = quicksort(array)
puts sorted_array
```

Neste código, a função `quicksort` recebe um array como parâmetro e retorna o array ordenado. O algoritmo funciona da seguinte forma:

1. Se o array tiver um tamanho menor ou igual a 1, ele já está ordenado e é retornado.
2. Caso contrário, um elemento pivot é selecionado aleatoriamente do array.
3. Em seguida, três arrays vazios são criados: `less` (menores que o pivot), `equal` (iguais ao pivot) e `greater` (maiores que o pivot).
4. Cada elemento do array original é percorrido e é adicionado ao array correspondente ao seu valor em relação ao pivot.
5. O algoritmo é aplicado recursivamente aos arrays `less` e `greater`, e o resultado é concatenado com o array `equal`.
6. O array ordenado final é retornado.

No exemplo fornecido, o array `[9, 7, 5, 11, 12, 2, 14, 10]` é ordenado utilizando o Quicksort e o resultado `[2, 5, 7, 9, 10, 11, 12, 14]` é exibido.