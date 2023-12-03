Claro! Aqui está um código complexo em Ruby que realiza a ordenação de um array utilizando o algoritmo de ordenação Bubble Sort. O Bubble Sort é um algoritmo de ordenação simples, porém ineficiente para grandes quantidades de dados. Este código também inclui comentários explicativos para facilitar seu entendimento.

```ruby
# Função que realiza a ordenação do array utilizando o Bubble Sort
def bubble_sort(array)
  n = array.length

  # Loop externo para percorrer todo o array
  for i in 0..n-1
    # Loop interno para comparar e trocar elementos adjacentes
    for j in 0..n-i-1
      # Compara dois elementos adjacentes
      if array[j] > array[j+1]
        # Realiza a troca de elementos
        array[j], array[j+1] = array[j+1], array[j]
      end
    end
  end

  # Retorna o array ordenado
  return array
end

# Array de exemplo
array = [64, 34, 25, 12, 22, 11, 90]

# Imprime o array desordenado
puts "Array desordenado: #{array}"

# Chama a função bubble_sort para ordenar o array
array_ordenado = bubble_sort(array)

# Imprime o array ordenado
puts "Array ordenado: #{array_ordenado}"
```

Neste código, a função `bubble_sort` recebe como parâmetro um array e utiliza o algoritmo de ordenação Bubble Sort para ordená-lo em ordem crescente. O algoritmo consiste em percorrer o array diversas vezes, comparando e trocando elementos adjacentes até que todos estejam na posição correta.

O loop externo `for` percorre todo o array, enquanto o loop interno `for` compara e troca os elementos adjacentes. A condição `array[j] > array[j+1]` verifica se dois elementos adjacentes estão fora de ordem e, caso positivo, realiza a troca utilizando a operação de troca múltipla `array[j], array[j+1] = array[j+1], array[j]`.

Após a ordenação, o código imprime o array desordenado e o array ordenado utilizando a função `puts`.

Espero que este código atenda às suas expectativas! Caso tenha alguma dúvida, estou à disposição para ajudar.