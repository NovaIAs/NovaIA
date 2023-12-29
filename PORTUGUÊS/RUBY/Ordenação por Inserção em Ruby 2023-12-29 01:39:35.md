Claro! Aqui está um código complexo em Ruby, que realiza a ordenação de um array utilizando o algoritmo de Ordenação por Inserção. O código é bastante comentado para facilitar a compreensão:

```ruby
def ordenacao_por_insercao(array)
  # Percorre o array a partir do segundo elemento
  for i in 1..array.length-1
    # Salva o valor atual em uma variável temporária
    valor_atual = array[i]
    # Inicializa a variável de comparação
    j = i-1

    # Desloca os elementos maiores que o valor atual para a direita
    while j >= 0 && array[j] > valor_atual
      array[j+1] = array[j]
      j = j-1
    end

    # Insere o valor atual na posição correta
    array[j+1] = valor_atual
  end

  return array
end

# Exemplo de uso
array = [5, 3, 8, 2, 1, 9, 4, 6, 7]
puts "Array desordenado: #{array}"

array_ordenado = ordenacao_por_insercao(array)
puts "Array ordenado: #{array_ordenado}"
```

Neste código, a função `ordenacao_por_insercao` recebe um array como parâmetro e retorna o array ordenado. O algoritmo de Ordenação por Inserção é utilizado para realizar a ordenação.

O algoritmo funciona da seguinte forma:
1. O laço `for` percorre o array a partir do segundo elemento.
2. O valor atual é salvo em uma variável temporária.
3. A variável `j` é inicializada com `i-1`, onde `i` é a posição atual do elemento no array.
4. O laço `while` é utilizado para deslocar os elementos maiores que o valor atual para a direita.
5. Ao final do laço `while`, o valor atual é inserido na posição correta.
6. O laço `for` continua até percorrer todos os elementos do array.
7. O array ordenado é retornado.

No exemplo de uso apresentado, um array desordenado é definido e passado como argumento para a função `ordenacao_por_insercao`. O array ordenado é então exibido no console.

Espero que esse código complexo em Ruby atenda às suas expectativas!