Claro! Aqui está um código complexo em Elixir que utiliza recursão para encontrar a soma de todos os números primos em um intervalo específico. Vou explicar o código passo a passo para facilitar o entendimento:

```elixir
defmodule SomaPrimos do
  def soma_primos(inicio, fim) do
    soma_primos_helper(inicio, fim, 0)
  end

  defp soma_primos_helper(inicio, fim, soma) when inicio > fim do
    soma
  end

  defp soma_primos_helper(inicio, fim, soma) do
    if is_primo(inicio) do
      soma_primos_helper(inicio + 1, fim, soma + inicio)
    else
      soma_primos_helper(inicio + 1, fim, soma)
    end
  end

  defp is_primo(n) when n <= 1 do
    false
  end

  defp is_primo(n) do
    is_primo_helper(n, 2)
  end

  defp is_primo_helper(n, divisor) when divisor >= n do
    true
  end

  defp is_primo_helper(n, divisor) do
    if rem(n, divisor) == 0 do
      false
    else
      is_primo_helper(n, divisor + 1)
    end
  end
end

IO.puts SomaPrimos.soma_primos(1, 100)
```

Explicação:

- Definimos um módulo chamado `SomaPrimos` que contém todas as funções necessárias para encontrar a soma dos números primos em um intervalo.
- A função pública `soma_primos` é a função principal que será chamada para obter a soma dos primos. Ela chama a função de ajuda `soma_primos_helper` com os parâmetros iniciais e uma soma inicial de 0.
- A função privada `soma_primos_helper` é uma função recursiva que percorre todos os números no intervalo fornecido. Se o número atual for primo, ele é adicionado à soma acumulada. Caso contrário, a soma permanece a mesma. Essa função continua chamando a si mesma, incrementando o número atual até que o número de início seja maior que o número de fim.
- A função privada `is_primo` é usada para verificar se um número é primo. Ela retorna `true` se o número for primo e `false` caso contrário. Primeiro, ela verifica se o número é menor ou igual a 1, nesse caso, já sabemos que não é primo. Caso contrário, ela chama a função de ajuda `is_primo_helper`.
- A função privada `is_primo_helper` é a função recursiva que verifica se um número é primo dividindo-o por todos os números de 2 até o número anterior a ele. Se o resto da divisão for 0 em algum momento, sabemos que o número não é primo e retornamos `false`. Caso contrário, continuamos chamando a função com o próximo divisor até que o divisor seja maior ou igual ao número em questão.
- Por fim, chamamos a função `SomaPrimos.soma_primos(1, 100)` para encontrar a soma de todos os números primos no intervalo de 1 a 100 e imprimimos o resultado na tela usando `IO.puts`.

Espero que este código complexo em Elixir atenda às suas expectativas!