Claro! Aqui está um exemplo de código em Elixir que realiza uma análise de sentimentos em um texto usando a biblioteca Sentiment:

```elixir
defmodule AnaliseSentimentos do
  def analisar_texto(texto) do
    # Realiza a análise de sentimentos usando a biblioteca Sentiment
    resultado = Sentiment.analyze(texto)
    
    # Obtém o valor do sentimento
    sentimento = resultado[:sentiment]
    
    # Imprime o resultado da análise de sentimentos
    IO.puts "Sentimento: #{sentimento}"
    
    # Obtém a pontuação de cada categoria de sentimento
    categorias = resultado[:categories]
    
    # Imprime a pontuação de cada categoria de sentimento
    IO.puts "Pontuação por categoria:"
    Enum.each(categorias, fn {categoria, pontuacao} ->
      IO.puts "#{categoria}: #{pontuacao}"
    end)
  end
end

# Exemplo de uso
texto = "Eu amo programar! É a melhor coisa do mundo!"
AnaliseSentimentos.analisar_texto(texto)
```

Neste código, criamos um módulo chamado `AnaliseSentimentos`. Dentro deste módulo, definimos uma função `analisar_texto` que recebe um texto como parâmetro.

Dentro da função, utilizamos a biblioteca Sentiment para realizar a análise de sentimentos no texto fornecido. O resultado da análise é armazenado na variável `resultado`.

Em seguida, extraímos o valor do sentimento do resultado e o imprimimos na tela usando a função `IO.puts`.

Também acessamos a pontuação de cada categoria de sentimento do resultado e as imprimimos na tela usando um `Enum.each`.

Por fim, fora do módulo, criamos uma variável `texto` com um exemplo de texto e chamamos a função `analisar_texto` para realizar a análise de sentimentos nesse texto.

Este é apenas um exemplo básico de como realizar uma análise de sentimentos usando a biblioteca Sentiment em Elixir. É possível expandir e personalizar esse código de acordo com as necessidades específicas do projeto.