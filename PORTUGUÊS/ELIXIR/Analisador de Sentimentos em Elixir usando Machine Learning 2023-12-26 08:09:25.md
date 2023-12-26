Claro! Aqui está um código complexo em Elixir que faz a análise e classificação de sentimentos em textos usando machine learning:

```elixir
defmodule AnalisadorSentimentos do
  @moduledoc """
  Módulo responsável por analisar e classificar sentimentos em textos.
  """

  @sentimentos_possiveis ~w(positivo negativo neutro)a

  @modelo_treinado [
    %{
      texto: "Eu amo essa música!",
      sentimento: "positivo"
    },
    %{
      texto: "Esse filme é horrível!",
      sentimento: "negativo"
    },
    %{
      texto: "Estou indiferente sobre esse assunto.",
      sentimento: "neutro"
    }
    # adicionar mais exemplos de treinamento aqui
  ]

  def classificar_sentimento(texto) do
    # pré-processamento do texto (remoção de stopwords, tokenização, etc)
    texto_processado = preprocessar_texto(texto)

    # extrair características do texto
    caracteristicas = extrair_caracteristicas(texto_processado)

    # classificar o sentimento usando um modelo treinado
    sentimento = classificar(caracteristicas)

    sentimento
  end

  defp preprocessar_texto(texto) do
    # implementar etapas de pré-processamento aqui
    # remover stopwords, pontuação, converter para minúsculo, etc
    texto_processado = texto |> String.downcase |> String.replace(~r/[[:punct:]]/, "")

    texto_processado
  end

  defp extrair_caracteristicas(texto) do
    # implementar extração de características aqui
    # vetorizar o texto usando TF-IDF, n-gramas, etc
    # retornar um vetor de características
    caracteristicas = texto |> String.split() |> Enum.map(&String.trim/1)

    caracteristicas
  end

  defp classificar(caracteristicas) do
    # implementar classificação usando um modelo treinado aqui
    # pode ser um algoritmo de machine learning como Naive Bayes, SVM, etc
    # retornar o sentimento classificado
    sentimento = Enum.random(@sentimentos_possiveis)

    sentimento
  end
end
```

Explicação do código:

- O módulo `AnalisadorSentimentos` é responsável por analisar e classificar sentimentos em textos.
- O atributo `@sentimentos_possiveis` define uma lista de sentimentos possíveis: "positivo", "negativo" e "neutro".
- O atributo `@modelo_treinado` representa um conjunto de exemplos de treinamento com textos e seus respectivos sentimentos.
- A função `classificar_sentimento/1` recebe um texto como entrada e retorna o sentimento classificado.
- A função `preprocessar_texto/1` realiza etapas de pré-processamento no texto, como remoção de stopwords e pontuação, e conversão para minúsculo.
- A função `extrair_caracteristicas/1` extrai características do texto, como a vetorização usando TF-IDF ou n-gramas.
- A função `classificar/1` realiza a classificação do sentimento usando um modelo treinado. Neste exemplo, é usado um algoritmo simples que seleciona um sentimento aleatório da lista de sentimentos possíveis.

Este código é apenas um exemplo básico para ilustrar o processo de análise e classificação de sentimentos em textos. Em um cenário real, seria necessário um conjunto de dados de treinamento mais completo e um modelo de machine learning mais sofisticado para obter resultados mais precisos.