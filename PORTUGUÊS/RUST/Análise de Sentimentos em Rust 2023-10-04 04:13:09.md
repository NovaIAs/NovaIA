Certamente! Aqui está um código complexo em Rust que utiliza múltiplas estruturas de dados e algoritmos para realizar uma análise de sentimentos em um conjunto de textos. Vou explicar cada parte do código para facilitar o entendimento.

```rust
use std::collections::{HashMap, HashSet};

enum Sentiment {
    Positive,
    Negative,
    Neutral,
}

fn analyze_sentiment(texts: &[&str]) -> HashMap<&str, Sentiment> {
    let mut sentiment_scores: HashMap<&str, i32> = HashMap::new();
    let positive_words: HashSet<&str> = ["feliz", "bom", "ótimo"].iter().cloned().collect();
    let negative_words: HashSet<&str> = ["triste", "ruim", "terrível"].iter().cloned().collect();

    for text in texts {
        let words: Vec<&str> = text.split_whitespace().collect();
        let sentiment_score = words.iter().fold(0, |acc, word| {
            if positive_words.contains(word) {
                acc + 1
            } else if negative_words.contains(word) {
                acc - 1
            } else {
                acc
            }
        });

        sentiment_scores.insert(text, sentiment_score);
    }

    sentiment_scores
        .into_iter()
        .map(|(text, score)| {
            let sentiment = if score > 0 {
                Sentiment::Positive
            } else if score < 0 {
                Sentiment::Negative
            } else {
                Sentiment::Neutral
            };

            (text, sentiment)
        })
        .collect()
}

fn main() {
    let texts = [
        "Estou muito feliz com o resultado!",
        "Que dia terrível, estou arrasado...",
        "O filme foi bom, mas o final foi ruim.",
        "Não tenho opinião formada sobre isso.",
    ];

    let sentiment_analysis = analyze_sentiment(&texts);

    for (text, sentiment) in sentiment_analysis {
        println!("Texto: '{}'", text);
        println!("Sentimento: {:?}", sentiment);
    }
}
```

Nesse código, começamos importando as bibliotecas necessárias, incluindo `HashMap` e `HashSet` do módulo `std::collections`. Em seguida, definimos um enum chamado `Sentiment` para representar os sentimentos possíveis (positivo, negativo ou neutro).

A função `analyze_sentiment` recebe um slice de textos como entrada e retorna um `HashMap` que mapeia cada texto ao seu respectivo sentimento. 

Dentro da função, inicializamos um `HashMap` chamado `sentiment_scores` para armazenar os escores de sentimento de cada texto. Também criamos dois `HashSet` chamados `positive_words` e `negative_words` para armazenar palavras positivas e negativas, respectivamente.

Em seguida, percorremos cada texto recebido e dividimos seu conteúdo em palavras usando `split_whitespace()`. Iteramos sobre as palavras e, para cada uma, verificamos se ela está presente no conjunto de palavras positivas ou negativas. Ao encontrar uma correspondência, incrementamos ou decrementamos um contador de escore de sentimento.

Depois de calcular o escore de sentimento de cada texto, inserimos o par (texto, escore) no `HashMap` `sentiment_scores`.

Finalmente, iteramos sobre os pares (texto, escore) em `sentiment_scores`, mapeando cada escore para o enum `Sentiment` apropriado. Em seguida, coletamos os pares (texto, sentimento) em um novo `HashMap` e retornamos o resultado.

No `main`, definimos um array de textos para análise e chamamos a função `analyze_sentiment` passando esse array como argumento. Em seguida, iteramos sobre o resultado da análise e imprimimos o texto e o sentimento correspondente.

Espero que esse código complexo em Rust atenda às suas expectativas!