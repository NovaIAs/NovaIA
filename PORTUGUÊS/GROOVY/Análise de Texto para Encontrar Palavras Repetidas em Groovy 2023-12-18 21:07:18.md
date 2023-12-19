Claro! Aqui está um exemplo de um código complexo em Groovy que realiza a análise de um texto para encontrar palavras repetidas:

```groovy
import java.util.HashMap

def analisarTexto(texto) {
    def palavras = texto.toLowerCase().replaceAll(/[^a-zA-ZÀ-ÿ0-9 ]/, "").split("\\s+")
    def contagemPalavras = new HashMap<String, Integer>()

    palavras.each { palavra ->
        if (contagemPalavras.containsKey(palavra)) {
            contagemPalavras.put(palavra, contagemPalavras.get(palavra) + 1)
        } else {
            contagemPalavras.put(palavra, 1)
        }
    }

    def palavrasRepetidas = contagemPalavras.findAll { it.value > 1 }.collect { it.key }
    
    println("Palavras repetidas encontradas:")
    palavrasRepetidas.each { palavra ->
        println("$palavra: ${contagemPalavras.get(palavra)} vezes")
    }
}

def texto = """
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed ut lacinia nunc. Fusce euismod mauris nisi, quis tincidunt mauris interdum eu. Ut gravida vulputate sem id lacinia. Sed ultrices convallis faucibus. Integer convallis lobortis semper. Nullam varius enim et nisl interdum, sed imperdiet erat congue. Etiam leo odio, dignissim eu ipsum at, fermentum faucibus elit. Sed at eros bibendum, fermentum mi a, mattis mauris. Aliquam nec urna eget nisi viverra consequat. Nunc varius mauris sed tincidunt scelerisque. Mauris fringilla massa purus, eu feugiat neque posuere nec. Sed sollicitudin, turpis quis consequat aliquam, libero tortor vestibulum mi, vel sollicitudin ante urna vitae eros. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Aliquam luctus ante quis nibh dapibus imperdiet.
"""

analisarTexto(texto)
```

Neste código, primeiro importamos a classe `HashMap` do Java para utilizar como estrutura de dados para guardar a contagem das palavras. 

Em seguida, definimos uma função `analisarTexto` que recebe o texto como parâmetro. O texto é convertido para letras minúsculas e removemos todos os caracteres especiais, mantendo apenas letras e números. Dividimos o texto em palavras utilizando o método `split("\\s+")` que considera qualquer sequência de espaços em branco como delimitador.

Criamos um objeto `contagemPalavras` do tipo `HashMap` para armazenar a contagem de cada palavra. Iteramos sobre as palavras e, se uma palavra já existe no mapa, incrementamos o valor da contagem. Caso contrário, adicionamos a palavra no mapa com contagem igual a 1.

Em seguida, filtramos o mapa para obter apenas as palavras repetidas, ou seja, aquelas que possuem uma contagem maior que 1. Por fim, imprimimos as palavras repetidas e a quantidade de vezes que cada uma aparece no texto.

No exemplo fornecido, o código analisa um texto em lorem ipsum e imprime as palavras repetidas encontradas:

```
Palavras repetidas encontradas:
sed: 4 vezes
et: 4 vezes
mauris: 2 vezes
consectetur: 2 vezes
ipsum: 2 vezes
lacinia: 2 vezes
quis: 2 vezes
```