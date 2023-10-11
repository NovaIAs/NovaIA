Claro! Aqui está um código complexo em Groovy que realiza uma análise de texto para identificar a frequência de cada palavra em um determinado documento:

```groovy
import java.util.Map
import java.util.HashMap

def contarPalavras(String texto) {
    // Remover pontuações e converter todas as palavras para minúsculas
    def palavras = texto.replaceAll(/[^\p{L}\s]/, "").toLowerCase().split("\\s+")
    
    // Criar um mapa para armazenar as palavras e suas frequências
    def frequencias = new HashMap<String, Integer>()
    
    // Iterar sobre todas as palavras
    palavras.each { palavra ->
        // Verificar se a palavra já existe no mapa de frequências
        if (frequencias.containsKey(palavra)) {
            // Incrementar a contagem da palavra
            frequencias.put(palavra, frequencias.get(palavra) + 1)
        } else {
            // Adicionar a palavra ao mapa de frequências com uma contagem inicial de 1
            frequencias.put(palavra, 1)
        }
    }
    
    // Retornar o mapa de frequências
    return frequencias
}

// Exemplo de uso
def texto = """
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Sed mattis massa in sem scelerisque, et tempus sapien aliquet.
Morbi eget sapien ut lectus fermentum consequat eu eu orci.
Sed vel lorem vitae arcu sollicitudin hendrerit.
"""

def frequencias = contarPalavras(texto)

// Imprimir as palavras e suas frequências
frequencias.each { palavra, frequencia ->
    println("$palavra: $frequencia")
}
```

Este código começa definindo uma função `contarPalavras` que recebe um texto como entrada e retorna um mapa contendo as palavras encontradas no texto e suas respectivas frequências.

O texto é primeiramente processado para remover pontuações e converter todas as palavras para minúsculas. Em seguida, o texto é dividido em uma lista de palavras utilizando o método `split("\\s+")`, que considera qualquer sequência de espaços em branco como um separador.

Um mapa chamado `frequencias` é criado para armazenar as palavras e suas frequências. Em um loop `each`, cada palavra é iterada e o mapa de frequências é atualizado. Se a palavra já existe no mapa, sua contagem é incrementada em 1. Caso contrário, a palavra é adicionada ao mapa com uma contagem inicial de 1.

Após a contagem de todas as palavras, o mapa de frequências é retornado pela função.

No exemplo de uso fornecido, um texto de exemplo é definido e a função `contarPalavras` é chamada passando o texto como argumento. Em seguida, o mapa de frequências é percorrido com um loop `each` para imprimir cada palavra e sua frequência no console.

Espero que este código complexo em Groovy tenha atendido às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, é só me avisar.