Certamente! Aqui está um código complexo em Swift que envolve manipulação de strings e estruturas de controle:

```swift
// Função para verificar se uma string é um palíndromo
func verificarPalindromo(_ texto: String) -> Bool {
    let caracteres = texto.lowercased().filter { $0.isLetter } // Remove espaços e caracteres especiais e converte tudo para minúsculas
    let inverso = String(caracteres.reversed()) // Inverte a string
    
    return caracteres == inverso
}

// Função principal
func main() {
    print("Digite uma palavra ou frase para verificar se é um palíndromo:")
    if let entrada = readLine() {
        if verificarPalindromo(entrada) {
            print("É um palíndromo!")
        } else {
            print("Não é um palíndromo!")
        }
    }
}

// Chamada da função principal
main()
```

Neste código, temos uma função chamada `verificarPalindromo` que recebe uma string como parâmetro e verifica se ela é um palíndromo. A função remove todos os caracteres especiais e espaços, converte todas as letras para minúsculas e, em seguida, inverte a string. Se a string invertida for igual à string original, significa que ela é um palíndromo.

Na função `main`, pedimos ao usuário para digitar uma palavra ou frase e chamamos a função `verificarPalindromo` para determinar se é um palíndromo. Em seguida, exibimos uma mensagem indicando se a entrada é ou não um palíndromo.

Esse código é útil para verificar se um texto digitado pelo usuário é um palíndromo, ou seja, se ele pode ser lido da mesma forma de trás para frente.