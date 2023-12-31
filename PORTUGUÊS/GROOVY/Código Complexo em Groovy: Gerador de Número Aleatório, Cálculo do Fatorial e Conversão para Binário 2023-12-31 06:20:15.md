Claro! Aqui está um código complexo em Groovy que realiza uma série de tarefas diferentes:

```groovy
import java.util.Random

def gerarNumeroAleatorio() {
   return new Random().nextInt(100)
}

def calcularFatorial(numero) {
   if (numero < 0) {
      throw new IllegalArgumentException("Não é possível calcular o fatorial de um número negativo.")
   }
   def resultado = 1
   for (int i = 1; i <= numero; i++) {
      resultado *= i
   }
   return resultado
}

def converterParaBinario(numero) {
   StringBuilder binario = new StringBuilder()
   while (numero > 0) {
      binario.insert(0, numero % 2)
      numero /= 2
   }
   return binario.toString()
}

def imprimirMensagemAleatoria() {
   def mensagens = [
      "Olá, mundo!",
      "Bem-vindo!",
      "Que dia maravilhoso!",
      "Estou feliz de estar aqui.",
      "Tenha um ótimo dia!",
      "Aproveite cada momento."
   ]
   def mensagemAleatoria = mensagens[new Random().nextInt(mensagens.size())]
   println mensagemAleatoria
}

def main() {
   def numeroAleatorio = gerarNumeroAleatorio()
   println "Número aleatório: $numeroAleatorio"
   
   def fatorial = calcularFatorial(numeroAleatorio)
   println "Fatorial de $numeroAleatorio: $fatorial"
   
   def numeroBinario = converterParaBinario(numeroAleatorio)
   println "Número binário de $numeroAleatorio: $numeroBinario"
   
   imprimirMensagemAleatoria()
}

main()
```

Este código em Groovy começa importando a classe `Random` para gerar números aleatórios. Em seguida, são definidas algumas funções:

- `gerarNumeroAleatorio()` cria um novo objeto `Random` e gera um número aleatório entre 0 e 99.

- `calcularFatorial(numero)` calcula o fatorial de um número fornecido. Se o número for negativo, uma exceção será lançada.

- `converterParaBinario(numero)` converte um número decimal para binário.

- `imprimirMensagemAleatoria()` seleciona uma mensagem aleatória de uma lista predefinida e a imprime.

Dentro da função `main()`, o código chama as funções definidas anteriormente e imprime os resultados. O número aleatório gerado é exibido, seguido pelo fatorial desse número e, em seguida, o número em sua representação binária. Por fim, uma mensagem aleatória é impressa.

Espero que este código atenda às suas expectativas!