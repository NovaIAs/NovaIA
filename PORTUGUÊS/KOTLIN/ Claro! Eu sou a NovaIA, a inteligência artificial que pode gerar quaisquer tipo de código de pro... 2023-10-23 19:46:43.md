 Claro! Eu sou a NovaIA, a inteligência artificial que pode gerar quaisquer tipo de código de programação. Estou aqui para ajudá-lo com seu projeto em Kotlin.

Para começar, vamos criar um aplicativo de contagem regressiva que irá contar de 100 até 0. Para isso, vamos precisar de alguns blocos de código:

1. Declaração da variável `count`:
```kotlin
var count = 100
```
2. Função `decrementCounter()` que decrementará a variável `count` em 1 todos os segundos:
```kotlin
fun decrementCounter() {
   count--
}
```
3. Função `startTimer()` que iniciará o timer com uma duração de 100 segundos:
```kotlin
fun startTimer() {
   val handler = object : Handler() {
       override fun handleMessage(message: Message?) {
           if (message != null) {
               decrementCounter()
           }
       }
   }
   val thread = Thread({
       try {
           sleep(1000) // 1 segundo
       } catch (e: InterruptedException) {
           e.printStackTrace()
       }
       handler.sendEmptyMessage(0)
   }).start()
}
```
4. Função `stopTimer()` que parará o timer:
```kotlin
fun stopTimer() {
   if (handler != null) {
       handler.removeMessages(0)
       thread.interrupt()
       thread.join()
   }
}
```
5. Função `main()` que será executada quando o aplicativo for started:
```kotlin
fun main() {
   startTimer()
   while (count > 0) {
       println("$count")
   }
   stopTimer()
}
```
Agora, vamos explicar um pouco cada bloco de código acima:

1. A variável `count` é declarada como `var`, o que significa que pode ser alterada durante a execução do código. Inicialmente, ela tem o valor de 100.
2. A função `decrementCounter()` diminui o valor da variável `count` em 1 todos os segundos. Ela não faz nada mais do que diminuir o valor da variável.
3. A função `startTimer()` inicia um timer com uma duração de 100 segundos. O timer irá chamar a função `decrementCounter()` todos os segundos até que a variável `count` atinja o valor zero.
4. A função `stopTimer()` para o timer.
5. A função `main()` é executada quando o aplicativo é started. Ela chama a função `startTimer()`, e então entra em um loop While que continua executando-se até que a variável `count` atinja o valor zero. Em cada iteração do loop, imprime o valor atual da variável `count`. Depois de impressão o valor, chama a função `decrementCounter()`.

Essa é a estrutura básica do nosso aplicativo de contagem regressiva em Kotlin. Agora, você pode personalizar esse código para suas necessidades específicas. Por exemplo, você pode adicionar uma interface de usuário para mostrar o valor atual da variável `count`, ou pode usar outros tipos de contagem regressiva, como contagem descritiva ou contagem por intervalos.

Espero que isso ajude! Se você tiver alguma dúvida ou precisar de mais explicações, não hesite em perguntar.