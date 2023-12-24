Claro! Aqui está um código em COOL (Classroom Object-Oriented Language) que implementa um jogo de adivinhação com números. O objetivo do jogo é adivinhar um número aleatório gerado pelo computador. Vou explicar o código em detalhes para melhor compreensão:

```
class JogoDeAdivinhacao {

   atributo numSecreto : Int <- 0;
   atributo numTentativas : Int <- 0;

   inicializacao() : Object {
      numSecreto <- self.gerarNumeroAleatorio();
      numTentativas <- 0;
      return self;
   };

   gerarNumeroAleatorio() : Int {
      let random : Int <- (objectid Random).nextInt(1, 100);
      return random;
   };

   jogar() : Object {
      let chute : Int <- 0;
      while chute != numSecreto loop
         chute <- self.lerChute();
         numTentativas <- numTentativas + 1;
         if chute < numSecreto then
            out_string("Chute muito baixo!\n");
         else
            out_string("Chute muito alto!\n");
         fi;
      pool;
      out_string("Parabéns! Você adivinhou o número em ");
      out_int(numTentativas);
      out_string(" tentativas.\n");
      return self;
   };

   lerChute() : Int {
      out_string("Digite seu chute: ");
      let chute : Int <- in_int();
      return chute;
   };

};

let jogo : JogoDeAdivinhacao <- (new JogoDeAdivinhacao).inicializacao();
jogo.jogar();
```

Neste código, temos uma classe chamada `JogoDeAdivinhacao` que contém os atributos `numSecreto` e `numTentativas`. O método `inicializacao` é responsável por gerar um número aleatório e inicializar o jogo. O método `gerarNumeroAleatorio` utiliza a classe `Random` para gerar um número aleatório entre 1 e 100.

O método `jogar` é o coração do jogo. Nele, é feito um loop até que o jogador adivinhe o número secreto. A cada iteração do loop, o jogador pode digitar um chute. O número de tentativas é atualizado e uma mensagem é exibida informando se o chute é muito baixo ou muito alto em relação ao número secreto.

Ao sair do loop, é exibida uma mensagem de parabéns, informando o número de tentativas que o jogador precisou para acertar. O método `lerChute` é responsável por ler o chute do jogador, utilizando a função `in_int()`.

Por fim, é criada uma instância da classe `JogoDeAdivinhacao` chamada `jogo` e o método `jogar` é chamado para iniciar o jogo.

Espero que este código atenda às suas expectativas!