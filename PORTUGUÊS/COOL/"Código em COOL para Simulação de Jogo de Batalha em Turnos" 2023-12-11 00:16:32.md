Claro! Aqui está um exemplo de código em COOL (Classroom Object-Oriented Language) que cria uma simulação de um jogo de batalha em turnos:

```cool
class Personagem {
  attribute nome : String;
  attribute vida : Int;
  attribute ataque : Int;

  method init(nome : String, vida : Int, ataque : Int) : Object {
    self.nome <- nome;
    self.vida <- vida;
    self.ataque <- ataque;
    self;
  };

  method atacar(outro : Personagem) : Object {
    outro.vida <- outro.vida - self.ataque;
    self;
  };

  method estaVivo() : Bool {
    if self.vida > 0 then
      true
    else
      false;
  };
};

class Jogo {
  method iniciarBatalha(jogador1 : String, jogador2 : String, vidaJogador1 : Int, vidaJogador2 : Int, ataqueJogador1 : Int, ataqueJogador2 : Int) : Object {
    let p1 : Personagem <- Personagem(jogador1, vidaJogador1, ataqueJogador1);
    let p2 : Personagem <- Personagem(jogador2, vidaJogador2, ataqueJogador2);

    while p1.estaVivo() && p2.estaVivo() loop
      p1.atacar(p2);
      p2.atacar(p1);
    pool;
    
    if p1.estaVivo() then
      out_string("O jogador " + jogador1 + " venceu!");
    else
      out_string("O jogador " + jogador2 + " venceu!");

    self;
  };
};

class Main {
  main() : Object {
    let jogo : Jogo <- Jogo();
    jogo.iniciarBatalha("Jogador 1", "Jogador 2", 100, 100, 10, 10);

    self;
  };
};
```

Neste código, temos a definição de duas classes: `Personagem` e `Jogo`. A classe `Personagem` representa os personagens que irão batalhar e possui atributos como nome, vida e ataque. Ela também possui os métodos `init` para inicializar os atributos, `atacar` para causar dano em outro personagem e `estaVivo` para verificar se o personagem ainda possui vida.

A classe `Jogo` é responsável por iniciar a batalha entre dois personagens. O método `iniciarBatalha` recebe os parâmetros necessários para criar os personagens e inicia um loop onde os personagens se atacam mutuamente até que um deles fique sem vida. Após o término da batalha, é exibida uma mensagem na tela indicando qual jogador venceu.

Por fim, a classe `Main` contém o método `main` que é o ponto de entrada do programa. Neste exemplo, o jogo é instanciado e a batalha é iniciada entre dois jogadores fictícios.

Esse código é apenas um exemplo e pode ser adaptado e expandido para criar um jogo de batalha mais complexo, com mais personagens, habilidades especiais, entre outros recursos.