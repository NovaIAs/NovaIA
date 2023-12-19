import scala.collection.mutable.ArrayBuffer

// Definindo a classe Pessoa
class Pessoa(var nome: String, var idade: Int)

object Main extends App {
  // Criando um objeto do tipo Pessoa
  val pessoa = new Pessoa("João", 25)

  // Função para imprimir o nome e a idade da pessoa
  def imprimirPessoa(p: Pessoa): Unit = {
    println("Nome: " + p.nome)
    println("Idade: " + p.idade)
  }

  // Função para atualizar o nome da pessoa
  def atualizarNome(p: Pessoa, novoNome: String): Unit = {
    p.nome = novoNome
  }

  // Função para atualizar a idade da pessoa
  def atualizarIdade(p: Pessoa, novaIdade: Int): Unit = {
    p.idade = novaIdade
  }

  // Chamando a função imprimirPessoa para exibir os dados iniciais da pessoa
  imprimirPessoa(pessoa)

  // Atualizando o nome da pessoa para "Ana"
  atualizarNome(pessoa, "Ana")

  // Atualizando a idade da pessoa para 30
  atualizarIdade(pessoa, 30)

  // Chamando novamente a função imprimirPessoa para exibir os dados atualizados da pessoa
  imprimirPessoa(pessoa)
}

/* 
  Explicação do código:
  
  - Primeiro definimos a classe Pessoa com os atributos nome e idade.
  - Em seguida, criamos um objeto do tipo Pessoa chamado "pessoa" com nome "João" e idade 25.
  - Após isso, temos três funções: imprimirPessoa, atualizarNome e atualizarIdade.
  - A função imprimirPessoa recebe um objeto do tipo Pessoa como parâmetro e imprime o nome e a idade desta pessoa.
  - A função atualizarNome recebe um objeto do tipo Pessoa e um novo nome como parâmetros e atualiza o nome da pessoa.
  - A função atualizarIdade recebe um objeto do tipo Pessoa e uma nova idade como parâmetros e atualiza a idade da pessoa.
  - No corpo do objeto Main, chamamos a função imprimirPessoa para exibir os dados iniciais da pessoa.
  - Em seguida, chamamos as funções atualizarNome e atualizarIdade para atualizar o nome e a idade da pessoa.
  - Por fim, chamamos novamente a função imprimirPessoa para exibir os dados atualizados da pessoa.
  
  Este código em Scala demonstra o uso de classes, objetos e funções para manipulação de dados de uma pessoa.