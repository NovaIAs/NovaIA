Claro! Aqui está um código complexo em Groovy:

```groovy
import groovy.json.JsonSlurper

// Classe para representar um usuário
class Usuario {
    String nome
    int idade
    
    Usuario(String nome, int idade) {
        this.nome = nome
        this.idade = idade
    }
    
    String toString() {
        return "Nome: ${nome}, Idade: ${idade}"
    }
}

// Método para ler dados de um arquivo JSON
def lerDadosJson(String caminhoArquivo) {
    def jsonSlurper = new JsonSlurper()
    def arquivo = new File(caminhoArquivo)
    def dados = jsonSlurper.parse(arquivo)
    return dados
}

// Método para processar os dados lidos do JSON
def processarDadosJson(dados) {
    def listaUsuarios = []
    
    dados.each { usuarioJson ->
        def nome = usuarioJson.nome
        def idade = usuarioJson.idade
        
        def usuario = new Usuario(nome, idade)
        listaUsuarios.add(usuario)
    }
    
    return listaUsuarios
}

// Método para exibir os usuários processados
def exibirUsuarios(listaUsuarios) {
    listaUsuarios.each { usuario ->
        println(usuario)
    }
}

// Caminho do arquivo JSON
def caminhoArquivo = "dados.json"

// Lendo dados do arquivo JSON
def dadosJson = lerDadosJson(caminhoArquivo)

// Processando dados do JSON
def usuarios = processarDadosJson(dadosJson)

// Exibindo os usuários processados
exibirUsuarios(usuarios)
```

Neste código, estamos criando uma classe `Usuario` que representa um usuário com nome e idade. Em seguida, temos um método `lerDadosJson` responsável por ler os dados de um arquivo JSON. Esse método utiliza a classe `JsonSlurper` para fazer o parsing do arquivo.

Em seguida, temos o método `processarDadosJson` que recebe os dados lidos do JSON e cria objetos `Usuario` com base nesses dados. Os usuários são armazenados em uma lista.

Por fim, temos o método `exibirUsuarios` que percorre a lista de usuários e exibe as informações de cada um.

O código principal lê os dados de um arquivo JSON chamado "dados.json", processa esses dados e exibe os usuários resultantes.

Espero que esse código complexo atenda às suas expectativas!