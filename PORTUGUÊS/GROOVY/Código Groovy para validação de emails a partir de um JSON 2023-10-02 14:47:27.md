Claro! Aqui está um código complexo em Groovy que faz uso de diversas funcionalidades da linguagem:

```groovy
import groovy.json.JsonSlurper
import java.util.regex.Pattern

// Função para validar um endereço de e-mail
def validarEmail(String email) {
    def pattern = Pattern.compile("^[_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})\$")
    def matcher = pattern.matcher(email)
    return matcher.matches()
}

// Classe para representar um usuário
class Usuario {
    String nome
    String email
    int idade

    Usuario(String nome, String email, int idade) {
        this.nome = nome
        this.email = email
        this.idade = idade
    }

    String toString() {
        "Nome: $nome, E-mail: $email, Idade: $idade"
    }
}

// Lista de usuários
List<Usuario> usuarios = []

// Dados em formato JSON
def dadosJson = """
    [
        {
            "nome": "João",
            "email": "joao@gmail.com",
            "idade": 25
        },
        {
            "nome": "Maria",
            "email": "maria@outlook.com",
            "idade": 30
        },
        {
            "nome": "Pedro",
            "email": "pedro@gmail.com",
            "idade": 18
        }
    ]
"""

// Parse do JSON
def jsonSlurper = new JsonSlurper()
def dados = jsonSlurper.parseText(dadosJson)

// Criar instâncias de usuário a partir dos dados do JSON
dados.each { dado ->
    if (validarEmail(dado.email)) {
        Usuario usuario = new Usuario(dado.nome, dado.email, dado.idade)
        usuarios.add(usuario)
    }
}

// Imprimir a lista de usuários cadastrados
println "Usuários cadastrados:"
usuarios.each { usuario ->
    println usuario.toString()
}
```

Esse código faz o seguinte:

1. Importa as classes necessárias, incluindo o `JsonSlurper` para fazer o parse de um JSON.
2. Define uma função `validarEmail` que utiliza uma expressão regular para validar um endereço de e-mail.
3. Define uma classe `Usuario` que representa um usuário com nome, e-mail e idade.
4. Cria uma lista vazia de usuários.
5. Define uma string `dadosJson` que contém dados de usuários em formato JSON.
6. Faz o parse do JSON utilizando o `JsonSlurper`.
7. Cria instâncias de `Usuario` a partir dos dados do JSON, validando o e-mail antes de adicionar o usuário à lista.
8. Imprime a lista de usuários cadastrados.

Esse código demonstra o uso de classes, funções, expressões regulares e parse de JSON em Groovy.