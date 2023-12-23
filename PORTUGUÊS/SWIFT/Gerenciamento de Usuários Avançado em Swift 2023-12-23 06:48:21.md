Claro! Aqui está um código complexo em Swift que utiliza uma série de conceitos avançados:

```swift
import Foundation

// Definindo uma estrutura de dados para representar um usuário
struct Usuario {
    var nome: String
    var idade: Int
    var email: String
}

// Criando uma função para gerar um número aleatório entre dois valores
func gerarNumeroAleatorio(inicio: Int, fim: Int) -> Int {
    return Int.random(in: inicio...fim)
}

// Criando uma classe responsável por gerenciar os usuários
class GerenciadorUsuarios {
    var usuarios: [Usuario] = []
    
    // Função para adicionar um novo usuário à lista
    func adicionarUsuario(usuario: Usuario) {
        usuarios.append(usuario)
        print("Usuário \(usuario.nome) adicionado com sucesso!")
    }
    
    // Função para buscar usuários com uma determinada idade
    func buscarUsuariosPorIdade(idade: Int) -> [Usuario] {
        let usuariosEncontrados = usuarios.filter { $0.idade == idade }
        return usuariosEncontrados
    }
    
    // Função para enviar um e-mail para todos os usuários
    func enviarEmailParaUsuarios(mensagem: String) {
        for usuario in usuarios {
            enviarEmail(email: usuario.email, mensagem: mensagem)
        }
    }
    
    // Função auxiliar para enviar um e-mail
    private func enviarEmail(email: String, mensagem: String) {
        print("Enviando e-mail para \(email): \(mensagem)")
    }
}

// Criando uma instância do gerenciador de usuários
let gerenciador = GerenciadorUsuarios()

// Adicionando alguns usuários
gerenciador.adicionarUsuario(usuario: Usuario(nome: "João", idade: 25, email: "joao@example.com"))
gerenciador.adicionarUsuario(usuario: Usuario(nome: "Maria", idade: 30, email: "maria@example.com"))
gerenciador.adicionarUsuario(usuario: Usuario(nome: "Pedro", idade: 25, email: "pedro@example.com"))

// Buscando usuários com idade 25
let usuariosEncontrados = gerenciador.buscarUsuariosPorIdade(idade: 25)
print("Usuários encontrados com idade 25:")
for usuario in usuariosEncontrados {
    print(usuario.nome)
}

// Enviando um e-mail para todos os usuários
gerenciador.enviarEmailParaUsuarios(mensagem: "Olá! Bem-vindo ao nosso aplicativo!")

```

Este código em Swift cria uma estrutura de dados `Usuario` que possui as propriedades `nome`, `idade` e `email`. Em seguida, é criada a classe `GerenciadorUsuarios`, responsável por gerenciar os usuários.

A classe `GerenciadorUsuarios` possui métodos para adicionar um novo usuário à lista, buscar usuários por idade e enviar um e-mail para todos os usuários. Para gerar um número aleatório, é utilizado o método `gerarNumeroAleatorio`.

O código cria uma instância do `GerenciadorUsuarios` chamado `gerenciador` e adiciona alguns usuários. Em seguida, busca os usuários com idade 25 e envia um e-mail para todos eles.

Espero que este código complexo em Swift possa lhe ajudar a entender melhor os conceitos avançados da linguagem e suas possibilidades de uso.