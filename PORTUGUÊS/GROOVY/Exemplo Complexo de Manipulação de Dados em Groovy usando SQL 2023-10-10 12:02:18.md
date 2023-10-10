Claro! Aqui está um código complexo em Groovy:

```groovy
// Este é um exemplo de código complexo em Groovy que envolve manipulação de dados e estruturas de controle avançadas.

import groovy.sql.Sql

class ComplexoGroovy {
    static main(args) {
        // Criar uma conexão com um banco de dados
        def sql = Sql.newInstance("jdbc:mysql://localhost:3306/meu_banco", "usuario", "senha", "com.mysql.jdbc.Driver")

        try {
            // Criar uma tabela no banco de dados
            sql.execute("CREATE TABLE IF NOT EXISTS usuarios (id INT, nome VARCHAR(50))")

            // Inserir alguns registros na tabela
            sql.execute("INSERT INTO usuarios (id, nome) VALUES (1, 'João')")
            sql.execute("INSERT INTO usuarios (id, nome) VALUES (2, 'Maria')")
            sql.execute("INSERT INTO usuarios (id, nome) VALUES (3, 'Pedro')")

            // Consultar os registros da tabela
            def usuarios = sql.rows("SELECT * FROM usuarios")

            // Percorrer os registros e exibir na tela
            usuarios.each { usuario ->
                println("ID: ${usuario.id}, Nome: ${usuario.nome}")
            }

            // Atualizar um registro na tabela
            sql.execute("UPDATE usuarios SET nome = 'Carlos' WHERE id = 1")

            // Consultar novamente os registros atualizados
            usuarios = sql.rows("SELECT * FROM usuarios")

            // Exibir os registros atualizados na tela
            usuarios.each { usuario ->
                println("ID: ${usuario.id}, Nome: ${usuario.nome}")
            }

            // Deletar um registro da tabela
            sql.execute("DELETE FROM usuarios WHERE id = 2")

            // Consultar novamente os registros após a exclusão
            usuarios = sql.rows("SELECT * FROM usuarios")

            // Exibir os registros atualizados na tela
            usuarios.each { usuario ->
                println("ID: ${usuario.id}, Nome: ${usuario.nome}")
            }
        } finally {
            // Fechar a conexão com o banco de dados
            sql.close()
        }
    }
}
```

Este código em Groovy demonstra o uso da biblioteca `groovy.sql.Sql` para criar uma conexão com um banco de dados MySQL, criar uma tabela, inserir registros, consultar registros, atualizar registros e excluir registros.

Primeiro, o código cria uma conexão com o banco de dados usando o método `Sql.newInstance`. Em seguida, ele cria uma tabela chamada "usuarios" (se ainda não existir) usando o método `execute`. Depois disso, ele insere alguns registros na tabela usando o método `execute`. 

Em seguida, ele consulta os registros da tabela usando o método `rows`. Essa consulta retorna um objeto `ResultSet`, que é percorrido usando o método `each`. Dentro do loop, os valores do ID e do nome de cada registro são exibidos na tela usando `println`.

Depois disso, o código atualiza um registro na tabela usando o método `execute` com uma instrução SQL de atualização. Em seguida, ele consulta novamente os registros atualizados e os exibe na tela.

Por fim, o código exclui um registro da tabela usando o método `execute` com uma instrução SQL de exclusão. Mais uma vez, ele consulta os registros restantes e os exibe na tela.

É importante notar que este código usa apenas exemplos simples e não aborda todos os casos possíveis. Ele serve apenas como um exemplo de código complexo em Groovy que envolve manipulação de dados e estruturas de controle avançadas.