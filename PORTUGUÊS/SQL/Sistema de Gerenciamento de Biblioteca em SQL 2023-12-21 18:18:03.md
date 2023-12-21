Claro! Aqui está um código complexo em SQL que envolve várias tabelas, consultas e junções. O código a seguir é um exemplo de um sistema de gerenciamento de uma biblioteca:

```sql
-- Criação das tabelas
CREATE TABLE Livro (
    id INT PRIMARY KEY,
    titulo VARCHAR(100),
    autor VARCHAR(100),
    ano_publicacao INT,
    disponivel BOOLEAN
);

CREATE TABLE Usuario (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    email VARCHAR(100),
    data_nascimento DATE
);

CREATE TABLE Emprestimo (
    id INT PRIMARY KEY,
    id_livro INT,
    id_usuario INT,
    data_emprestimo DATE,
    data_devolucao DATE,
    FOREIGN KEY (id_livro) REFERENCES Livro(id),
    FOREIGN KEY (id_usuario) REFERENCES Usuario(id)
);

-- Inserção de dados
INSERT INTO Livro (id, titulo, autor, ano_publicacao, disponivel)
VALUES (1, 'Dom Casmurro', 'Machado de Assis', 1899, true);

INSERT INTO Livro (id, titulo, autor, ano_publicacao, disponivel)
VALUES (2, 'Grande Sertão: Veredas', 'João Guimarães Rosa', 1956, true);

INSERT INTO Usuario (id, nome, email, data_nascimento)
VALUES (1, 'João Silva', 'joao.silva@example.com', '1990-01-01');

INSERT INTO Usuario (id, nome, email, data_nascimento)
VALUES (2, 'Maria Souza', 'maria.souza@example.com', '1985-05-10');

-- Consulta para verificar livros disponíveis
SELECT titulo, autor
FROM Livro
WHERE disponivel = true;

-- Consulta para verificar livros emprestados por um usuário
SELECT L.titulo, L.autor, E.data_emprestimo, E.data_devolucao
FROM Livro L
JOIN Emprestimo E ON L.id = E.id_livro
JOIN Usuario U ON U.id = E.id_usuario
WHERE U.nome = 'João Silva';

-- Atualização de um livro para indisponível
UPDATE Livro
SET disponivel = false
WHERE id = 1;

-- Exclusão de um usuário e seus respectivos empréstimos
DELETE FROM Usuario
WHERE id = 2;
```

Neste exemplo, criei três tabelas: Livro, Usuario e Emprestimo. A tabela Livro armazena informações sobre os livros, como título, autor, ano de publicação e disponibilidade. A tabela Usuario armazena informações sobre os usuários, como nome, email e data de nascimento. A tabela Emprestimo registra os empréstimos de livros, relacionando o livro emprestado, o usuário que realizou o empréstimo e as datas de empréstimo e devolução.

Após a criação das tabelas, são inseridos alguns dados de exemplo utilizando a cláusula `INSERT INTO`.

Em seguida, são realizadas consultas utilizando a cláusula `SELECT` para verificar os livros disponíveis e os livros emprestados por um usuário específico. As consultas utilizam a cláusula `JOIN` para relacionar as tabelas.

Após as consultas, é realizada uma atualização na tabela Livro para marcar um livro como indisponível, utilizando a cláusula `UPDATE`. Por fim, é realizada a exclusão de um usuário e seus respectivos empréstimos utilizando a cláusula `DELETE FROM`.

Espero que este exemplo tenha sido útil e tenha atendido às suas expectativas! Se precisar de mais algum código, estou à disposição.