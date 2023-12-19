Claro! Vou criar um código complexo em SQL para você. Este código irá criar um banco de dados fictício para gerenciar uma livraria. Vou usar a linguagem SQL padrão, mas vou adicionar alguns recursos adicionais para tornar o código mais interessante.

Aqui está o código:

```
-- Criar o banco de dados
CREATE DATABASE Livraria;

-- Usar o banco de dados
USE Livraria;

-- Criar a tabela "Livros"
CREATE TABLE Livros (
    id INT PRIMARY KEY,
    titulo VARCHAR(100),
    autor VARCHAR(100),
    ano_publicacao INT,
    preco DECIMAL(10, 2),
    estoque INT
);

-- Criar a tabela "Clientes"
CREATE TABLE Clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    email VARCHAR(100),
    endereco VARCHAR(200)
);

-- Criar a tabela "Vendas"
CREATE TABLE Vendas (
    id INT PRIMARY KEY,
    id_cliente INT,
    data_venda DATE,
    total DECIMAL(10, 2),
    FOREIGN KEY (id_cliente) REFERENCES Clientes(id)
);

-- Inserir alguns dados de exemplo na tabela "Livros"
INSERT INTO Livros (id, titulo, autor, ano_publicacao, preco, estoque)
VALUES
    (1, 'Dom Casmurro', 'Machado de Assis', 1899, 25.99, 10),
    (2, 'O Grande Gatsby', 'F. Scott Fitzgerald', 1925, 29.99, 5),
    (3, '1984', 'George Orwell', 1949, 19.99, 15),
    (4, 'Cem Anos de Solidão', 'Gabriel García Márquez', 1967, 34.99, 8),
    (5, 'Orgulho e Preconceito', 'Jane Austen', 1813, 27.99, 12);

-- Inserir alguns dados de exemplo na tabela "Clientes"
INSERT INTO Clientes (id, nome, email, endereco)
VALUES
    (1, 'João da Silva', 'joao@example.com', 'Rua A, 123'),
    (2, 'Maria dos Santos', 'maria@example.com', 'Avenida B, 456'),
    (3, 'José Oliveira', 'jose@example.com', 'Travessa C, 789');

-- Simular uma venda
DECLARE @id_venda INT;
SET @id_venda = 1;

INSERT INTO Vendas (id, id_cliente, data_venda, total)
VALUES
    (@id_venda, 1, GETDATE(), 79.97); -- Total da venda: 3 livros * 25.99

-- Atualizar estoque após a venda
UPDATE Livros
SET estoque = estoque - 3
WHERE id IN (SELECT id FROM Vendas WHERE id = @id_venda);

-- Consultar dados de exemplo
SELECT
    Livros.titulo,
    Livros.autor,
    Clientes.nome AS nome_cliente,
    Vendas.data_venda,
    Vendas.total
FROM
    Livros
    INNER JOIN Vendas ON Livros.id = Vendas.id
    INNER JOIN Clientes ON Vendas.id_cliente = Clientes.id;
```

Este código cria um banco de dados chamado "Livraria" e define três tabelas: "Livros", "Clientes" e "Vendas". A tabela "Livros" armazena informações sobre os livros disponíveis na livraria, como título, autor, ano de publicação, preço e estoque. A tabela "Clientes" armazena informações sobre os clientes, como nome, e-mail e endereço. A tabela "Vendas" registra as vendas realizadas, incluindo o ID do cliente, a data da venda e o total.

O código também insere alguns dados de exemplo nas tabelas "Livros" e "Clientes" e simula uma venda, inserindo um registro na tabela "Vendas". Em seguida, atualiza o estoque dos livros vendidos e realiza uma consulta para exibir informações sobre os livros, clientes e vendas.

Observe que este código é apenas um exemplo e pode ser adaptado e expandido de acordo com as necessidades do sistema de gerenciamento de livraria.