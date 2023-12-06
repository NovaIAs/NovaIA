Ok! Aqui está um código complexo em SQL que atende aos seus requisitos. Esse código cria um banco de dados fictício de uma empresa de e-commerce, com várias tabelas e relacionamentos entre elas.

```
-- Criação do banco de dados
CREATE DATABASE EcommerceDB;
USE EcommerceDB;

-- Criação da tabela "Clientes"
CREATE TABLE Clientes (
    id_cliente INT PRIMARY KEY,
    nome VARCHAR(100),
    email VARCHAR(100),
    cpf VARCHAR(11),
    data_nascimento DATE,
    endereco VARCHAR(200),
    cidade VARCHAR(100),
    estado VARCHAR(50)
);

-- Criação da tabela "Pedidos"
CREATE TABLE Pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATETIME,
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (id_cliente) REFERENCES Clientes(id_cliente)
);

-- Criação da tabela "Produtos"
CREATE TABLE Produtos (
    id_produto INT PRIMARY KEY,
    nome VARCHAR(100),
    descricao VARCHAR(200),
    preco DECIMAL(10, 2)
);

-- Criação da tabela "ItensPedido"
CREATE TABLE ItensPedido (
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    subtotal DECIMAL(10, 2),
    FOREIGN KEY (id_pedido) REFERENCES Pedidos(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES Produtos(id_produto)
);

-- Inserção de dados de exemplo na tabela "Clientes"
INSERT INTO Clientes (id_cliente, nome, email, cpf, data_nascimento, endereco, cidade, estado)
VALUES
    (1, 'João Silva', 'joao@example.com', '12345678901', '1990-01-01', 'Rua A, 123', 'São Paulo', 'SP'),
    (2, 'Maria Santos', 'maria@example.com', '98765432101', '1985-05-10', 'Av. B, 456', 'Rio de Janeiro', 'RJ');

-- Inserção de dados de exemplo na tabela "Produtos"
INSERT INTO Produtos (id_produto, nome, descricao, preco)
VALUES
    (1, 'Celular', 'Smartphone novo modelo', 1500.00),
    (2, 'Notebook', 'Notebook de última geração', 3000.00);

-- Inserção de dados de exemplo na tabela "Pedidos"
INSERT INTO Pedidos (id_pedido, id_cliente, data_pedido, valor_total)
VALUES
    (1, 1, '2021-01-01 10:00:00', 2500.00),
    (2, 2, '2021-02-02 12:00:00', 3500.00);

-- Inserção de dados de exemplo na tabela "ItensPedido"
INSERT INTO ItensPedido (id_pedido, id_produto, quantidade, subtotal)
VALUES
    (1, 1, 2, 3000.00),
    (2, 2, 1, 3500.00);

-- Consulta para exibir dados do pedido, seus respectivos itens e informações do cliente
SELECT
    P.id_pedido,
    C.nome AS nome_cliente,
    C.email AS email_cliente,
    PR.nome AS nome_produto,
    PR.preco AS preco_produto,
    IP.quantidade,
    IP.subtotal,
    P.valor_total
FROM
    Pedidos AS P
    INNER JOIN Clientes AS C ON P.id_cliente = C.id_cliente
    INNER JOIN ItensPedido AS IP ON P.id_pedido = IP.id_pedido
    INNER JOIN Produtos AS PR ON IP.id_produto = PR.id_produto;
```

Explicação:

1. Começamos criando o banco de dados chamado "EcommerceDB" usando a instrução `CREATE DATABASE`.
2. Em seguida, utilizamos o comando `USE` para selecionar o banco de dados recém-criado.
3. A tabela "Clientes" é criada com os campos para armazenar informações dos clientes, como nome, email, CPF, data de nascimento, endereço, cidade e estado. O campo "id_cliente" é definido como chave primária.
4. A tabela "Pedidos" é criada com os campos para armazenar informações dos pedidos, como a referência ao cliente, data do pedido, valor total e um campo "id_pedido" como chave primária. É definida uma chave estrangeira "id_cliente" que referencia a tabela "Clientes".
5. A tabela "Produtos" é criada com os campos para armazenar informações dos produtos, como nome, descrição e preço. O campo "id_produto" é definido como chave primária.
6. A tabela "ItensPedido" é criada com os campos para armazenar informações dos itens do pedido, como a referência ao pedido e ao produto, quantidade e subtotal. São definidas duas chaves estrangeiras: "id_pedido" que referencia a tabela "Pedidos" e "id_produto" que referencia a tabela "Produtos".
7. Dados de exemplo são inseridos nas tabelas "Clientes", "Produtos", "Pedidos" e "ItensPedido" utilizando o comando `INSERT INTO`.
8. Por fim, é feita uma consulta utilizando a instrução `SELECT` com `INNER JOIN` para exibir os dados do pedido, incluindo informações do cliente e informações dos produtos presentes no pedido.

Esse código é um exemplo de estrutura de banco de dados de uma empresa de e-commerce, onde é possível armazenar informações sobre clientes, produtos, pedidos e itens do pedido. A consulta final busca exibir os dados do pedido juntamente com as informações do cliente e dos produtos, utilizando os relacionamentos entre as tabelas.