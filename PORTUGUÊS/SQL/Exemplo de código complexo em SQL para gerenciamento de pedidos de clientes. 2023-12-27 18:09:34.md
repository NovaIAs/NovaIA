Aqui está um exemplo de código complexo em SQL que realiza uma ampla gama de operações:

```sql
-- Criação da tabela "Clientes"
CREATE TABLE Clientes (
    id_cliente INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(200),
    telefone VARCHAR(20),
    email VARCHAR(100)
);

-- Criação da tabela "Pedidos"
CREATE TABLE Pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (id_cliente) REFERENCES Clientes(id_cliente)
);

-- Criação da tabela "ItensPedido"
CREATE TABLE ItensPedido (
    id_item INT PRIMARY KEY,
    id_pedido INT,
    descricao VARCHAR(200),
    quantidade INT,
    preco_unitario DECIMAL(10, 2),
    FOREIGN KEY (id_pedido) REFERENCES Pedidos(id_pedido)
);

-- Inserção de dados na tabela "Clientes"
INSERT INTO Clientes (id_cliente, nome, endereco, telefone, email)
VALUES (1, 'João da Silva', 'Rua A, 123', '(11) 1234-5678', 'joao.silva@email.com'),
       (2, 'Maria Souza', 'Av. B, 456', '(22) 9876-5432', 'maria.souza@email.com');

-- Inserção de dados na tabela "Pedidos"
INSERT INTO Pedidos (id_pedido, id_cliente, data_pedido, valor_total)
VALUES (1, 1, '2021-01-01', 1000.00),
       (2, 2, '2021-02-01', 1500.00);

-- Inserção de dados na tabela "ItensPedido"
INSERT INTO ItensPedido (id_item, id_pedido, descricao, quantidade, preco_unitario)
VALUES (1, 1, 'Item 1 - Pedido 1', 2, 50.00),
       (2, 1, 'Item 2 - Pedido 1', 3, 30.00),
       (3, 2, 'Item 1 - Pedido 2', 1, 100.00),
       (4, 2, 'Item 2 - Pedido 2', 2, 70.00);

-- Seleção de todos os clientes e seus pedidos
SELECT 
    c.id_cliente,
    c.nome,
    c.endereco,
    c.telefone,
    c.email,
    p.id_pedido,
    p.data_pedido,
    p.valor_total
FROM 
    Clientes c
    INNER JOIN Pedidos p ON c.id_cliente = p.id_cliente;

-- Atualização do valor total de um pedido específico
UPDATE Pedidos
SET valor_total = 2000.00
WHERE id_pedido = 1;

-- Remoção de um item de pedido específico
DELETE FROM ItensPedido
WHERE id_item = 4;

-- Cálculo do valor total de todos os pedidos de cada cliente
SELECT
    c.id_cliente,
    c.nome,
    SUM(p.valor_total) AS valor_total_pedidos
FROM
    Clientes c
    INNER JOIN Pedidos p ON c.id_cliente = p.id_cliente
GROUP BY
    c.id_cliente,
    c.nome;

-- Exclusão de todos os pedidos de um cliente específico
DELETE FROM Pedidos
WHERE id_cliente = 1;

-- Exclusão da tabela "ItensPedido"
DROP TABLE ItensPedido;

-- Exclusão da tabela "Pedidos"
DROP TABLE Pedidos;

-- Exclusão da tabela "Clientes"
DROP TABLE Clientes;
```

Este código em SQL cria três tabelas: "Clientes", "Pedidos" e "ItensPedido". Em seguida, insere dados nessas tabelas, realiza consultas, atualizações e exclusões. 

Ele simula um sistema de gerenciamento de pedidos, onde os clientes são cadastrados na tabela "Clientes" e os pedidos são registrados na tabela "Pedidos". A tabela "ItensPedido" armazena os itens de cada pedido.

O código inclui exemplos de consultas utilizando JOINs para relacionar as tabelas, atualizar valores, excluir registros e calcular o valor total dos pedidos por cliente.

Por fim, o código remove as tabelas criadas. Essa é apenas uma representação complexa e completa de um código SQL, mas sua aplicação pode variar de acordo com as necessidades do projeto.