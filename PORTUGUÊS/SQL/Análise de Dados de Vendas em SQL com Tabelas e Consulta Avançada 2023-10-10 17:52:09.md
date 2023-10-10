Claro! Aqui está um código complexo em SQL que realiza uma análise de dados de vendas em uma base de dados fictícia de uma loja de varejo.

```
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS Clientes (
    id_cliente INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(200),
    telefone VARCHAR(20)
);

CREATE TABLE IF NOT EXISTS Produtos (
    id_produto INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10, 2),
    estoque INT
);

CREATE TABLE IF NOT EXISTS Pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    FOREIGN KEY (id_cliente) REFERENCES Clientes(id_cliente)
);

CREATE TABLE IF NOT EXISTS ItensPedidos (
    id_item INT PRIMARY KEY,
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    preco_unitario DECIMAL(10, 2),
    FOREIGN KEY (id_pedido) REFERENCES Pedidos(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES Produtos(id_produto)
);

-- Inserção de dados nas tabelas
INSERT INTO Clientes (id_cliente, nome, endereco, telefone)
VALUES 
    (1, 'João da Silva', 'Rua A, 123', '123456789'),
    (2, 'Maria Santos', 'Av. B, 456', '987654321');

INSERT INTO Produtos (id_produto, nome, preco, estoque)
VALUES 
    (1, 'Camiseta', 29.90, 100),
    (2, 'Calça Jeans', 99.90, 50);

INSERT INTO Pedidos (id_pedido, id_cliente, data_pedido)
VALUES 
    (1, 1, '2022-01-01'),
    (2, 2, '2022-01-02');

INSERT INTO ItensPedidos (id_item, id_pedido, id_produto, quantidade, preco_unitario)
VALUES 
    (1, 1, 1, 2, 29.90),
    (2, 1, 2, 1, 99.90),
    (3, 2, 2, 3, 99.90);

-- Consulta avançada
SELECT
    c.nome AS nome_cliente,
    SUM(ip.quantidade) AS total_itens,
    SUM(ip.quantidade * ip.preco_unitario) AS valor_total
FROM
    Clientes c
    JOIN Pedidos p ON c.id_cliente = p.id_cliente
    JOIN ItensPedidos ip ON p.id_pedido = ip.id_pedido
GROUP BY
    c.nome;
```

Neste código em SQL, criamos quatro tabelas: "Clientes" para armazenar informações dos clientes, "Produtos" para armazenar informações dos produtos, "Pedidos" para armazenar informações dos pedidos e "ItensPedidos" para armazenar os itens de cada pedido.

Em seguida, inserimos alguns dados nas tabelas para simular uma situação real. Foram inseridos dois clientes, dois produtos, dois pedidos e três itens de pedido.

Por fim, realizamos uma consulta avançada que retorna o nome do cliente, a quantidade total de itens e o valor total gasto por cada cliente em todos os pedidos. Utilizamos as cláusulas JOIN para relacionar as tabelas corretamente e a cláusula GROUP BY para agrupar os resultados por cliente.

Espero que este código SQL complexo atenda às suas expectativas!