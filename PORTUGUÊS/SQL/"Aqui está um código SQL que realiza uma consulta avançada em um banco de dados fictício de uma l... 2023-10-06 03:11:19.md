Claro! Aqui está um código complexo em SQL, que realiza uma consulta avançada em um banco de dados fictício de uma loja e retorna informações detalhadas sobre os produtos vendidos:

```
-- Criação das tabelas

CREATE TABLE IF NOT EXISTS clientes (
    id_cliente INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(200),
    telefone VARCHAR(15)
);

CREATE TABLE IF NOT EXISTS produtos (
    id_produto INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10, 2),
    quantidade_estoque INT
);

CREATE TABLE IF NOT EXISTS pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

CREATE TABLE IF NOT EXISTS itens_pedido (
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);

-- Inserção de dados nas tabelas

INSERT INTO clientes (id_cliente, nome, endereco, telefone)
VALUES
    (1, 'João da Silva', 'Rua A, 123', '111111111'),
    (2, 'Maria Oliveira', 'Av. B, 456', '222222222'),
    (3, 'Pedro Santos', 'Rua C, 789', '333333333');

INSERT INTO produtos (id_produto, nome, preco, quantidade_estoque)
VALUES
    (1, 'Camiseta', 39.90, 100),
    (2, 'Calça Jeans', 99.90, 50),
    (3, 'Tênis', 199.90, 20);

INSERT INTO pedidos (id_pedido, id_cliente, data_pedido)
VALUES
    (1, 1, '2021-01-01'),
    (2, 2, '2021-02-05'),
    (3, 3, '2021-03-10');

INSERT INTO itens_pedido (id_pedido, id_produto, quantidade)
VALUES
    (1, 1, 2),
    (1, 2, 1),
    (2, 1, 3),
    (2, 3, 2),
    (3, 2, 1);

-- Consulta avançada

SELECT
    c.nome AS nome_cliente,
    p.nome AS nome_produto,
    ip.quantidade,
    p.preco,
    ip.quantidade * p.preco AS total
FROM
    clientes c
    INNER JOIN pedidos pd ON c.id_cliente = pd.id_cliente
    INNER JOIN itens_pedido ip ON pd.id_pedido = ip.id_pedido
    INNER JOIN produtos p ON ip.id_produto = p.id_produto
WHERE
    p.preco >= 50.00
ORDER BY
    total DESC;
```

Este código em SQL cria e popula um banco de dados fictício com tabelas para clientes, produtos, pedidos e itens de pedido. 

Em seguida, ele realiza uma consulta avançada que retorna o nome do cliente, nome do produto, quantidade, preço unitário e total (quantidade * preço) para todos os itens de pedido onde o preço do produto seja igual ou superior a 50.00. Os resultados são ordenados pelo total em ordem decrescente.

Espero que este código complexo em SQL seja útil para você! Caso tenha alguma dúvida, estou à disposição para ajudar.