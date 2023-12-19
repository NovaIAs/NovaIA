Claro! Aqui está um exemplo de um código SQL complexo que envolve várias tabelas e operações:

```sql
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS clientes (
    id_cliente INT PRIMARY KEY,
    nome_cliente VARCHAR(50),
    endereco_cliente VARCHAR(100)
);

CREATE TABLE IF NOT EXISTS produtos (
    id_produto INT PRIMARY KEY,
    nome_produto VARCHAR(50),
    preco_produto DECIMAL(10, 2)
);

CREATE TABLE IF NOT EXISTS pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

CREATE TABLE IF NOT EXISTS itens_pedido (
    id_item INT PRIMARY KEY,
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);

-- Inserção de dados nas tabelas
INSERT INTO clientes (id_cliente, nome_cliente, endereco_cliente) VALUES
(1, 'João', 'Rua A, 123'),
(2, 'Maria', 'Rua B, 456'),
(3, 'Pedro', 'Rua C, 789');

INSERT INTO produtos (id_produto, nome_produto, preco_produto) VALUES
(1, 'Camisa', 49.99),
(2, 'Calça', 79.99),
(3, 'Tênis', 99.99);

INSERT INTO pedidos (id_pedido, id_cliente, data_pedido) VALUES
(1, 1, '2022-01-01'),
(2, 2, '2022-02-01'),
(3, 3, '2022-03-01');

INSERT INTO itens_pedido (id_item, id_pedido, id_produto, quantidade) VALUES
(1, 1, 1, 2),
(2, 1, 2, 1),
(3, 2, 3, 1),
(4, 3, 1, 3),
(5, 3, 2, 2),
(6, 3, 3, 1);

-- Consulta para obter o valor total de cada pedido
SELECT p.id_pedido, c.nome_cliente, SUM(pr.preco_produto * ip.quantidade) AS valor_total
FROM pedidos p
JOIN clientes c ON p.id_cliente = c.id_cliente
JOIN itens_pedido ip ON p.id_pedido = ip.id_pedido
JOIN produtos pr ON ip.id_produto = pr.id_produto
GROUP BY p.id_pedido, c.nome_cliente;
```

Neste exemplo, o código SQL cria um banco de dados com quatro tabelas: `clientes`, `produtos`, `pedidos` e `itens_pedido`. Em seguida, são inseridos alguns dados nessas tabelas.

A consulta final realiza um join entre todas as tabelas para obter o valor total de cada pedido, somando o preço dos produtos multiplicado pela quantidade de cada item. O resultado é agrupado por pedido e exibe o ID do pedido, o nome do cliente e o valor total.