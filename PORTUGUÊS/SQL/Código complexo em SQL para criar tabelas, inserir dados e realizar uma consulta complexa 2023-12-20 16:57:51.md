Claro! Aqui está um código complexo em SQL, que envolve várias tabelas e consultas complexas:

```
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS clientes (
    id_cliente INT PRIMARY KEY,
    nome VARCHAR(100),
    email VARCHAR(100),
    telefone VARCHAR(20)
);

CREATE TABLE IF NOT EXISTS pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

CREATE TABLE IF NOT EXISTS itens_pedido (
    id_item INT PRIMARY KEY,
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    valor_unitario DECIMAL(10, 2),
    FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);

CREATE TABLE IF NOT EXISTS produtos (
    id_produto INT PRIMARY KEY,
    nome VARCHAR(100),
    descricao TEXT,
    preco DECIMAL(10, 2)
);

-- Inserção de dados nas tabelas
INSERT INTO clientes (id_cliente, nome, email, telefone)
VALUES (1, 'João Silva', 'joao.silva@example.com', '(11) 99999-9999');

INSERT INTO produtos (id_produto, nome, descricao, preco)
VALUES (1, 'Camiseta', 'Camiseta preta tamanho M', 39.90),
       (2, 'Calça Jeans', 'Calça Jeans azul tamanho 42', 99.90),
       (3, 'Tênis', 'Tênis branco tamanho 40', 149.90);

INSERT INTO pedidos (id_pedido, id_cliente, data_pedido, valor_total)
VALUES (1, 1, '2022-01-01', 189.80);

INSERT INTO itens_pedido (id_item, id_pedido, id_produto, quantidade, valor_unitario)
VALUES (1, 1, 1, 2, 39.90),
       (2, 1, 2, 1, 99.90);

-- Consulta para obter o nome do cliente, data do pedido, nome dos produtos e valor total de cada pedido
SELECT c.nome AS nome_cliente, 
       p.data_pedido, 
       pr.nome AS nome_produto, 
       i.quantidade, 
       i.valor_unitario,
       (i.quantidade * i.valor_unitario) AS valor_total_item,
       (SELECT SUM(itens_pedido.quantidade * itens_pedido.valor_unitario) 
        FROM itens_pedido 
        WHERE itens_pedido.id_pedido = p.id_pedido) AS valor_total_pedido
FROM clientes c
INNER JOIN pedidos p ON c.id_cliente = p.id_cliente
INNER JOIN itens_pedido i ON p.id_pedido = i.id_pedido
INNER JOIN produtos pr ON i.id_produto = pr.id_produto;
```

Este código em SQL cria quatro tabelas: `clientes`, `pedidos`, `itens_pedido` e `produtos`. Em seguida, insere alguns dados nessas tabelas. 

A consulta final retorna o nome do cliente, a data do pedido, o nome dos produtos, a quantidade, o valor unitário de cada item do pedido e o valor total de cada pedido. O valor total do pedido é calculado através de uma subconsulta que soma o valor total de todos os itens do pedido.