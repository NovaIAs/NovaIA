Claro! Aqui está um código em SQL que envolve várias tabelas e consultas complexas:

```sql
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(100),
    telefone VARCHAR(15)
);

CREATE TABLE IF NOT EXISTS pedidos (
    id INT PRIMARY KEY,
    cliente_id INT,
    data_pedido DATE,
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (cliente_id) REFERENCES clientes(id)
);

CREATE TABLE IF NOT EXISTS produtos (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10, 2)
);

CREATE TABLE IF NOT EXISTS itens_pedido (
    id INT PRIMARY KEY,
    pedido_id INT,
    produto_id INT,
    quantidade INT,
    FOREIGN KEY (pedido_id) REFERENCES pedidos(id),
    FOREIGN KEY (produto_id) REFERENCES produtos(id)
);

-- Inserção de dados nas tabelas
INSERT INTO clientes (id, nome, endereco, telefone)
VALUES (1, 'João Silva', 'Rua A, 123', '9999-1234'),
       (2, 'Maria Santos', 'Av. B, 456', '8888-5678'),
       (3, 'Pedro Oliveira', 'Rua C, 789', '7777-8901');

INSERT INTO pedidos (id, cliente_id, data_pedido, valor_total)
VALUES (1, 1, '2021-01-01', 100.00),
       (2, 2, '2021-01-02', 150.00),
       (3, 3, '2021-01-03', 200.00);

INSERT INTO produtos (id, nome, preco)
VALUES (1, 'Produto A', 10.00),
       (2, 'Produto B', 20.00),
       (3, 'Produto C', 30.00);

INSERT INTO itens_pedido (id, pedido_id, produto_id, quantidade)
VALUES (1, 1, 1, 5),
       (2, 1, 2, 3),
       (3, 2, 1, 2),
       (4, 3, 3, 4);

-- Consulta complexa
SELECT c.nome AS nome_cliente, p.data_pedido, pr.nome AS nome_produto, ip.quantidade, pr.preco, (ip.quantidade * pr.preco) AS subtotal
FROM clientes c
INNER JOIN pedidos p ON c.id = p.cliente_id
INNER JOIN itens_pedido ip ON p.id = ip.pedido_id
INNER JOIN produtos pr ON ip.produto_id = pr.id
WHERE p.data_pedido >= '2021-01-01' AND p.data_pedido <= '2021-01-03'
ORDER BY c.nome, p.data_pedido;
```

Neste exemplo, criamos quatro tabelas: `clientes`, `pedidos`, `produtos` e `itens_pedido`. Em seguida, inserimos dados nessas tabelas.

A consulta complexa realiza um `SELECT` que retorna o nome do cliente, a data do pedido, o nome do produto, a quantidade, o preço e o subtotal (quantidade * preço) para os pedidos realizados entre 01/01/2021 e 03/01/2021, ordenados por nome do cliente e data do pedido.

Essa consulta utiliza `INNER JOIN` para relacionar as tabelas e obter as informações necessárias para a exibição do resultado final.