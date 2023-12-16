Claro! Aqui está um exemplo de um código complexo em SQL que envolve várias tabelas, subconsultas e funções de agregação para obter resultados específicos:

```sql
-- Criando as tabelas
CREATE TABLE clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(200)
);

CREATE TABLE pedidos (
    id INT PRIMARY KEY,
    cliente_id INT,
    data_pedido DATE,
    valor DECIMAL(10, 2),
    FOREIGN KEY (cliente_id) REFERENCES clientes(id)
);

CREATE TABLE itens_pedido (
    id INT PRIMARY KEY,
    pedido_id INT,
    produto VARCHAR(100),
    quantidade INT,
    valor_unitario DECIMAL(10, 2),
    FOREIGN KEY (pedido_id) REFERENCES pedidos(id)
);

-- Inserindo dados
INSERT INTO clientes (id, nome, endereco) VALUES (1, 'João', 'Rua A');
INSERT INTO clientes (id, nome, endereco) VALUES (2, 'Maria', 'Rua B');
INSERT INTO clientes (id, nome, endereco) VALUES (3, 'Pedro', 'Rua C');

INSERT INTO pedidos (id, cliente_id, data_pedido, valor) VALUES (1, 1, '2021-01-01', 100.00);
INSERT INTO pedidos (id, cliente_id, data_pedido, valor) VALUES (2, 1, '2021-02-01', 200.00);
INSERT INTO pedidos (id, cliente_id, data_pedido, valor) VALUES (3, 2, '2021-03-01', 150.00);
INSERT INTO pedidos (id, cliente_id, data_pedido, valor) VALUES (4, 3, '2021-04-01', 300.00);

INSERT INTO itens_pedido (id, pedido_id, produto, quantidade, valor_unitario) VALUES (1, 1, 'Produto A', 2, 50.00);
INSERT INTO itens_pedido (id, pedido_id, produto, quantidade, valor_unitario) VALUES (2, 1, 'Produto B', 3, 20.00);
INSERT INTO itens_pedido (id, pedido_id, produto, quantidade, valor_unitario) VALUES (3, 2, 'Produto C', 1, 100.00);
INSERT INTO itens_pedido (id, pedido_id, produto, quantidade, valor_unitario) VALUES (4, 3, 'Produto A', 2, 50.00);
INSERT INTO itens_pedido (id, pedido_id, produto, quantidade, valor_unitario) VALUES (5, 3, 'Produto B', 1, 20.00);
INSERT INTO itens_pedido (id, pedido_id, produto, quantidade, valor_unitario) VALUES (6, 4, 'Produto C', 3, 100.00);

-- Consulta complexa
SELECT
    c.nome AS cliente,
    COUNT(DISTINCT p.id) AS total_pedidos,
    SUM(ip.quantidade) AS total_itens,
    SUM(p.valor) AS total_valor
FROM
    clientes c
INNER JOIN
    pedidos p ON c.id = p.cliente_id
INNER JOIN
    itens_pedido ip ON p.id = ip.pedido_id
WHERE
    p.data_pedido >= '2021-01-01' AND p.data_pedido <= '2021-03-31'
GROUP BY
    c.nome
HAVING
    COUNT(DISTINCT p.id) >= 2
ORDER BY
    total_valor DESC;
```

Neste exemplo, criamos três tabelas: clientes, pedidos e itens_pedido. Em seguida, inserimos dados nessas tabelas. A consulta complexa utiliza junções (INNER JOIN) para relacionar as tabelas e obter informações específicas sobre os clientes, pedidos e itens de pedido.

A consulta retorna o nome do cliente, o total de pedidos feitos por cada cliente no primeiro trimestre de 2021, o total de itens pedidos por cliente e o valor total dos pedidos. A cláusula WHERE filtra os pedidos feitos entre os meses de janeiro a março de 2021. A cláusula GROUP BY agrupa os resultados por cliente, e a cláusula HAVING filtra apenas os clientes que fizeram pelo menos dois pedidos. Por fim, a cláusula ORDER BY classifica os resultados pelo valor total em ordem decrescente.

Espero que este exemplo atenda às suas expectativas de complexidade!