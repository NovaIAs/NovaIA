Claro! Aqui está um exemplo de um código SQL complexo que envolve várias tabelas e recursos avançados, como junções, subconsultas, funções agregadas e cláusulas condicionais.

```sql
-- Criação das tabelas
CREATE TABLE clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    cidade VARCHAR(50)
);

CREATE TABLE pedidos (
    id INT PRIMARY KEY,
    data_pedido DATE,
    cliente_id INT,
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (cliente_id) REFERENCES clientes(id)
);

CREATE TABLE produtos (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    preco DECIMAL(10, 2)
);

CREATE TABLE itens_pedido (
    id INT PRIMARY KEY,
    pedido_id INT,
    produto_id INT,
    quantidade INT,
    FOREIGN KEY (pedido_id) REFERENCES pedidos(id),
    FOREIGN KEY (produto_id) REFERENCES produtos(id)
);

-- Inserção de dados nas tabelas
INSERT INTO clientes (id, nome, cidade) VALUES
    (1, 'João', 'São Paulo'),
    (2, 'Maria', 'Rio de Janeiro'),
    (3, 'Pedro', 'Belo Horizonte');

INSERT INTO pedidos (id, data_pedido, cliente_id, valor_total) VALUES
    (1, '2021-01-01', 1, 100.00),
    (2, '2021-02-15', 2, 250.00),
    (3, '2021-03-10', 3, 80.00);

INSERT INTO produtos (id, nome, preco) VALUES
    (1, 'Camiseta', 50.00),
    (2, 'Calça', 80.00),
    (3, 'Tênis', 120.00);

INSERT INTO itens_pedido (id, pedido_id, produto_id, quantidade) VALUES
    (1, 1, 1, 2),
    (2, 2, 2, 1),
    (3, 2, 3, 1),
    (4, 3, 1, 3);

-- Consulta complexa para obter informações detalhadas de pedidos
SELECT
    p.id AS pedido_id,
    c.nome AS nome_cliente,
    c.cidade AS cidade_cliente,
    p.data_pedido,
    SUM(ip.quantidade * pr.preco) AS valor_total_pedido
FROM
    pedidos p
    JOIN clientes c ON p.cliente_id = c.id
    JOIN itens_pedido ip ON p.id = ip.pedido_id
    JOIN produtos pr ON ip.produto_id = pr.id
WHERE
    p.data_pedido >= '2021-01-01' AND p.data_pedido <= '2021-12-31'
GROUP BY
    p.id, c.nome, c.cidade, p.data_pedido
HAVING
    SUM(ip.quantidade * pr.preco) > 100.00
ORDER BY
    p.data_pedido ASC;
```

Nesse código SQL, começamos criando as tabelas `clientes`, `pedidos`, `produtos` e `itens_pedido`, que representam a estrutura básica de um sistema de pedidos de uma loja virtual.

Em seguida, inserimos alguns dados fictícios nas tabelas para exemplificar a consulta.

A consulta complexa é realizada na última parte do código. Ela retorna informações detalhadas de pedidos, incluindo o ID do pedido, o nome do cliente, a cidade do cliente, a data do pedido e o valor total do pedido. A cláusula `JOIN` é usada para unir as tabelas relacionadas, enquanto a cláusula `WHERE` filtra os pedidos por uma faixa de datas específica. A cláusula `GROUP BY` agrupa os resultados por pedido e, em seguida, a cláusula `HAVING` filtra os pedidos cujo valor total seja superior a 100.00. Por fim, a cláusula `ORDER BY` ordena os resultados pela data do pedido em ordem ascendente.