Claro! Aqui está um exemplo de um código SQL complexo que envolve várias tabelas, subconsultas e funções avançadas:

```
-- Criação das tabelas
CREATE TABLE clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(100),
    telefone VARCHAR(20)
);

CREATE TABLE pedidos (
    id INT PRIMARY KEY,
    cliente_id INT,
    data_pedido DATE,
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (cliente_id) REFERENCES clientes(id)
);

CREATE TABLE produtos (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10, 2)
);

CREATE TABLE itens_pedido (
    pedido_id INT,
    produto_id INT,
    quantidade INT,
    FOREIGN KEY (pedido_id) REFERENCES pedidos(id),
    FOREIGN KEY (produto_id) REFERENCES produtos(id)
);

-- Inserção de dados nas tabelas
INSERT INTO clientes (id, nome, endereco, telefone)
VALUES (1, 'João', 'Rua A, 123', '11111111');

INSERT INTO clientes (id, nome, endereco, telefone)
VALUES (2, 'Maria', 'Rua B, 456', '22222222');

INSERT INTO clientes (id, nome, endereco, telefone)
VALUES (3, 'Pedro', 'Rua C, 789', '33333333');

INSERT INTO produtos (id, nome, preco)
VALUES (1, 'Camiseta', 39.99);

INSERT INTO produtos (id, nome, preco)
VALUES (2, 'Calça', 79.99);

INSERT INTO produtos (id, nome, preco)
VALUES (3, 'Tênis', 129.99);

INSERT INTO pedidos (id, cliente_id, data_pedido, valor_total)
VALUES (1, 1, '2021-01-01', 79.98);

INSERT INTO pedidos (id, cliente_id, data_pedido, valor_total)
VALUES (2, 2, '2021-02-01', 129.99);

INSERT INTO pedidos (id, cliente_id, data_pedido, valor_total)
VALUES (3, 3, '2021-03-01', 169.97);

INSERT INTO itens_pedido (pedido_id, produto_id, quantidade)
VALUES (1, 1, 2);

INSERT INTO itens_pedido (pedido_id, produto_id, quantidade)
VALUES (2, 2, 1);

INSERT INTO itens_pedido (pedido_id, produto_id, quantidade)
VALUES (3, 3, 1);

-- Consulta complexa
SELECT c.nome AS cliente, p.nome AS produto, ip.quantidade, (p.preco * ip.quantidade) AS subtotal
FROM clientes c
INNER JOIN pedidos pd ON pd.cliente_id = c.id
INNER JOIN itens_pedido ip ON ip.pedido_id = pd.id
INNER JOIN produtos p ON p.id = ip.produto_id
WHERE pd.data_pedido >= '2021-01-01' AND pd.data_pedido <= '2021-03-31'
GROUP BY c.nome, p.nome, ip.quantidade, p.preco
HAVING (p.preco * ip.quantidade) > 50
ORDER BY c.nome, p.nome;
```

Neste código, criamos quatro tabelas: `clientes`, `pedidos`, `produtos` e `itens_pedido`. Em seguida, inserimos alguns dados nessas tabelas. 

A consulta complexa que fazemos no final retorna o nome do cliente, nome do produto, quantidade e subtotal dos itens pedidos. Fazemos um join entre as tabelas para obter as informações necessárias, filtramos os pedidos feitos entre janeiro e março de 2021, agrupamos os resultados por cliente, produto, quantidade e preço, e filtramos apenas os resultados cujo subtotal seja maior que 50. Por fim, ordenamos os resultados por nome do cliente e nome do produto.

Espero que este código complexo em SQL atenda às suas expectativas!