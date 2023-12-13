Claro! Aqui está um exemplo de um código complexo em SQL que envolve várias tabelas e consultas avançadas. O código cria um banco de dados fictício de uma loja de e-commerce e realiza uma análise de vendas.

```sql
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS clientes (
    id_cliente INT PRIMARY KEY,
    nome VARCHAR(100),
    email VARCHAR(100),
    data_nascimento DATE
);

CREATE TABLE IF NOT EXISTS produtos (
    id_produto INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10, 2),
    descricao TEXT
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


-- Inserção de dados
INSERT INTO clientes (id_cliente, nome, email, data_nascimento)
VALUES (1, 'João da Silva', 'joao@email.com', '1990-01-01');

INSERT INTO produtos (id_produto, nome, preco, descricao)
VALUES (1, 'Camiseta', 29.99, 'Camiseta de algodão');

INSERT INTO pedidos (id_pedido, id_cliente, data_pedido)
VALUES (1, 1, '2021-01-01');

INSERT INTO itens_pedido (id_pedido, id_produto, quantidade)
VALUES (1, 1, 2);


-- Consultas
-- Total de vendas por produto
SELECT p.nome, SUM(ip.quantidade) AS total_vendas
FROM produtos p
JOIN itens_pedido ip ON p.id_produto = ip.id_produto
GROUP BY p.nome;

-- Clientes que fizeram mais de 3 pedidos
SELECT c.nome, COUNT(p.id_pedido) AS total_pedidos
FROM clientes c
JOIN pedidos p ON c.id_cliente = p.id_cliente
GROUP BY c.nome
HAVING COUNT(p.id_pedido) > 3;

-- Produtos mais vendidos no último mês
SELECT p.nome, SUM(ip.quantidade) AS total_vendas
FROM produtos p
JOIN itens_pedido ip ON p.id_produto = ip.id_produto
JOIN pedidos pe ON ip.id_pedido = pe.id_pedido
WHERE pe.data_pedido >= DATE_SUB(CURRENT_DATE, INTERVAL 1 MONTH)
GROUP BY p.nome
ORDER BY total_vendas DESC;
```

Neste exemplo, criamos quatro tabelas: `clientes`, `produtos`, `pedidos` e `itens_pedido`. Em seguida, inserimos alguns dados nessas tabelas.

As consultas realizadas são:

1. Total de vendas por produto: retorna o nome do produto e a quantidade total vendida para cada produto.
2. Clientes que fizeram mais de 3 pedidos: retorna o nome dos clientes que fizeram mais de 3 pedidos e o total de pedidos feitos por cada um.
3. Produtos mais vendidos no último mês: retorna o nome dos produtos mais vendidos no último mês e a quantidade total vendida para cada produto, ordenados pelo total de vendas.

Espero que este exemplo tenha atendido às suas expectativas! Se você tiver mais alguma solicitação, estou aqui para ajudar.