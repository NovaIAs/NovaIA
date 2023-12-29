Claro! Aqui está um código complexo em SQL, que envolve várias tabelas e consultas para fornecer informações detalhadas sobre vendas de produtos em uma loja online:

```
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS clientes (
    cliente_id INT PRIMARY KEY,
    nome VARCHAR(50),
    endereco VARCHAR(100)
);

CREATE TABLE IF NOT EXISTS produtos (
    produto_id INT PRIMARY KEY,
    nome VARCHAR(50),
    preco DECIMAL(10,2)
);

CREATE TABLE IF NOT EXISTS pedidos (
    pedido_id INT PRIMARY KEY,
    cliente_id INT,
    data_pedido DATE,
    FOREIGN KEY (cliente_id) REFERENCES clientes(cliente_id)
);

CREATE TABLE IF NOT EXISTS itens_pedido (
    pedido_id INT,
    produto_id INT,
    quantidade INT,
    FOREIGN KEY (pedido_id) REFERENCES pedidos(pedido_id),
    FOREIGN KEY (produto_id) REFERENCES produtos(produto_id)
);

-- Inserção de dados
INSERT INTO clientes (cliente_id, nome, endereco) VALUES
(1, 'João', 'Rua A, 123'),
(2, 'Maria', 'Rua B, 456'),
(3, 'Carlos', 'Rua C, 789');

INSERT INTO produtos (produto_id, nome, preco) VALUES
(1, 'Camiseta', 29.90),
(2, 'Calça', 79.90),
(3, 'Tênis', 99.90);

INSERT INTO pedidos (pedido_id, cliente_id, data_pedido) VALUES
(1, 1, '2022-01-01'),
(2, 2, '2022-01-02'),
(3, 3, '2022-01-03');

INSERT INTO itens_pedido (pedido_id, produto_id, quantidade) VALUES
(1, 1, 2),
(1, 2, 1),
(2, 3, 1),
(3, 1, 3),
(3, 3, 2);

-- Consulta para obter informações sobre vendas por cliente
SELECT c.nome AS nome_cliente, p.nome AS nome_produto, ip.quantidade, p.preco * ip.quantidade AS total_venda
FROM clientes c
JOIN pedidos ped ON c.cliente_id = ped.cliente_id
JOIN itens_pedido ip ON ped.pedido_id = ip.pedido_id
JOIN produtos p ON ip.produto_id = p.produto_id
ORDER BY c.nome;

-- Consulta para obter informações sobre vendas por produto
SELECT p.nome AS nome_produto, COUNT(ip.produto_id) AS total_vendas, SUM(ip.quantidade) AS total_quantidade, SUM(ip.quantidade * p.preco) AS total_receita
FROM produtos p
JOIN itens_pedido ip ON p.produto_id = ip.produto_id
GROUP BY p.nome
ORDER BY total_receita DESC;
```

Neste código, primeiro criamos as tabelas `clientes`, `produtos`, `pedidos` e `itens_pedido`. Essas tabelas representam os clientes, produtos, pedidos e os itens dos pedidos, respectivamente.

Em seguida, inserimos alguns dados de exemplo nas tabelas utilizando a instrução `INSERT INTO`.

Depois, temos duas consultas. A primeira consulta retorna as informações de vendas por cliente, mostrando o nome do cliente, o nome do produto, a quantidade vendida e o total da venda. Essa consulta utiliza as cláusulas `JOIN` para combinar as tabelas relacionadas.

A segunda consulta retorna as informações de vendas por produto, mostrando o nome do produto, o total de vendas, a quantidade total vendida e a receita total gerada por esse produto. Essa consulta utiliza a cláusula `GROUP BY` para agrupar os resultados por produto.

O código SQL acima é apenas um exemplo de código complexo que envolve várias tabelas e consultas. Você pode adaptá-lo e adicionar mais funcionalidades de acordo com suas necessidades.