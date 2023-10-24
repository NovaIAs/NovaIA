Claro! Aqui está um exemplo de um código complexo em SQL que envolve várias tabelas e operações:

```
-- Criação das tabelas
CREATE TABLE Clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(200)
);

CREATE TABLE Pedidos (
    id INT PRIMARY KEY,
    cliente_id INT,
    data_pedido DATE,
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (cliente_id) REFERENCES Clientes(id)
);

CREATE TABLE Produtos (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10, 2)
);

CREATE TABLE ItensPedido (
    id INT PRIMARY KEY,
    pedido_id INT,
    produto_id INT,
    quantidade INT,
    FOREIGN KEY (pedido_id) REFERENCES Pedidos(id),
    FOREIGN KEY (produto_id) REFERENCES Produtos(id)
);

-- Inserção de dados
INSERT INTO Clientes (id, nome, endereco) VALUES
    (1, 'João', 'Rua A, 123'),
    (2, 'Maria', 'Rua B, 456');

INSERT INTO Pedidos (id, cliente_id, data_pedido, valor_total) VALUES
    (1, 1, '2022-01-01', 100.00),
    (2, 2, '2022-02-01', 200.00);

INSERT INTO Produtos (id, nome, preco) VALUES
    (1, 'Camiseta', 50.00),
    (2, 'Calça', 80.00);

INSERT INTO ItensPedido (id, pedido_id, produto_id, quantidade) VALUES
    (1, 1, 1, 2),
    (2, 1, 2, 1),
    (3, 2, 1, 3),
    (4, 2, 2, 2);

-- Consulta complexa
SELECT c.nome AS nome_cliente, p.data_pedido, SUM(ip.quantidade * pr.preco) AS valor_total_pedido
FROM Clientes c
INNER JOIN Pedidos p ON c.id = p.cliente_id
INNER JOIN ItensPedido ip ON p.id = ip.pedido_id
INNER JOIN Produtos pr ON ip.produto_id = pr.id
GROUP BY c.nome, p.data_pedido
HAVING COUNT(*) > 1
ORDER BY valor_total_pedido DESC;
```

Neste código, começamos criando as tabelas para representar os clientes, pedidos, produtos e itens de pedido em um sistema de gerenciamento de pedidos. 

Em seguida, inserimos alguns dados de exemplo nessas tabelas. Os clientes João e Maria são inseridos na tabela de clientes, os pedidos são associados a cada cliente, os produtos são inseridos na tabela de produtos, e os itens de pedido são vinculados aos pedidos e produtos correspondentes.

Finalmente, realizamos uma consulta complexa que envolve várias junções (INNER JOIN) entre as tabelas. A consulta retorna o nome do cliente, a data do pedido e o valor total de cada pedido, exibindo apenas os pedidos com mais de um item e ordenando pelo valor total do pedido em ordem decrescente.

Esse é apenas um exemplo de um código complexo em SQL, que envolve a criação de tabelas, a inserção de dados e a realização de consultas complexas com múltiplas junções. O SQL é uma linguagem poderosa para manipulação de bancos de dados relacionais e permite realizar diversas operações sofisticadas.