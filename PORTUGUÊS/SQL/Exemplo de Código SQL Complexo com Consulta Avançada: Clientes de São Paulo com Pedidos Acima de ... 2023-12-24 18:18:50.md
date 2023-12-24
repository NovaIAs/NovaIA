Certamente! Aqui está um exemplo de código SQL complexo que envolve várias tabelas e operações:

```
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS Clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(200),
    cidade VARCHAR(100),
    estado VARCHAR(50)
);

CREATE TABLE IF NOT EXISTS Pedidos (
    id INT PRIMARY KEY,
    cliente_id INT,
    data_pedido DATE,
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (cliente_id) REFERENCES Clientes(id)
);

CREATE TABLE IF NOT EXISTS Produtos (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10, 2)
);

CREATE TABLE IF NOT EXISTS ItensPedido (
    id INT PRIMARY KEY,
    pedido_id INT,
    produto_id INT,
    quantidade INT,
    FOREIGN KEY (pedido_id) REFERENCES Pedidos(id),
    FOREIGN KEY (produto_id) REFERENCES Produtos(id)
);

-- Inserção de dados
INSERT INTO Clientes (id, nome, endereco, cidade, estado)
VALUES (1, 'João Silva', 'Rua A, 123', 'São Paulo', 'SP'),
       (2, 'Maria Santos', 'Av. B, 456', 'Rio de Janeiro', 'RJ'),
       (3, 'Pedro Oliveira', 'Rua C, 789', 'Belo Horizonte', 'MG');

INSERT INTO Pedidos (id, cliente_id, data_pedido, valor_total)
VALUES (1, 1, '2021-01-01', 100.00),
       (2, 2, '2021-02-05', 150.00),
       (3, 3, '2021-03-10', 200.00);

INSERT INTO Produtos (id, nome, preco)
VALUES (1, 'Camiseta', 50.00),
       (2, 'Calça', 80.00),
       (3, 'Tênis', 120.00);

INSERT INTO ItensPedido (id, pedido_id, produto_id, quantidade)
VALUES (1, 1, 1, 2),
       (2, 1, 2, 1),
       (3, 2, 2, 3),
       (4, 3, 3, 2);

-- Consulta complexa
SELECT c.nome AS cliente, p.data_pedido AS data, SUM(ip.quantidade * pr.preco) AS valor_total
FROM Clientes AS c
JOIN Pedidos AS p ON c.id = p.cliente_id
JOIN ItensPedido AS ip ON p.id = ip.pedido_id
JOIN Produtos AS pr ON ip.produto_id = pr.id
WHERE c.cidade = 'São Paulo'
GROUP BY c.nome, p.data_pedido
HAVING SUM(ip.quantidade * pr.preco) > 100.00
ORDER BY p.data_pedido DESC;
```

Nesse exemplo, criamos quatro tabelas: `Clientes`, `Pedidos`, `Produtos` e `ItensPedido`. A tabela `Clientes` armazena informações sobre os clientes, como nome, endereço, cidade e estado. A tabela `Pedidos` armazena informações sobre os pedidos feitos pelos clientes, como a data do pedido e o valor total. A tabela `Produtos` armazena informações sobre os produtos disponíveis para venda, como nome e preço. A tabela `ItensPedido` relaciona os produtos comprados em cada pedido, armazenando a quantidade de cada produto.

Em seguida, inserimos dados nessas tabelas usando declarações `INSERT INTO`. Após a inserção dos dados, fazemos uma consulta complexa para obter os clientes da cidade de "São Paulo" que fizeram pedidos totalizando mais de 100 reais. A consulta usa várias cláusulas SQL, como `SELECT`, `JOIN`, `WHERE`, `GROUP BY`, `HAVING` e `ORDER BY`.

Espero que esse exemplo lhe ajude a entender como criar um código SQL complexo e executar consultas avançadas em um banco de dados!