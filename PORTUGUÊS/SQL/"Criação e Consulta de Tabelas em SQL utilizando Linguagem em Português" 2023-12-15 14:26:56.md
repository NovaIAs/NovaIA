Claro! Aqui está um código em SQL que envolve várias tabelas e consultas, utilizando a linguagem em português para facilitar o entendimento:

```sql
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS Clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    idade INT,
    endereco VARCHAR(200)
);

CREATE TABLE IF NOT EXISTS Pedidos (
    id INT PRIMARY KEY,
    cliente_id INT,
    data_pedido DATE,
    valor DECIMAL(10,2),
    FOREIGN KEY (cliente_id) REFERENCES Clientes(id)
);

CREATE TABLE IF NOT EXISTS ItensPedido (
    id INT PRIMARY KEY,
    pedido_id INT,
    produto VARCHAR(100),
    quantidade INT,
    valor_unitario DECIMAL(10,2),
    FOREIGN KEY (pedido_id) REFERENCES Pedidos(id)
);

-- Inserção de dados nas tabelas
INSERT INTO Clientes (id, nome, idade, endereco)
VALUES (1, 'João Silva', 30, 'Rua A, 123'),
       (2, 'Maria Santos', 25, 'Av. B, 456'),
       (3, 'Carlos Oliveira', 40, 'Rua C, 789');

INSERT INTO Pedidos (id, cliente_id, data_pedido, valor)
VALUES (1, 1, '2021-01-10', 100.00),
       (2, 2, '2021-02-15', 150.00),
       (3, 1, '2021-03-20', 200.00);

INSERT INTO ItensPedido (id, pedido_id, produto, quantidade, valor_unitario)
VALUES (1, 1, 'Camiseta', 2, 25.00),
       (2, 1, 'Calça', 1, 50.00),
       (3, 2, 'Tênis', 1, 100.00),
       (4, 3, 'Boné', 3, 20.00);

-- Consulta de pedidos e seus itens
SELECT c.nome AS cliente, p.data_pedido, i.produto, i.quantidade, i.valor_unitario
FROM Clientes c
JOIN Pedidos p ON c.id = p.cliente_id
JOIN ItensPedido i ON p.id = i.pedido_id;

-- Consulta de pedidos com valor total
SELECT p.id, c.nome AS cliente, p.data_pedido, SUM(i.quantidade * i.valor_unitario) AS valor_total
FROM Pedidos p
JOIN Clientes c ON p.cliente_id = c.id
JOIN ItensPedido i ON p.id = i.pedido_id
GROUP BY p.id, c.nome, p.data_pedido;
```

Neste código, estamos criando três tabelas: "Clientes", "Pedidos" e "ItensPedido". Em seguida, inserimos alguns dados nessas tabelas usando as declarações `INSERT INTO`. 

A primeira consulta exibe os pedidos feitos por cada cliente e os itens de cada pedido. Utilizamos as cláusulas `JOIN` para relacionar as tabelas entre si.

A segunda consulta calcula o valor total de cada pedido, somando a quantidade de cada item multiplicada pelo valor unitário. Utilizamos a cláusula `GROUP BY` para agrupar os resultados pelo ID do pedido, nome do cliente e data do pedido.

Espero que este código complexo e diferenciado atenda às suas expectativas! Se tiver mais alguma solicitação, estou à disposição.