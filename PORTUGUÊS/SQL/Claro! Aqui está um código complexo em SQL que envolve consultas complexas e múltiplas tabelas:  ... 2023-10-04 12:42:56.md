Claro! Aqui está um código complexo em SQL que envolve consultas complexas e múltiplas tabelas:

```sql
-- Criação da tabela "clientes"
CREATE TABLE clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    idade INT,
    endereco VARCHAR(100)
);

-- Criação da tabela "pedidos"
CREATE TABLE pedidos (
    id INT PRIMARY KEY,
    data_pedido DATE,
    valor_total DECIMAL(10, 2),
    cliente_id INT,
    FOREIGN KEY (cliente_id) REFERENCES clientes(id)
);

-- Criação da tabela "produtos"
CREATE TABLE produtos (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    preco DECIMAL(10, 2)
);

-- Criação da tabela "itens_pedido"
CREATE TABLE itens_pedido (
    id INT PRIMARY KEY,
    pedido_id INT,
    produto_id INT,
    quantidade INT,
    FOREIGN KEY (pedido_id) REFERENCES pedidos(id),
    FOREIGN KEY (produto_id) REFERENCES produtos(id)
);

-- Inserção de dados na tabela "clientes"
INSERT INTO clientes (id, nome, idade, endereco)
VALUES (1, 'João', 25, 'Rua A'),
       (2, 'Maria', 30, 'Rua B'),
       (3, 'José', 35, 'Rua C');

-- Inserção de dados na tabela "pedidos"
INSERT INTO pedidos (id, data_pedido, valor_total, cliente_id)
VALUES (1, '2022-01-01', 100.00, 1),
       (2, '2022-01-02', 200.00, 2),
       (3, '2022-01-03', 150.00, 3);

-- Inserção de dados na tabela "produtos"
INSERT INTO produtos (id, nome, preco)
VALUES (1, 'Camiseta', 50.00),
       (2, 'Calça', 80.00),
       (3, 'Tênis', 120.00);

-- Inserção de dados na tabela "itens_pedido"
INSERT INTO itens_pedido (id, pedido_id, produto_id, quantidade)
VALUES (1, 1, 1, 2),
       (2, 1, 2, 1),
       (3, 2, 3, 1),
       (4, 3, 2, 3),
       (5, 3, 3, 2);

-- Consulta para obter o nome do cliente, a data do pedido, o nome do produto e a quantidade de itens por pedido
SELECT c.nome AS nome_cliente, p.data_pedido, pr.nome AS nome_produto, ip.quantidade
FROM clientes c
JOIN pedidos p ON c.id = p.cliente_id
JOIN itens_pedido ip ON p.id = ip.pedido_id
JOIN produtos pr ON ip.produto_id = pr.id;
```

Nesse código, começamos criando as tabelas "clientes", "pedidos", "produtos" e "itens_pedido", cada uma com suas respectivas colunas e restrições de chave primária e chave estrangeira.

Em seguida, inserimos alguns dados nessas tabelas utilizando a cláusula "INSERT INTO".

Por fim, realizamos uma consulta complexa utilizando a cláusula "SELECT" para obter o nome do cliente, a data do pedido, o nome do produto e a quantidade de itens por pedido. Essa consulta envolve várias junções (joins) entre as tabelas para obter as informações desejadas.

Espero que esse código atenda às suas expectativas e demonstre a complexidade e versatilidade do SQL!