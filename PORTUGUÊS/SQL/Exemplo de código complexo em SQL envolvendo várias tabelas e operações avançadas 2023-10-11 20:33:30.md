Claro! Aqui está um exemplo de um código complexo em SQL que envolve várias tabelas e operações avançadas:

```
-- CRIAÇÃO DAS TABELAS
CREATE TABLE clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    email VARCHAR(100),
    telefone VARCHAR(20)
);

CREATE TABLE pedidos (
    id INT PRIMARY KEY,
    cliente_id INT,
    data_pedido DATE,
    valor_total DECIMAL(10,2),
    FOREIGN KEY (cliente_id) REFERENCES clientes(id)
);

CREATE TABLE itens_pedido (
    id INT PRIMARY KEY,
    pedido_id INT,
    produto VARCHAR(100),
    quantidade INT,
    preco_unitario DECIMAL(10,2),
    FOREIGN KEY (pedido_id) REFERENCES pedidos(id)
);

-- INSERÇÃO DE DADOS
INSERT INTO clientes (id, nome, email, telefone)
VALUES (1, 'João', 'joao@example.com', '1234567890');

INSERT INTO pedidos (id, cliente_id, data_pedido, valor_total)
VALUES (1, 1, '2022-01-01', 100.00);

INSERT INTO itens_pedido (id, pedido_id, produto, quantidade, preco_unitario)
VALUES (1, 1, 'Produto A', 2, 50.00),
       (2, 1, 'Produto B', 1, 30.00),
       (3, 1, 'Produto C', 3, 10.00);

-- CONSULTA COM JOIN E AGREGAÇÃO
SELECT c.nome AS nome_cliente, p.data_pedido, SUM(ip.quantidade) AS total_itens, SUM(ip.quantidade * ip.preco_unitario) AS valor_total
FROM clientes c
INNER JOIN pedidos p ON c.id = p.cliente_id
INNER JOIN itens_pedido ip ON p.id = ip.pedido_id
GROUP BY c.nome, p.data_pedido;
```

Neste exemplo, o código SQL cria três tabelas: "clientes", "pedidos" e "itens_pedido". A tabela "clientes" armazena informações sobre os clientes, a tabela "pedidos" registra os pedidos dos clientes e a tabela "itens_pedido" contém os produtos associados a cada pedido.

Em seguida, são inseridos alguns dados nas tabelas usando a cláusula INSERT INTO.

Por fim, é feita uma consulta complexa usando o JOIN para combinar as informações das três tabelas. A consulta retorna o nome do cliente, a data do pedido, o total de itens por pedido e o valor total de cada pedido.

Espero que este exemplo ilustre um código SQL complexo e ajude a entender a estrutura e as operações envolvidas.