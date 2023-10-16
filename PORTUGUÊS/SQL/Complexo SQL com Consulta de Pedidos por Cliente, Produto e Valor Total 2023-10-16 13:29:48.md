Claro! Aqui está um código complexo em SQL que envolve várias tabelas e consultas complexas. Vou explicar cada parte do código para facilitar o entendimento:

```
-- Criação das tabelas
CREATE TABLE clientes (
    id_cliente INT PRIMARY KEY,
    nome_cliente VARCHAR(100),
    endereco_cliente VARCHAR(200),
    cidade_cliente VARCHAR(100),
    estado_cliente VARCHAR(2)
);

CREATE TABLE pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    valor_total DECIMAL(10, 2),
    FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

CREATE TABLE itens_pedido (
    id_item INT PRIMARY KEY,
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    preco_unitario DECIMAL(10, 2),
    FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);

CREATE TABLE produtos (
    id_produto INT PRIMARY KEY,
    nome_produto VARCHAR(100),
    preco_produto DECIMAL(10, 2)
);

-- Inserção de dados nas tabelas
INSERT INTO clientes (id_cliente, nome_cliente, endereco_cliente, cidade_cliente, estado_cliente)
VALUES
    (1, 'João Silva', 'Rua A, 123', 'São Paulo', 'SP'),
    (2, 'Maria Santos', 'Av. B, 456', 'Rio de Janeiro', 'RJ'),
    (3, 'Pedro Oliveira', 'Rua C, 789', 'Belo Horizonte', 'MG');

INSERT INTO pedidos (id_pedido, id_cliente, data_pedido, valor_total)
VALUES
    (1, 1, '2021-01-01', 100.00),
    (2, 2, '2021-02-01', 250.00),
    (3, 3, '2021-03-01', 150.00);

INSERT INTO produtos (id_produto, nome_produto, preco_produto)
VALUES
    (1, 'Camiseta', 50.00),
    (2, 'Calça', 80.00),
    (3, 'Tênis', 120.00);

INSERT INTO itens_pedido (id_item, id_pedido, id_produto, quantidade, preco_unitario)
VALUES
    (1, 1, 1, 2, 50.00),
    (2, 1, 2, 1, 80.00),
    (3, 2, 3, 1, 120.00),
    (4, 3, 1, 3, 50.00),
    (5, 3, 2, 2, 80.00);

-- Consulta complexa
SELECT c.nome_cliente, p.nome_produto, ip.quantidade, ip.preco_unitario, (ip.quantidade * ip.preco_unitario) AS subtotal
FROM clientes c
INNER JOIN pedidos pd ON c.id_cliente = pd.id_cliente
INNER JOIN itens_pedido ip ON pd.id_pedido = ip.id_pedido
INNER JOIN produtos p ON ip.id_produto = p.id_produto
WHERE pd.data_pedido >= '2021-02-01'
AND pd.data_pedido <= '2021-03-31'
AND c.estado_cliente = 'SP'
GROUP BY c.nome_cliente, p.nome_produto, ip.quantidade, ip.preco_unitario
HAVING SUM(ip.quantidade * ip.preco_unitario) > 200.00
ORDER BY c.nome_cliente, p.nome_produto;
```

Este código em SQL cria uma estrutura de tabelas para armazenar informações de clientes, pedidos, produtos e itens de pedido. Em seguida, insere dados fictícios nessas tabelas.

A parte mais complexa do código é a consulta final. Nessa consulta, é realizado um JOIN entre as tabelas clientes, pedidos, itens_pedido e produtos para obter informações detalhadas sobre os pedidos realizados por clientes específicos.

A consulta possui algumas condições de filtro, como a data do pedido, o estado do cliente e um valor mínimo para o subtotal dos itens do pedido. Além disso, é realizada uma soma do subtotal dos itens do pedido e a ordenação é feita pelo nome do cliente e pelo nome do produto.

Espero que este código em SQL atenda às suas expectativas!