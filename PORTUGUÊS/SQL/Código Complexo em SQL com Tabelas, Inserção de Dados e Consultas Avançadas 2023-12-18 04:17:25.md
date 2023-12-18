Certamente! Aqui está um código complexo em SQL que envolve várias tabelas e consultas avançadas:

```sql
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS clientes (
    id_cliente INT PRIMARY KEY,
    nome_cliente VARCHAR(100),
    endereco_cliente VARCHAR(100),
    telefone_cliente VARCHAR(20)
);

CREATE TABLE IF NOT EXISTS produtos (
    id_produto INT PRIMARY KEY,
    nome_produto VARCHAR(100),
    preco_produto DECIMAL(10,2),
    quantidade_estoque INT
);

CREATE TABLE IF NOT EXISTS pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente)
);

CREATE TABLE IF NOT EXISTS itens_pedido (
    id_item INT PRIMARY KEY,
    id_pedido INT,
    id_produto INT,
    quantidade_item INT,
    FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES produtos(id_produto)
);

-- Inserção de dados nas tabelas
INSERT INTO clientes (id_cliente, nome_cliente, endereco_cliente, telefone_cliente)
VALUES (1, 'João Silva', 'Rua A, 123', '(11) 1234-5678');

INSERT INTO produtos (id_produto, nome_produto, preco_produto, quantidade_estoque)
VALUES (1, 'Camiseta', 39.99, 100),
       (2, 'Calça Jeans', 89.99, 50),
       (3, 'Tênis', 149.99, 30);

INSERT INTO pedidos (id_pedido, id_cliente, data_pedido)
VALUES (1, 1, '2021-01-01');

INSERT INTO itens_pedido (id_item, id_pedido, id_produto, quantidade_item)
VALUES (1, 1, 1, 2),
       (2, 1, 2, 1),
       (3, 1, 3, 1);

-- Consulta complexa: soma dos valores totais dos pedidos por cliente
SELECT c.nome_cliente, SUM(p.preco_produto * ip.quantidade_item) AS valor_total
FROM clientes c
INNER JOIN pedidos pd ON c.id_cliente = pd.id_cliente
INNER JOIN itens_pedido ip ON pd.id_pedido = ip.id_pedido
INNER JOIN produtos p ON ip.id_produto = p.id_produto
GROUP BY c.nome_cliente;

-- Consulta complexa: lista de produtos com estoque baixo
SELECT nome_produto, quantidade_estoque
FROM produtos
WHERE quantidade_estoque < 10;
```

Neste código, começamos criando tabelas para representar clientes, produtos, pedidos e itens de pedido. Em seguida, inserimos alguns dados de exemplo nas tabelas. 

A primeira consulta complexa retorna o nome do cliente e o valor total de seus pedidos, calculado multiplicando o preço de cada produto pelo quantidade de itens no pedido. Utilizamos joins para relacionar as tabelas e agrupamos os resultados pelo nome do cliente.

A segunda consulta complexa retorna o nome do produto e a quantidade em estoque para aqueles que possuem uma quantidade inferior a 10. Utilizamos a cláusula WHERE para filtrar os resultados.

Espero que esse código atenda às suas expectativas!