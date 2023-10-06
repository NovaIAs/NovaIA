Claro! Aqui está um exemplo de um código complexo em SQL que envolve várias tabelas, junções, subconsultas e funções:

```
-- Criação das tabelas
CREATE TABLE clientes (
    id_cliente INT PRIMARY KEY,
    nome_cliente VARCHAR(100),
    endereco VARCHAR(200),
    cidade VARCHAR(100),
    estado VARCHAR(50)
);

CREATE TABLE pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    valor_total DECIMAL(10, 2),
    data_pedido DATE,
    status_pedido VARCHAR(50),
    FOREIGN KEY (id_cliente) REFERENCES clientes (id_cliente)
);

CREATE TABLE produtos (
    id_produto INT PRIMARY KEY,
    nome_produto VARCHAR(100),
    preco_produto DECIMAL(10, 2),
    quantidade_estoque INT
);

CREATE TABLE itens_pedido (
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    valor_total_item DECIMAL(10, 2),
    FOREIGN KEY (id_pedido) REFERENCES pedidos (id_pedido),
    FOREIGN KEY (id_produto) REFERENCES produtos (id_produto)
);

-- Inserção de dados nas tabelas
INSERT INTO clientes (id_cliente, nome_cliente, endereco, cidade, estado)
VALUES (1, 'João da Silva', 'Rua das Flores, 123', 'São Paulo', 'SP');

INSERT INTO clientes (id_cliente, nome_cliente, endereco, cidade, estado)
VALUES (2, 'Maria Souza', 'Av. das Palmeiras, 456', 'Rio de Janeiro', 'RJ');

INSERT INTO produtos (id_produto, nome_produto, preco_produto, quantidade_estoque)
VALUES (1, 'Camiseta', 29.99, 100);

INSERT INTO produtos (id_produto, nome_produto, preco_produto, quantidade_estoque)
VALUES (2, 'Calça Jeans', 79.99, 50);

INSERT INTO pedidos (id_pedido, id_cliente, valor_total, data_pedido, status_pedido)
VALUES (1, 1, 59.98, '2021-01-01', 'Entregue');

INSERT INTO pedidos (id_pedido, id_cliente, valor_total, data_pedido, status_pedido)
VALUES (2, 2, 159.98, '2021-02-01', 'Em andamento');

INSERT INTO itens_pedido (id_pedido, id_produto, quantidade, valor_total_item)
VALUES (1, 1, 2, 59.98);

INSERT INTO itens_pedido (id_pedido, id_produto, quantidade, valor_total_item)
VALUES (2, 2, 2, 159.98);

-- Consulta com junções, subconsultas e funções agregadas
SELECT c.nome_cliente, p.nome_produto, ip.quantidade, ip.valor_total_item
FROM clientes c
JOIN pedidos pd ON c.id_cliente = pd.id_cliente
JOIN itens_pedido ip ON pd.id_pedido = ip.id_pedido
JOIN produtos p ON ip.id_produto = p.id_produto
WHERE pd.status_pedido = 'Entregue'
AND YEAR(pd.data_pedido) = 2021
AND p.preco_produto > (
    SELECT AVG(preco_produto) FROM produtos
)
ORDER BY c.nome_cliente ASC, p.nome_produto ASC;
```

Neste código, estamos criando quatro tabelas: `clientes`, `pedidos`, `produtos` e `itens_pedido`. A tabela `clientes` armazena informações sobre os clientes, como nome, endereço, cidade e estado. A tabela `pedidos` registra os pedidos feitos pelos clientes, armazenando o valor total, a data do pedido e o status atual. A tabela `produtos` contém informações sobre os produtos disponíveis para venda, como nome, preço e quantidade em estoque. A tabela `itens_pedido` relaciona os produtos solicitados em cada pedido, armazenando a quantidade e o valor total do item.

Em seguida, inserimos alguns dados de exemplo nas tabelas. Após a inserção de dados, realizamos uma consulta complexa que envolve junções (`JOIN`), subconsultas e funções agregadas. A consulta retorna o nome do cliente, o nome do produto, a quantidade e o valor total do item para todos os pedidos entregues em 2021, onde o preço do produto é maior que a média de preços de todos os produtos. Os resultados são ordenados pelo nome do cliente e pelo nome do produto.

Esse código é apenas um exemplo de como um código complexo em SQL pode ser desenvolvido. Dependendo dos requisitos específicos do sistema em questão, a estrutura e a lógica do código podem variar.