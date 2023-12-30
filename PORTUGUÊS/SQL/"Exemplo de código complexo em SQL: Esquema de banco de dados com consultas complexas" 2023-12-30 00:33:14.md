Claro! Aqui está um exemplo de código complexo em SQL, que envolve várias tabelas e operações:

```
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS Clientes (
    id_cliente INT PRIMARY KEY,
    nome VARCHAR(50),
    endereco VARCHAR(100)
);

CREATE TABLE IF NOT EXISTS Pedidos (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    data_pedido DATE,
    FOREIGN KEY (id_cliente) REFERENCES Clientes(id_cliente)
);

CREATE TABLE IF NOT EXISTS ItensPedido (
    id_item INT PRIMARY KEY,
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    FOREIGN KEY (id_pedido) REFERENCES Pedidos(id_pedido),
    FOREIGN KEY (id_produto) REFERENCES Produtos(id_produto)
);

CREATE TABLE IF NOT EXISTS Produtos (
    id_produto INT PRIMARY KEY,
    nome VARCHAR(50),
    preco DECIMAL(10, 2)
);


-- Inserção de dados nas tabelas
INSERT INTO Clientes (id_cliente, nome, endereco) VALUES
    (1, 'João Silva', 'Rua A, 123'),
    (2, 'Maria Santos', 'Avenida B, 456'),
    (3, 'Carlos Oliveira', 'Rua C, 789');

INSERT INTO Pedidos (id_pedido, id_cliente, data_pedido) VALUES
    (1, 1, '2021-01-01'),
    (2, 2, '2021-02-05'),
    (3, 3, '2021-03-10');

INSERT INTO Produtos (id_produto, nome, preco) VALUES
    (1, 'Camiseta', 29.90),
    (2, 'Calça Jeans', 99.90),
    (3, 'Tênis', 149.90);

INSERT INTO ItensPedido (id_item, id_pedido, id_produto, quantidade) VALUES
    (1, 1, 1, 2),
    (2, 1, 2, 1),
    (3, 2, 2, 3),
    (4, 3, 3, 1);


-- Consulta complexa
SELECT c.nome AS cliente, p.data_pedido AS data, pr.nome AS produto, ip.quantidade, pr.preco, ip.quantidade * pr.preco AS total
FROM Clientes c
JOIN Pedidos p ON c.id_cliente = p.id_cliente
JOIN ItensPedido ip ON p.id_pedido = ip.id_pedido
JOIN Produtos pr ON ip.id_produto = pr.id_produto
WHERE p.data_pedido >= '2021-02-01'
ORDER BY c.nome, p.data_pedido;
```

Este código SQL cria um esquema de banco de dados simples com as tabelas "Clientes", "Pedidos", "ItensPedido" e "Produtos". Em seguida, insere alguns dados nas tabelas. Por fim, realiza uma consulta complexa que retorna o nome do cliente, a data do pedido, o nome do produto, a quantidade, o preço unitário e o total de cada item do pedido, filtrando apenas os pedidos realizados a partir de 1º de fevereiro de 2021 e ordenando por nome do cliente e data do pedido.

Espero que este exemplo atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais assistência, fique à vontade para perguntar.