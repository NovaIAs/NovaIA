Claro! Aqui está um exemplo de código complexo em SQL:

```
-- Criação da tabela de clientes
CREATE TABLE clientes (
    id INT PRIMARY KEY,
    nome VARCHAR(50),
    email VARCHAR(100),
    endereco VARCHAR(200)
);

-- Criação da tabela de pedidos
CREATE TABLE pedidos (
    id INT PRIMARY KEY,
    cliente_id INT,
    data_pedido DATE,
    valor_total DECIMAL(10,2),
    FOREIGN KEY (cliente_id) REFERENCES clientes(id)
);

-- Inserção de dados na tabela de clientes
INSERT INTO clientes (id, nome, email, endereco)
VALUES 
    (1, 'João da Silva', 'joao.silva@gmail.com', 'Rua A, 123'),
    (2, 'Maria Santos', 'maria.santos@gmail.com', 'Avenida B, 456'),
    (3, 'Pedro Oliveira', 'pedro.oliveira@gmail.com', 'Travessa C, 789');

-- Inserção de dados na tabela de pedidos
INSERT INTO pedidos (id, cliente_id, data_pedido, valor_total)
VALUES
    (1, 1, '2022-01-01', 100.00),
    (2, 1, '2022-02-15', 150.00),
    (3, 2, '2022-03-10', 200.00),
    (4, 3, '2022-04-20', 50.00);

-- Consulta para obter o nome e o valor total de todos os pedidos de cada cliente
SELECT c.nome, SUM(p.valor_total) AS valor_total_pedidos
FROM clientes c
JOIN pedidos p ON c.id = p.cliente_id
GROUP BY c.nome;
```

Explicação do código:

1. Primeiramente, criamos a tabela "clientes" com as colunas "id", "nome", "email" e "endereco". A coluna "id" é definida como chave primária.
2. Em seguida, criamos a tabela "pedidos" com as colunas "id", "cliente_id", "data_pedido" e "valor_total". A coluna "id" é definida como chave primária e a coluna "cliente_id" possui uma chave estrangeira que faz referência à tabela "clientes".
3. Insere-se alguns dados na tabela "clientes" utilizando a instrução INSERT INTO.
4. Em seguida, insere-se alguns dados na tabela "pedidos" utilizando a instrução INSERT INTO.
5. Por fim, faz-se uma consulta que retorna o nome e o valor total de todos os pedidos de cada cliente. Isso é obtido através de um JOIN entre as tabelas "clientes" e "pedidos" e utilizando a função de agregação SUM para somar os valores dos pedidos. O resultado é agrupado pelo nome do cliente.