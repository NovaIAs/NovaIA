Claro! Aqui está um exemplo de código complexo em SQL.

```sql
-- Criação das tabelas
CREATE TABLE IF NOT EXISTS Cliente (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    endereco VARCHAR(200)
);

CREATE TABLE IF NOT EXISTS Pedido (
    id INT PRIMARY KEY,
    data_pedido DATE,
    id_cliente INT,
    FOREIGN KEY (id_cliente) REFERENCES Cliente(id)
);

CREATE TABLE IF NOT EXISTS Produto (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    preco DECIMAL(10,2)
);

CREATE TABLE IF NOT EXISTS ItemPedido (
    id_pedido INT,
    id_produto INT,
    quantidade INT,
    PRIMARY KEY (id_pedido, id_produto),
    FOREIGN KEY (id_pedido) REFERENCES Pedido(id),
    FOREIGN KEY (id_produto) REFERENCES Produto(id)
);

-- Inserção de dados nas tabelas
INSERT INTO Cliente (id, nome, endereco) VALUES
    (1, 'João', 'Rua A, 123'),
    (2, 'Maria', 'Avenida B, 456'),
    (3, 'Pedro', 'Travessa C, 789');

INSERT INTO Produto (id, nome, preco) VALUES
    (1, 'Camiseta', 29.90),
    (2, 'Calça', 59.90),
    (3, 'Tênis', 99.90);

INSERT INTO Pedido (id, data_pedido, id_cliente) VALUES
    (1, '2021-01-01', 1),
    (2, '2021-02-05', 2),
    (3, '2021-03-10', 1);

INSERT INTO ItemPedido (id_pedido, id_produto, quantidade) VALUES
    (1, 1, 2),
    (1, 3, 1),
    (2, 2, 1),
    (3, 1, 3),
    (3, 3, 2);

-- Consulta de dados
SELECT c.nome AS Cliente, p.nome AS Produto, ip.quantidade AS Quantidade, p.preco * ip.quantidade AS Total
FROM Cliente c
JOIN Pedido pd ON pd.id_cliente = c.id
JOIN ItemPedido ip ON ip.id_pedido = pd.id
JOIN Produto p ON p.id = ip.id_produto
WHERE pd.data_pedido BETWEEN '2021-01-01' AND '2021-03-31'
ORDER BY c.nome, pd.data_pedido;
```

Neste exemplo, estamos criando um esquema de banco de dados para uma loja fictícia. São criadas quatro tabelas: `Cliente`, `Pedido`, `Produto` e `ItemPedido`. 

A tabela `Cliente` armazena informações sobre os clientes, como `id`, `nome` e `endereco`. A tabela `Pedido` guarda informações sobre os pedidos, como `id`, `data_pedido` e o `id_cliente` relacionado. A tabela `Produto` armazena informações sobre os produtos, como `id`, `nome` e `preco`. A tabela `ItemPedido` relaciona os pedidos com os produtos, guardando o `id_pedido`, `id_produto` e `quantidade` de cada produto em um pedido. 

Após criar as tabelas, são inseridos alguns dados fictícios nas tabelas utilizando a instrução `INSERT INTO`. Por exemplo, é inserido um cliente com `id` 1, nome "João" e endereço "Rua A, 123". Também são inseridos produtos e pedidos relacionados aos clientes.

Por fim, é realizada uma consulta para obter informações sobre os pedidos realizados entre uma determinada faixa de datas, mostrando o nome do cliente, o nome do produto, a quantidade e o valor total gasto em cada pedido. Os resultados são ordenados pelo nome do cliente e pela data do pedido.

Espero que esse exemplo possa te ajudar a compreender melhor o uso da linguagem SQL em um cenário real!