Claro! Aqui está um exemplo de código em SQL complexo que envolve várias tabelas e consultas. O código abaixo cria um banco de dados fictício para uma loja online, com tabelas para produtos, clientes, pedidos e pagamentos:

```sql
-- Criação do banco de dados
CREATE DATABASE LojaOnline;

-- Utilização do banco de dados
USE LojaOnline;

-- Criação da tabela de produtos
CREATE TABLE Produtos (
    ID INT PRIMARY KEY,
    Nome VARCHAR(100),
    Preco DECIMAL(10, 2),
    QuantidadeEmEstoque INT
);

-- Criação da tabela de clientes
CREATE TABLE Clientes (
    ID INT PRIMARY KEY,
    Nome VARCHAR(100),
    Email VARCHAR(100)
);

-- Criação da tabela de pedidos
CREATE TABLE Pedidos (
    ID INT PRIMARY KEY,
    DataPedido DATE,
    IDCliente INT,
    FOREIGN KEY (IDCliente) REFERENCES Clientes(ID)
);

-- Criação da tabela de itens de pedido
CREATE TABLE ItensPedido (
    IDPedido INT,
    IDProduto INT,
    Quantidade INT,
    PRIMARY KEY (IDPedido, IDProduto),
    FOREIGN KEY (IDPedido) REFERENCES Pedidos(ID),
    FOREIGN KEY (IDProduto) REFERENCES Produtos(ID)
);

-- Criação da tabela de pagamentos
CREATE TABLE Pagamentos (
    ID INT PRIMARY KEY,
    IDPedido INT,
    Valor DECIMAL(10, 2),
    DataPagamento DATE,
    FOREIGN KEY (IDPedido) REFERENCES Pedidos(ID)
);

-- Inserção de dados de exemplo
INSERT INTO Produtos (ID, Nome, Preco, QuantidadeEmEstoque)
VALUES (1, 'Camiseta', 29.90, 50),
       (2, 'Calça Jeans', 99.90, 30),
       (3, 'Tênis', 149.90, 20);

INSERT INTO Clientes (ID, Nome, Email)
VALUES (1, 'João Silva', 'joao.silva@example.com'),
       (2, 'Maria Santos', 'maria.santos@example.com');

INSERT INTO Pedidos (ID, DataPedido, IDCliente)
VALUES (1, '2022-01-01', 1),
       (2, '2022-01-02', 2);

INSERT INTO ItensPedido (IDPedido, IDProduto, Quantidade)
VALUES (1, 1, 2),
       (1, 2, 1),
       (2, 3, 1);

INSERT INTO Pagamentos (ID, IDPedido, Valor, DataPagamento)
VALUES (1, 1, 89.80, '2022-01-02'),
       (2, 2, 149.90, '2022-01-03');

-- Consulta para obter todos os pedidos de um cliente específico, incluindo os detalhes dos produtos e pagamentos
SELECT c.Nome AS NomeCliente, p.ID AS NumeroPedido, pr.Nome AS NomeProduto, pr.Preco AS PrecoProduto, ip.Quantidade AS QuantidadeProduto, pg.Valor AS ValorPago
FROM Clientes c
JOIN Pedidos p ON c.ID = p.IDCliente
JOIN ItensPedido ip ON p.ID = ip.IDPedido
JOIN Produtos pr ON ip.IDProduto = pr.ID
LEFT JOIN Pagamentos pg ON p.ID = pg.IDPedido
WHERE c.ID = 1;
```

Nesse exemplo, o código cria um banco de dados chamado "LojaOnline" e define as tabelas necessárias para armazenar produtos, clientes, pedidos e pagamentos. Em seguida, são inseridos alguns dados de exemplo nessas tabelas.

Por fim, é realizada uma consulta para obter todos os pedidos de um cliente específico (ID do cliente = 1), incluindo os detalhes dos produtos e pagamentos. Essa consulta utiliza as cláusulas JOIN para relacionar as tabelas e obter as informações desejadas.

Esse é apenas um exemplo de código complexo em SQL, mas existem muitas outras possibilidades e cenários em que poderia ser aplicado.