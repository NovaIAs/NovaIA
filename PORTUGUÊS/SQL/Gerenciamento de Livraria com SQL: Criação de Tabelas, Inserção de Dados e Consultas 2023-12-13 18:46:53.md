Certamente! Aqui está um código complexo em SQL que utiliza várias funcionalidades e aborda um problema hipotético de gerenciamento de uma livraria. O código é extenso e abrange diversas tabelas e consultas diferentes. Vou explicar cada parte do código para facilitar o entendimento:

```
-- Criação das tabelas
CREATE TABLE Autor (
    id_autor INT PRIMARY KEY,
    nome_autor VARCHAR(100),
    data_nascimento DATE
);

CREATE TABLE Livro (
    id_livro INT PRIMARY KEY,
    titulo VARCHAR(200),
    ano_publicacao INT,
    id_editora INT,
    FOREIGN KEY (id_editora) REFERENCES Editora(id_editora)
);

CREATE TABLE Editora (
    id_editora INT PRIMARY KEY,
    nome_editora VARCHAR(200),
    cidade_editora VARCHAR(100)
);

CREATE TABLE Cliente (
    id_cliente INT PRIMARY KEY,
    nome_cliente VARCHAR(100),
    endereco_cliente VARCHAR(200),
    telefone_cliente VARCHAR(20)
);

CREATE TABLE Pedido (
    id_pedido INT PRIMARY KEY,
    id_cliente INT,
    id_livro INT,
    data_pedido DATE,
    FOREIGN KEY (id_cliente) REFERENCES Cliente(id_cliente),
    FOREIGN KEY (id_livro) REFERENCES Livro(id_livro)
);

-- Inserção de dados nas tabelas
INSERT INTO Autor (id_autor, nome_autor, data_nascimento) VALUES
    (1, 'Machado de Assis', '1839-06-21'),
    (2, 'Clarice Lispector', '1920-12-10'),
    (3, 'Carlos Drummond de Andrade', '1902-10-31');

INSERT INTO Editora (id_editora, nome_editora, cidade_editora) VALUES
    (1, 'Companhia das Letras', 'São Paulo'),
    (2, 'Editora Intrínseca', 'Rio de Janeiro'),
    (3, 'Editora Record', 'Rio de Janeiro');

INSERT INTO Livro (id_livro, titulo, ano_publicacao, id_editora) VALUES
    (1, 'Dom Casmurro', 1899, 1),
    (2, 'A Hora da Estrela', 1977, 1),
    (3, 'Quincas Borba', 1891, 2),
    (4, 'O Lustre', 1946, 3),
    (5, 'Grande Sertão: Veredas', 1956, 3);

INSERT INTO Cliente (id_cliente, nome_cliente, endereco_cliente, telefone_cliente) VALUES
    (1, 'João Silva', 'Rua A, 123', '1111111111'),
    (2, 'Maria Santos', 'Avenida B, 456', '2222222222'),
    (3, 'Pedro Souza', 'Rua C, 789', '3333333333');

INSERT INTO Pedido (id_pedido, id_cliente, id_livro, data_pedido) VALUES
    (1, 1, 1, '2021-01-01'),
    (2, 2, 3, '2021-02-15'),
    (3, 3, 5, '2021-03-20');

-- Consulta para listar todos os livros publicados por uma determinada editora
SELECT L.titulo, E.nome_editora
FROM Livro L
JOIN Editora E ON L.id_editora = E.id_editora
WHERE E.nome_editora = 'Editora Intrínseca';

-- Consulta para listar todos os pedidos feitos por um determinado cliente
SELECT P.id_pedido, C.nome_cliente, L.titulo, P.data_pedido
FROM Pedido P
JOIN Cliente C ON P.id_cliente = C.id_cliente
JOIN Livro L ON P.id_livro = L.id_livro
WHERE C.nome_cliente = 'João Silva';
```

Neste código, são criadas cinco tabelas: Autor, Livro, Editora, Cliente e Pedido. Cada tabela possui colunas relevantes para armazenar informações específicas. Em seguida, são inseridos dados fictícios nessas tabelas para exemplificar o funcionamento do código.

Após a inserção dos dados, são realizadas duas consultas diferentes:

1. A primeira consulta lista todos os livros publicados por uma determinada editora (no exemplo, a "Editora Intrínseca"). Isso é feito utilizando as cláusulas JOIN para relacionar a tabela Livro com a tabela Editora e então filtrar os resultados pela editora desejada.

2. A segunda consulta lista todos os pedidos feitos por um determinado cliente (no exemplo, o "João Silva"). Também são utilizadas as cláusulas JOIN para relacionar as tabelas Pedido, Cliente e Livro, e em seguida filtrar os resultados pelo nome do cliente.

Essas consultas são apenas exemplos de como você pode utilizar SQL para obter informações específicas do banco de dados. É importante ressaltar que esse código é apenas uma demonstração e pode ser adaptado de acordo com as necessidades do projeto real.