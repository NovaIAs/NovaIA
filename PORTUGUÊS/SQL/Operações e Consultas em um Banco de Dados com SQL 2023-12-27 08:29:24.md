Claro! Aqui está um código complexo em SQL que realiza várias operações em um banco de dados fictício de uma empresa:

```sql
-- Criando tabela de funcionários
CREATE TABLE Funcionarios (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    salario DECIMAL(10, 2),
    data_contratacao DATE,
    departamento_id INT,
    FOREIGN KEY (departamento_id) REFERENCES Departamentos(id)
);

-- Criando tabela de departamentos
CREATE TABLE Departamentos (
    id INT PRIMARY KEY,
    nome VARCHAR(100),
    localizacao VARCHAR(100)
);

-- Inserindo dados na tabela de departamentos
INSERT INTO Departamentos (id, nome, localizacao) VALUES
    (1, 'Vendas', 'São Paulo'),
    (2, 'Marketing', 'Rio de Janeiro'),
    (3, 'TI', 'Belo Horizonte');

-- Inserindo dados na tabela de funcionários
INSERT INTO Funcionarios (id, nome, salario, data_contratacao, departamento_id) VALUES
    (1, 'João Silva', 5000.00, '2020-01-01', 1),
    (2, 'Maria Santos', 6000.00, '2019-06-15', 2),
    (3, 'Pedro Almeida', 4500.00, '2020-03-10', 3),
    (4, 'Ana Oliveira', 7000.00, '2018-09-01', 1),
    (5, 'Carlos Souza', 5500.00, '2021-02-20', 2);

-- Consulta para obter o nome e salário dos funcionários com salário maior que 5500
SELECT nome, salario
FROM Funcionarios
WHERE salario > 5500;

-- Consulta para calcular a média salarial por departamento
SELECT d.nome AS departamento, AVG(f.salario) AS media_salarial
FROM Funcionarios f
JOIN Departamentos d ON f.departamento_id = d.id
GROUP BY d.nome;

-- Consulta para obter o nome do funcionário mais recentemente contratado em cada departamento
SELECT d.nome AS departamento, f.nome AS funcionario, f.data_contratacao
FROM Funcionarios f
JOIN Departamentos d ON f.departamento_id = d.id
WHERE f.data_contratacao = (
    SELECT MAX(data_contratacao)
    FROM Funcionarios
    WHERE departamento_id = d.id
);
```

Neste código, estamos criando duas tabelas: "Funcionarios" e "Departamentos". A tabela "Funcionarios" contém informações sobre os funcionários, como nome, salário, data de contratação e o ID do departamento em que trabalham (referenciando a tabela "Departamentos"). A tabela "Departamentos" armazena informações sobre os departamentos da empresa, como nome e localização.

Após criar as tabelas, inserimos alguns dados fictícios em ambas as tabelas. Em seguida, realizamos algumas consultas para obter informações do banco de dados:

1. Consulta para obter o nome e salário dos funcionários com salário maior que 5500.
2. Consulta para calcular a média salarial por departamento.
3. Consulta para obter o nome do funcionário mais recentemente contratado em cada departamento.

Essas consultas ilustram diferentes operações que podem ser realizadas em um banco de dados SQL, como seleção de dados, cálculos e junção de tabelas.