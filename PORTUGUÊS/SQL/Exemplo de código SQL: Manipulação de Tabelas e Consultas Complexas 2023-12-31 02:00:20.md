Certamente! Aqui está um exemplo de um código SQL complexo e diferenciado, que explora várias funcionalidades da linguagem:

```
-- Criação da tabela de funcionários
CREATE TABLE Funcionarios (
    ID INT PRIMARY KEY,
    Nome VARCHAR(100),
    Sobrenome VARCHAR(100),
    DataNascimento DATE,
    Salario DECIMAL(10, 2),
    DepartamentoID INT
);

-- Criação da tabela de departamentos
CREATE TABLE Departamentos (
    ID INT PRIMARY KEY,
    Nome VARCHAR(100),
    Localizacao VARCHAR(100)
);

-- Inserção de dados na tabela de departamentos
INSERT INTO Departamentos (ID, Nome, Localizacao)
VALUES
    (1, 'Vendas', 'São Paulo'),
    (2, 'Marketing', 'Rio de Janeiro'),
    (3, 'Tecnologia', 'Belo Horizonte');

-- Inserção de dados na tabela de funcionários
INSERT INTO Funcionarios (ID, Nome, Sobrenome, DataNascimento, Salario, DepartamentoID)
VALUES
    (1, 'João', 'Silva', '1990-01-01', 5000.00, 1),
    (2, 'Maria', 'Santos', '1995-05-10', 6000.00, 2),
    (3, 'Pedro', 'Ferreira', '1985-12-15', 7000.00, 3),
    (4, 'Ana', 'Gomes', '1992-07-20', 5500.00, 1),
    (5, 'Carlos', 'Ribeiro', '1988-03-25', 6500.00, 2);

-- Consulta de funcionários por departamento ordenados por salário
SELECT
    f.ID,
    f.Nome,
    f.Sobrenome,
    f.Salario,
    d.Nome AS Departamento
FROM
    Funcionarios f
    JOIN Departamentos d ON f.DepartamentoID = d.ID
ORDER BY
    d.Nome ASC,
    f.Salario DESC;

-- Atualização do salário dos funcionários do departamento de Vendas
UPDATE Funcionarios
SET Salario = Salario * 1.1
WHERE DepartamentoID = 1;

-- Remoção dos funcionários com salário inferior a 6000.00
DELETE FROM Funcionarios
WHERE Salario < 6000.00;

-- Consulta do número total de funcionários por departamento
SELECT
    d.Nome AS Departamento,
    COUNT(*) AS TotalFuncionarios
FROM
    Departamentos d
    LEFT JOIN Funcionarios f ON d.ID = f.DepartamentoID
GROUP BY
    d.Nome;
```

Neste exemplo de código SQL, começo criando duas tabelas: "Funcionarios" e "Departamentos". Em seguida, insiro dados nessas tabelas usando comandos INSERT INTO.

Logo após, faço uma consulta complexa para obter os funcionários de cada departamento, ordenando-os por salário. Utilizo um JOIN para relacionar as tabelas "Funcionarios" e "Departamentos" e aproveito para renomear a coluna "Nome" da tabela "Departamentos" como "Departamento" na consulta.

Em seguida, faço uma atualização nos salários dos funcionários do departamento de Vendas, aumentando-os em 10%. Utilizo o comando UPDATE com uma cláusula WHERE para selecionar apenas os funcionários do departamento desejado.

Após a atualização, removo os funcionários que possuem salário inferior a 6000.00, utilizando o comando DELETE FROM com uma cláusula WHERE.

Por fim, faço uma consulta para obter o número total de funcionários por departamento, usando um LEFT JOIN para incluir os departamentos sem funcionários na contagem. Agrupo os resultados utilizando o comando GROUP BY.

Espero que esse código complexo e explicativo possa lhe ajudar em suas necessidades!