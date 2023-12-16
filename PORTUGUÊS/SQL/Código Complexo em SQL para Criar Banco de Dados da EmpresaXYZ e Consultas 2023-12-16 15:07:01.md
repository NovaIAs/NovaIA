Claro! Aqui está um exemplo de um código complexo em SQL. Ele cria uma base de dados de uma empresa fictícia chamada "EmpresaXYZ" e inclui tabelas, chaves primárias, chaves estrangeiras e alguns exemplos de consultas.

```sql
-- Criar a base de dados EmpresaXYZ
CREATE DATABASE EmpresaXYZ;

-- Usar a base de dados EmpresaXYZ
USE EmpresaXYZ;

-- Criar a tabela de departamentos
CREATE TABLE Departamento (
  id_departamento INT PRIMARY KEY,
  nome_departamento VARCHAR(50),
  localizacao VARCHAR(100)
);

-- Criar a tabela de funcionários
CREATE TABLE Funcionario (
  id_funcionario INT PRIMARY KEY,
  nome_funcionario VARCHAR(50),
  salario DECIMAL(10, 2),
  id_departamento INT,
  FOREIGN KEY (id_departamento) REFERENCES Departamento(id_departamento)
);

-- Inserir dados na tabela Departamento
INSERT INTO Departamento (id_departamento, nome_departamento, localizacao) VALUES
  (1, 'Vendas', 'São Paulo'),
  (2, 'Marketing', 'Rio de Janeiro'),
  (3, 'RH', 'Brasília');

-- Inserir dados na tabela Funcionario
INSERT INTO Funcionario (id_funcionario, nome_funcionario, salario, id_departamento) VALUES
  (1, 'João Silva', 5000.00, 1),
  (2, 'Maria Santos', 6000.00, 1),
  (3, 'Pedro Almeida', 4500.00, 2),
  (4, 'Ana Oliveira', 5500.00, 2),
  (5, 'Carlos Souza', 4000.00, 3);

-- Consulta 1: Listar todos os departamentos
SELECT * FROM Departamento;

-- Consulta 2: Listar todos os funcionários e seus salários
SELECT nome_funcionario, salario FROM Funcionario;

-- Consulta 3: Listar os funcionários do departamento de Vendas
SELECT nome_funcionario FROM Funcionario WHERE id_departamento = 1;
```

Esse código cria uma base de dados chamada "EmpresaXYZ" com duas tabelas: "Departamento" e "Funcionario". A tabela "Departamento" tem as colunas id_departamento, nome_departamento e localizacao. A tabela "Funcionario" tem as colunas id_funcionario, nome_funcionario, salario e id_departamento (chave estrangeira referenciando a tabela Departamento).

O código insere alguns dados nas tabelas e também inclui três exemplos de consultas. A primeira consulta lista todos os departamentos, a segunda consulta lista todos os funcionários e seus salários, e a terceira consulta lista os funcionários do departamento de Vendas.

Espero que esse código complexo em SQL seja útil para você!