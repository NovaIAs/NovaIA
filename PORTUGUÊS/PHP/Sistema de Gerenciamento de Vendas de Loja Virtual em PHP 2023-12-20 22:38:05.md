Claro, vou criar um código complexo em PHP para você. Este código consiste em um sistema de gerenciamento de vendas de uma loja virtual. Ele usa um banco de dados MySQL para armazenar os produtos, clientes e pedidos. Vou explicar o código em detalhes.

Antes de começar, certifique-se de ter um servidor web local configurado com PHP e MySQL. Crie um banco de dados chamado "lojavirtual" e importe a seguinte estrutura de tabelas:

```sql
CREATE TABLE produtos (
    id INT(11) AUTO_INCREMENT PRIMARY KEY,
    nome VARCHAR(255) NOT NULL,
    preco DECIMAL(10, 2) NOT NULL
);

CREATE TABLE clientes (
    id INT(11) AUTO_INCREMENT PRIMARY KEY,
    nome VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL
);

CREATE TABLE pedidos (
    id INT(11) AUTO_INCREMENT PRIMARY KEY,
    cliente_id INT(11) NOT NULL,
    produto_id INT(11) NOT NULL,
    quantidade INT(11) NOT NULL,
    total DECIMAL(10, 2) NOT NULL,
    data_pedido DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

Agora, vamos ao código PHP:

```php
<?php
// Função para conectar ao banco de dados
function conectar() {
    $host = "localhost";
    $usuario = "root";
    $senha = "";
    $banco = "lojavirtual";
    
    $conexao = mysqli_connect($host, $usuario, $senha, $banco);
    
    if (mysqli_connect_errno()) {
        die("Falha na conexão com o banco de dados: " . mysqli_connect_error());
    }
    
    return $conexao;
}

// Função para exibir a lista de produtos
function exibirProdutos() {
    $conexao = conectar();
    $query = "SELECT * FROM produtos";
    $result = mysqli_query($conexao, $query);
    
    echo "<h2>Produtos Disponíveis:</h2>";
    
    if (mysqli_num_rows($result) > 0) {
        while ($row = mysqli_fetch_assoc($result)) {
            echo $row["id"] . ": " . $row["nome"] . " - R$" . $row["preco"] . "<br>";
        }
    } else {
        echo "Não há produtos disponíveis.";
    }
    
    mysqli_close($conexao);
}

// Função para exibir a lista de clientes
function exibirClientes() {
    $conexao = conectar();
    $query = "SELECT * FROM clientes";
    $result = mysqli_query($conexao, $query);
    
    echo "<h2>Clientes Cadastrados:</h2>";
    
    if (mysqli_num_rows($result) > 0) {
        while ($row = mysqli_fetch_assoc($result)) {
            echo $row["id"] . ": " . $row["nome"] . " - " . $row["email"] . "<br>";
        }
    } else {
        echo "Não há clientes cadastrados.";
    }
    
    mysqli_close($conexao);
}

// Função para realizar um novo pedido
function realizarPedido($cliente_id, $produto_id, $quantidade) {
    $conexao = conectar();
    
    // Verifica se o cliente existe
    $query = "SELECT * FROM clientes WHERE id = $cliente_id";
    $result = mysqli_query($conexao, $query);
    
    if (mysqli_num_rows($result) == 0) {
        echo "Cliente não encontrado.";
        mysqli_close($conexao);
        return;
    }
    
    // Verifica se o produto existe
    $query = "SELECT * FROM produtos WHERE id = $produto_id";
    $result = mysqli_query($conexao, $query);
    
    if (mysqli_num_rows($result) == 0) {
        echo "Produto não encontrado.";
        mysqli_close($conexao);
        return;
    }
    
    // Calcula o total do pedido
    $row = mysqli_fetch_assoc($result);
    $total = $row["preco"] * $quantidade;
    
    // Insere o pedido no banco
    $query = "INSERT INTO pedidos (cliente_id, produto_id, quantidade, total) VALUES ($cliente_id, $produto_id, $quantidade, $total)";
    
    if (mysqli_query($conexao, $query)) {
        echo "Pedido realizado com sucesso.";
    } else {
        echo "Erro ao realizar o pedido: " . mysqli_error($conexao);
    }
    
    mysqli_close($conexao);
}

// Exemplo de uso
exibirProdutos();
exibirClientes();
realizarPedido(1, 2, 3);
```

Este código PHP cria funções para exibir a lista de produtos e clientes, além de permitir a realização de um novo pedido. Ele se conecta ao banco de dados usando a função `conectar()` e executa consultas SQL para obter e exibir os dados.

Note que as informações de conexão ao banco de dados (host, usuário, senha e nome do banco) estão definidas na função `conectar()`. Certifique-se de atualizá-las de acordo com sua configuração.

Para testar o código, salve-o em um arquivo PHP e acesse-o pelo navegador. Você verá a lista de produtos e clientes, e um pedido será realizado no final.

Espero que este código complexo atenda às suas expectativas!