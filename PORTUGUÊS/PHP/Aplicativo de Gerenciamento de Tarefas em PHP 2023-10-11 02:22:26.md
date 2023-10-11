Claro! Vou criar um código complexo em PHP para você. Este código vai simular uma aplicação de gerenciamento de tarefas, onde os usuários podem criar, visualizar, editar e excluir tarefas. 

Vamos começar criando um banco de dados para armazenar as tarefas. Você pode usar o MySQL ou qualquer outro banco de dados de sua escolha. Crie uma tabela chamada "tarefas" com as seguintes colunas: "id" (chave primária auto incrementável), "titulo" (varchar), "descricao" (text), "data_criacao" (datetime) e "data_atualizacao" (datetime).

Agora, vamos criar o arquivo principal, chamado "index.php". Neste arquivo, vamos começar estabelecendo a conexão com o banco de dados:

```php
<?php
$host = 'localhost';
$usuario = 'seu_usuario';
$senha = 'sua_senha';
$banco = 'seu_banco';

$conexao = mysqli_connect($host, $usuario, $senha, $banco);

if (!$conexao) {
    die('Erro ao conectar com o banco de dados: ' . mysqli_connect_error());
}
?>
```

Agora, vamos criar a função para exibir as tarefas:

```php
<?php
function exibirTarefas($conexao) {
    $query = 'SELECT * FROM tarefas';
    $result = mysqli_query($conexao, $query);

    if (mysqli_num_rows($result) > 0) {
        while ($row = mysqli_fetch_assoc($result)) {
            echo 'ID: ' . $row['id'] . '<br>';
            echo 'Título: ' . $row['titulo'] . '<br>';
            echo 'Descrição: ' . $row['descricao'] . '<br>';
            echo 'Data de Criação: ' . $row['data_criacao'] . '<br>';
            echo 'Data de Atualização: ' . $row['data_atualizacao'] . '<br><br>';
        }
    } else {
        echo 'Nenhuma tarefa encontrada.';
    }
}
?>
```

Agora, vamos criar a função para adicionar uma nova tarefa:

```php
<?php
function adicionarTarefa($conexao, $titulo, $descricao) {
    $dataCriacao = date('Y-m-d H:i:s');
    $dataAtualizacao = date('Y-m-d H:i:s');

    $query = "INSERT INTO tarefas (titulo, descricao, data_criacao, data_atualizacao) VALUES ('$titulo', '$descricao', '$dataCriacao', '$dataAtualizacao')";
    mysqli_query($conexao, $query);
}
?>
```

Em seguida, vamos criar a função para editar uma tarefa existente:

```php
<?php
function editarTarefa($conexao, $id, $titulo, $descricao) {
    $dataAtualizacao = date('Y-m-d H:i:s');

    $query = "UPDATE tarefas SET titulo = '$titulo', descricao = '$descricao', data_atualizacao = '$dataAtualizacao' WHERE id = $id";
    mysqli_query($conexao, $query);
}
?>
```

Por fim, vamos criar a função para excluir uma tarefa:

```php
<?php
function excluirTarefa($conexao, $id) {
    $query = "DELETE FROM tarefas WHERE id = $id";
    mysqli_query($conexao, $query);
}
?>
```

Agora, podemos utilizar essas funções no nosso arquivo "index.php". Vamos adicionar um formulário para adicionar uma nova tarefa:

```php
<form method="POST" action="">
    <label for="titulo">Título:</label>
    <input type="text" name="titulo" id="titulo" required>

    <label for="descricao">Descrição:</label>
    <textarea name="descricao" id="descricao" required></textarea>

    <input type="submit" value="Adicionar">
</form>
```

No início do arquivo, vamos verificar se o formulário foi enviado e adicionar a nova tarefa:

```php
<?php
if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    $titulo = $_POST['titulo'];
    $descricao = $_POST['descricao'];

    adicionarTarefa($conexao, $titulo, $descricao);
}
?>
```

Abaixo do formulário, vamos exibir as tarefas existentes:

```php
<?php
exibirTarefas($conexao);
?>
```

Agora, vamos adicionar um formulário para editar uma tarefa:

```php
<form method="POST" action="">
    <label for="id">ID da Tarefa:</label>
    <input type="text" name="id" id="id" required>

    <label for="novoTitulo">Novo Título:</label>
    <input type="text" name="novoTitulo" id="novoTitulo" required>

    <label for="novaDescricao">Nova Descrição:</label>
    <textarea name="novaDescricao" id="novaDescricao" required></textarea>

    <input type="submit" value="Editar">
</form>
```

No início do arquivo, vamos verificar se o formulário foi enviado e editar a tarefa:

```php
<?php
if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    $id = $_POST['id'];
    $novoTitulo = $_POST['novoTitulo'];
    $novaDescricao = $_POST['novaDescricao'];

    editarTarefa($conexao, $id, $novoTitulo, $novaDescricao);
}
?>
```

Por fim, vamos adicionar um formulário para excluir uma tarefa:

```php
<form method="POST" action="">
    <label for="idExcluir">ID da Tarefa:</label>
    <input type="text" name="idExcluir" id="idExcluir" required>

    <input type="submit" value="Excluir">
</form>
```

No início do arquivo, vamos verificar se o formulário foi enviado e excluir a tarefa:

```php
<?php
if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    $idExcluir = $_POST['idExcluir'];

    excluirTarefa($conexao, $idExcluir);
}
?>
```

Isso conclui o código complexo em PHP para a aplicação de gerenciamento de tarefas. Lembre-se de ajustar as configurações de conexão com o banco de dados de acordo com o seu ambiente.