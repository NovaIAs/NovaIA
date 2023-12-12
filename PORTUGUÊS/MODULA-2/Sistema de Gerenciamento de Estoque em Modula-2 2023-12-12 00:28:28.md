Claro! Aqui está um código complexo em MODULA-2 que utiliza estruturas de controle, funções e procedimentos para realizar uma tarefa específica. Este código é um exemplo de um sistema de gerenciamento de estoque simplificado. 

```
MODULE GerenciamentoEstoque;

FROM InOut IMPORT ReadInt, WriteString, WriteInt, WriteLn;
FROM Sets IMPORT Include, Exclude;
FROM SYSTEM IMPORT NEW, DISPOSE;

CONST
    MAX_PRODUTOS = 100;
    MAX_VENDAS = 1000;

TYPE
    Produto = RECORD
        codigo: INTEGER;
        nome: ARRAY [1..50] OF CHAR;
        quantidade: INTEGER;
        preco: REAL;
    END;

VAR
    estoque: ARRAY [1..MAX_PRODUTOS] OF Produto;
    totalProdutos: INTEGER;

PROCEDURE InicializarEstoque;
VAR
    i: INTEGER;
BEGIN
    totalProdutos := 0;
    FOR i := 1 TO MAX_PRODUTOS DO
        estoque[i].codigo := 0;
        estoque[i].nome := '';
        estoque[i].quantidade := 0;
        estoque[i].preco := 0.0;
    END;
END InicializarEstoque;

PROCEDURE CadastrarProduto;
VAR
    novoProduto: Produto;
BEGIN
    IF totalProdutos < MAX_PRODUTOS THEN
        WriteString('Digite o código do produto: ');
        novoProduto.codigo := ReadInt();
        
        WriteString('Digite o nome do produto: ');
        ReadString(novoProduto.nome);
        
        WriteString('Digite a quantidade do produto: ');
        novoProduto.quantidade := ReadInt();
        
        WriteString('Digite o preço do produto: ');
        novoProduto.preco := ReadReal();
        
        totalProdutos := totalProdutos + 1;
        estoque[totalProdutos] := novoProduto;
        
        WriteLn('Produto cadastrado com sucesso!');
    ELSE
        WriteLn('O estoque está cheio. Impossível cadastrar um novo produto.');
    END;
END CadastrarProduto;

PROCEDURE VenderProduto(codigo: INTEGER; quantidade: INTEGER);
VAR
    i: INTEGER;
BEGIN
    FOR i := 1 TO totalProdutos DO
        IF estoque[i].codigo = codigo THEN
            IF estoque[i].quantidade >= quantidade THEN
                estoque[i].quantidade := estoque[i].quantidade - quantidade;
                WriteLn('Venda realizada com sucesso!');
                RETURN;
            ELSE
                WriteLn('Não há quantidade suficiente do produto em estoque.');
                RETURN;
            END;
        END;
    END;
    
    WriteLn('Produto não encontrado no estoque.');
END VenderProduto;

PROCEDURE ImprimirEstoque;
VAR
    i: INTEGER;
BEGIN
    WriteLn('Estoque:');
    FOR i := 1 TO totalProdutos DO
        WriteLn('Código: ', estoque[i].codigo);
        WriteLn('Nome: ', estoque[i].nome);
        WriteLn('Quantidade: ', estoque[i].quantidade);
        WriteLn('Preço: ', estoque[i].preco:0:2);
    END;
END ImprimirEstoque;

PROCEDURE MenuPrincipal;
VAR
    opcao: INTEGER;
    codigo, quantidade: INTEGER;
BEGIN
    REPEAT
        WriteLn('--- Menu Principal ---');
        WriteLn('1. Cadastrar produto');
        WriteLn('2. Vender produto');
        WriteLn('3. Imprimir estoque');
        WriteLn('0. Sair');
        WriteString('Opção: ');
        opcao := ReadInt();
        
        CASE opcao OF
            1: CadastrarProduto();
            2: BEGIN
                WriteString('Digite o código do produto: ');
                codigo := ReadInt();
                WriteString('Digite a quantidade a ser vendida: ');
                quantidade := ReadInt();
                VenderProduto(codigo, quantidade);
            END;
            3: ImprimirEstoque();
            0: WriteLn('Encerrando o programa...');
            ELSE WriteLn('Opção inválida.');
        END;
    UNTIL opcao = 0;
END MenuPrincipal;

PROCEDURE Main;
BEGIN
    InicializarEstoque();
    MenuPrincipal();
END Main;

BEGIN
    Main();
END GerenciamentoEstoque.
```

Este código em MODULA-2 é um exemplo de um sistema de gerenciamento de estoque. Ele possui um menu principal que permite cadastrar produtos, vender produtos, imprimir o estoque e sair do programa. 

A estrutura do programa é dividida em procedimentos e funções para facilitar a organização e a reutilização de código. O procedimento `InicializarEstoque` é responsável por inicializar o estoque, zerando as informações de todos os produtos. O procedimento `CadastrarProduto` solicita ao usuário as informações do novo produto e o adiciona ao estoque, desde que haja espaço disponível. O procedimento `VenderProduto` recebe o código e a quantidade do produto a ser vendido e verifica se há quantidade suficiente em estoque para realizar a venda. O procedimento `ImprimirEstoque` exibe na tela todas as informações dos produtos presentes no estoque. O procedimento `MenuPrincipal` exibe um menu interativo para o usuário, onde ele pode escolher as opções disponíveis.

Espero que este código atenda às suas necessidades!