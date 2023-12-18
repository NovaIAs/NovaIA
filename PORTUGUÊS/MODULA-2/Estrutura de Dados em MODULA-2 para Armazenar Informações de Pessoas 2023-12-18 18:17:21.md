{
    DEFINITIONS:
        TYPE 
            Pessoa = RECORD
                nome: ARRAY [0..50] OF CHAR;
                idade: INTEGER;
            END;
            
            ListaPessoas = RECORD
                dados: ARRAY [1..100] OF Pessoa;
                total: INTEGER;
            END;

    PROCEDURE InicializarLista(VAR lista: ListaPessoas);
    BEGIN
        lista.total := 0;
    END;

    PROCEDURE AdicionarPessoa(VAR lista: ListaPessoas; nome: ARRAY OF CHAR; idade: INTEGER);
    BEGIN
        IF lista.total < 100 THEN
            lista.total := lista.total + 1;
            lista.dados[lista.total].nome := nome;
            lista.dados[lista.total].idade := idade;
        END;
    END;

    PROCEDURE RemoverPessoa(VAR lista: ListaPessoas; nome: ARRAY OF CHAR);
    VAR
        i, j: INTEGER;
    BEGIN
        i := 1;
        WHILE (i <= lista.total) AND (lista.dados[i].nome <> nome) DO
            i := i + 1;
        END;
        
        IF i <= lista.total THEN
            FOR j := i TO lista.total - 1 DO
                lista.dados[j] := lista.dados[j + 1];
            END;
            lista.total := lista.total - 1;
        END;
    END;

    PROCEDURE ImprimirLista(lista: ListaPessoas);
    VAR
        i: INTEGER;
    BEGIN
        IF lista.total = 0 THEN
            WRITELN('Lista vazia.')
        ELSE
            FOR i := 1 TO lista.total DO
                WRITELN('Nome:', lista.dados[i].nome, ' | Idade:', lista.dados[i].idade);
            END;
        END;
    END;

    VAR
        lista: ListaPessoas;
    
    BEGIN
        InicializarLista(lista);
        
        AdicionarPessoa(lista, 'Joao', 25);
        AdicionarPessoa(lista, 'Maria', 30);
        AdicionarPessoa(lista, 'Pedro', 35);
        
        ImprimirLista(lista);
        
        RemoverPessoa(lista, 'Maria');
        
        ImprimirLista(lista);
    END.
}

Este é um código em MODULA-2 que implementa uma estrutura de dados simples para armazenar informações de pessoas. Ele utiliza um registro chamado "Pessoa" para representar cada indivíduo, contendo um campo para o nome (um array de caracteres) e outro para a idade (um número inteiro).

Além disso, há um registro chamado "ListaPessoas", que contém um array de registros Pessoa chamado "dados" e um campo "total" que indica a quantidade de pessoas armazenadas na lista.

A função "InicializarLista" é responsável por inicializar o campo "total" da lista, definindo-o inicialmente como zero.

A função "AdicionarPessoa" permite adicionar uma nova pessoa à lista. Ela verifica se a lista ainda não está cheia (comportando no máximo 100 pessoas), incrementa o campo "total" em 1, e armazena o nome e a idade fornecidos nos campos correspondentes do próximo elemento disponível no array "dados".

A função "RemoverPessoa" permite remover uma pessoa da lista pelo nome. Ela percorre a lista procurando pelo nome fornecido e, quando o encontra, reorganiza os elementos do array para preencher o espaço vazio, diminui o campo "total" em 1.

Por fim, a função "ImprimirLista" exibe na tela todas as pessoas presentes na lista, mostrando o nome e a idade de cada uma. Se a lista estiver vazia, uma mensagem indicando isso será exibida.

No programa principal, chamamos a função "InicializarLista" para garantir que a lista comece vazia. Em seguida, adicionamos três pessoas (João, Maria e Pedro), usando a função "AdicionarPessoa", e imprimimos a lista resultante. Por fim, removemos a pessoa "Maria" utilizando a função "RemoverPessoa", e imprimimos novamente a lista para verificar as modificações.