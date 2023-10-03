Claro! Aqui está um exemplo de um código em COOL que cria uma calculadora básica com as operações de soma, subtração, multiplicação e divisão:

```cool
class Calculadora {
    (* Método principal que executa a calculadora *)
    main(): Object {
        let opcao : Int <- 0;
        let continuar : Bool <- true;

        while continuar loop
            opcao <- self.menu();
            let resultado : Int <- 0;
            let a : Int <- 0;
            let b : Int <- 0;

            if opcao = 0 then
                continuar <- false;
            else
                out_string("Digite o primeiro número: ");
                a <- self.ler_numero();
                out_string("Digite o segundo número: ");
                b <- self.ler_numero();

                if opcao = 1 then
                    resultado <- a + b;
                    out_string("A soma dos números é: ");
                elseif opcao = 2 then
                    resultado <- a - b;
                    out_string("A subtração dos números é: ");
                elseif opcao = 3 then
                    resultado <- a * b;
                    out_string("A multiplicação dos números é: ");
                elseif opcao = 4 then
                    if b = 0 then
                        out_string("Divisão por zero não é permitida!");
                    else
                        resultado <- a / b;
                        out_string("A divisão dos números é: ");
                    end;
                end;

                if opcao = 1 or opcao = 2 or opcao = 3 or (opcao = 4 and b != 0) then
                    out_int(resultado);
                end;

                out_string("\n");
            end;
        pool;

        out_string("Calculadora encerrada.\n");
        return void;
    };

    (* Função para exibir o menu e ler a opção escolhida pelo usuário *)
    menu(): Int {
        out_string("---------- Calculadora ----------\n");
        out_string("Escolha uma opção:\n");
        out_string("1. Soma\n");
        out_string("2. Subtração\n");
        out_string("3. Multiplicação\n");
        out_string("4. Divisão\n");
        out_string("0. Sair\n");
        out_string("Digite o número da opção desejada: ");
        let opcao : Int <- self.ler_numero();
        out_string("\n");
        return opcao;
    };

    (* Função para ler um número digitado pelo usuário *)
    ler_numero(): Int {
        let numero : Int <- 0;
        let valido : Bool <- false;

        while not valido loop
            let entrada : String <- in_string();
            numero <- atoi(entrada);

            if number? then
                valido <- true;
            else
                out_string("Entrada inválida! Digite um número válido: ");
            end;
        pool;

        return numero;
    };
};
```

Este código cria uma classe `Calculadora` que possui um método principal `main()`. A calculadora apresenta um menu com opções para o usuário escolher a operação desejada. O usuário pode selecionar a operação inserindo um número correspondente. Em seguida, a calculadora solicita os dois números para realizar a operação escolhida.

Depois de executar a operação, o resultado é exibido na tela. O código também verifica se o usuário tentou realizar uma divisão por zero e exibe uma mensagem de erro nesse caso.

Para ler a entrada do usuário, a calculadora utiliza a função `ler_numero()`, que verifica se o valor digitado é um número válido antes de retorná-lo.

É importante ressaltar que o COOL é uma linguagem de programação fictícia criada para fins acadêmicos, portanto, esse código não pode ser executado diretamente em um ambiente real.