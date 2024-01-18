```portugol
program DivisaoInteiraResto {

  function DivisaoInteira {

    local inteiro Dividendo, Divisor, Quociente;

    read Dividendo;
    read Divisor;
    Quociente := Dividendo div Divisor;
    return Quociente;

  }

  function Resto {

    local inteiro Dividendo, Divisor, Resto;

    read Dividendo;
    read Divisor;
    Resto := Dividendo mod Divisor;
    return Resto;

  }

  # Função Mínimo
  function minimo(inteiro a, inteiro b) {  

    if a < b then # Se a for menor que b  
      return a; # Retorna a   
    else  # Se não  
      return b; # Retorna b  
    end if;  
  }  

  # Função Máximo
  function maximo(inteiro a, inteiro b) {  

    if a > b then # Se a for maior que b  
      return a; # Retorna a  
    else  # Se não  
      return b; # Retorna b  
    end if;  
  }  

  # Programa principal  

  local booleano Loop;  
  Loop := True;  

  while Loop do  

    local inteiro Opcao, Dividendo, Divisor, Quociente, Resto, Minimo, Maximo;  

    print("Escolha uma opção:");  
    print("1 - Divisão Inteira");  
    print("2 - Resto da Divisão");  
    print("3 - Mínimo de Dois Números");  
    print("4 - Máximo de Dois Números");  
    print("5 - Sair");  
    read Opcao;  

    switch Opcao do  

      case 1:  

        Quociente := DivisaoInteira();  
        print("O quociente da divisão é:", Quociente);  
        break;  

      case 2:  

        Resto := Resto();  
        print("O resto da divisão é:", Resto);  
        break;  

      case 3:  

        print("Informe o primeiro número:");  
        read Dividendo;  
        print("Informe o segundo número:");  
        read Divisor;  
        Minimo := minimo(Dividendo, Divisor);  
        print("O menor número é:", Minimo);  
        break;  

      case 4:  

        print("Informe o primeiro número:");  
        read Dividendo;  
        print("Informe o segundo número:");  
        read Divisor;  
        Maximo := maximo(Dividendo, Divisor);  
        print("O maior número é:", Maximo);  
        break;  

      case 5:  

        Loop := False;  
        break;  

      default:  

        print("Opção inválida!");  

    end switch;  

  end while;  
}
```

This code implements a program that gives the user the option to perform various mathematical operations, including whole number division, finding the remainder of a division, finding the minimum of two numbers, finding the maximum of two numbers, and exiting the program. The code uses functions to perform the operations. The `DivisaoInteira` function takes two integers as input and returns the quotient of the division of the first integer by the second integer. The `Resto` function takes two integers as input and returns the remainder of the division of the first integer by the second integer. The `minimo` function takes two integers as input and returns the smaller of the two integers. The `maximo` function takes two integers as input and returns the larger of the two integers.

The main program first prints a menu of options to the user. The user can choose to perform one of the four operations or to exit the program. Depending on the user's choice, the program calls the appropriate function to perform the operation. If the user chooses to exit the program, the program terminates.

Here is an example of how the program could be used:

```
Escolha uma opção:
1 - Divisão Inteira
2 - Resto da Divisão
3 - Mínimo de Dois Números
4 - Máximo de Dois Números
5 - Sair
1
Informe o dividendo:
10
Informe o divisor:
3
O quociente da divisão é: 3

Escolha uma opção:
1 - Divisão Inteira
2 - Resto da Divisão
3 - Mínimo de Dois Números
4 - Máximo de Dois Números
5 - Sair
2
Informe o dividendo:
10
Informe o divisor:
3
O resto da divisão é: 1

Escolha uma opção:
1 - Divisão Inteira
2 - Resto da Divisão
3 - Mínimo de Dois Números
4 - Máximo de Dois Números
5 - Sair
3
Informe o primeiro número:
5
Informe o segundo número:
8
O menor número é: 5

Escolha uma opção:
1 - Divisão Inteira
2 - Resto da Divisão
3 - Mínimo de Dois Números
4 - Máximo de Dois Números
5 - Sair
4
Informe o primeiro número:
5
Informe o segundo número:
8
O maior número é: 8

Escolha uma opção:
1 - Divisão Inteira
2 - Resto da Divisão
3 - Mínimo de Dois Números
4 - Máximo de Dois Números
5 - Sair
5
```