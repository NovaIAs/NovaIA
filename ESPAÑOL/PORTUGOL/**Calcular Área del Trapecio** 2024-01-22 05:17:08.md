```portuguol
programa CalculaAreaTrapezio {

  funcao CalculaAreaTrapezio(baseMaior: real; baseMenor: real; altura: real): real {
    var area: real;
    area := ((baseMaior + baseMenor) * altura) / 2;
    retorna area;
  }

  inicio
    var baseMaior, baseMenor, altura, area: real;
    escreva("Informe a base maior do trapézio: ");
    leia(baseMaior);
    escreva("Informe a base menor do trapézio: ");
    leia(baseMenor);
    escreva("Informe a altura do trapézio: ");
    leia(altura);
    area := CalculaAreaTrapezio(baseMaior, baseMenor, altura);
    escreva("A área do trapézio é: ", area);
  fim.

}
```

Este código calcula a área de um trapézio. Ele usa uma função chamada `CalculaAreaTrapezio` que recebe a base maior, a base menor e a altura do trapézio e retorna a área.

A função `CalculaAreaTrapezio` usa a fórmula da área do trapézio: `A = ((b1 + b2) * h) / 2`, onde `b1` é a base maior, `b2` é a base menor e `h` é a altura.

A função `CalculaAreaTrapezio` é chamada no programa principal, que também solicita ao usuário que informe a base maior, a base menor e a altura do trapézio.

Depois que o usuário informa os valores, o programa calcula a área do trapézio usando a função `CalculaAreaTrapezio` e exibe o resultado na tela.