```portugol
program CalcularMedia {
    funcao CalcularMedia(lista: array[1..10] de real): real;
    var
        soma: real;
        i: inteiro;
    inicio
        soma := 0;
        para i de 1 ate 10 faca
            soma := soma + lista[i];
        fim para;
        retorna (soma / 10);
    fim funcao;

    var
        notas: array[1..10] de real;
        media: real;
        i: inteiro;
    inicio
        para i de 1 ate 10 faca
            escreva("Digite a nota do aluno ", i, ": ");
            leia(notas[i]);
        fim para;
        media := CalcularMedia(notas);
        escreva("A média das notas é ", media);
    fim.
}
```

This code calculates the average of 10 grades in PORTUGOL.

The `CalcularMedia` function receives an array of 10 real numbers, which are the grades, and returns the average of the grades.
The `CalcularMedia` function iterates over the array and sums the grades.
It then divides the sum by 10, which is the number of grades, and returns the result.

The main program calls the `CalcularMedia` function to compute the average of the grades.
It first creates an array of 10 real numbers called `notas`, which stores the grades.
It then uses a for loop to read the grades from the user and store them in the `notas` array.
Once all the grades have been read, the main program calls the `CalcularMedia` function, passing in the `notas` array.
The `CalcularMedia` function returns the average of the grades, which is stored in the `media` variable.
The main program then prints the average of the grades to the console.