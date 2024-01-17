```portugol
program conta_caracteres;
#include <stdio.h>

int main() {
    char texto[100];
    int i, vogais = 0, consoantes = 0, outros = 0;

    printf("Digite um texto: ");
    gets(texto);

    for (i = 0; texto[i] != '\0'; i++) {
        // Verificação da letra
        if (texto[i] >= 'a' && texto[i] <= 'z') {
            texto[i] = toupper(texto[i]);
        }

        // Contagem dos caracteres
        if (texto[i] == 'A' || texto[i] == 'E' || texto[i] == 'I' || texto[i] == 'O' || texto[i] == 'U') {
            vogais++;
        } else if ((texto[i] >= 'B' && texto[i] <= 'Z') || texto[i] == ' ') {
            consoantes++;
        } else {
            outros++;
        }
    }

    // Impressão dos resultados
    printf("\nNúmero de vogais: %d\n", vogais);
    printf("Número de consoantes: %d\n", consoantes);
    printf("Número de outros caracteres: %d\n", outros);

    return 0;
}
```

This program reads a text from the user and counts the number of vowels, consonants, and other characters in it.

The program first includes the `<stdio.h>` header file, which contains the definition of the `gets()` and `printf()` functions.

It then declares an array of characters `texto` with a size of 100, which will be used to store the user's input.

It also declares four integers: `i`, which will be used as a loop counter; `vogais`, which will be used to count the number of vowels; `consoantes`, which will be used to count the number of consonants; and `outros`, which will be used to count the number of other characters.

The program then uses the `gets()` function to read the user's input and store it in the `texto` array.

It then enters a `for` loop that iterates through each character in the `texto` array.

For each character, the program first checks if it is a lowercase letter. If it is, it converts it to uppercase using the `toupper()` function.

The program then checks if the character is a vowel (A, E, I, O, or U). If it is, it increments the `vogais` counter.

The program then checks if the character is a consonant (B, C, D, ..., Z) or a space. If it is, it increments the `consoantes` counter.

Otherwise, the program increments the `outros` counter.

After the loop has finished, the program prints out the number of vowels, consonants, and other characters in the text.