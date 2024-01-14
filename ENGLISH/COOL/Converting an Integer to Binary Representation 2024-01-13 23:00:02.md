```cool
class Main {
    static main() {
        let n = 10;
        let s = "";
        while (n > 0) {
            if (n % 2 == 0) {
                s = s + "0";
            } else {
                s = s + "1";
            }
            n = n / 2;
        }
        print(s);
    }
}
```

This code converts an integer to its binary representation. It starts by creating a variable `n` and assigning it the value 10. It then creates a variable `s` and assigns it an empty string.

The `while` loop is used to repeatedly divide `n` by 2 and check if the remainder is 0 or 1. If the remainder is 0, it means that the current bit in the binary representation is 0, so the string `s` is concatenated with the character '0'. If the remainder is 1, it means that the current bit in the binary representation is 1, so the string `s` is concatenated with the character '1'.

The loop continues until `n` reaches 0, at which point the binary representation of the original number `n` is stored in the string `s`. The `print` statement is then used to print the string `s` to the console.

The output of the code is:

```
1010
```