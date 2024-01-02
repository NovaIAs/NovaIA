Sure! Here's a complex and unique shell code that generates a random alphanumeric string of a specified length:

```shell
#!/bin/bash

generate_random_string() {
    length=$1
    string=""
    characters="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

    for (( i=0; i<$length; i++ )); do
        random_index=$((RANDOM % ${#characters}))
        random_char=${characters:$random_index:1}
        string="${string}${random_char}"
    done

    echo "$string"
}

read -p "Enter the length of the random string: " length
random_string=$(generate_random_string $length)
echo "Random string of length $length: $random_string"
```

Explanation:
1. The script starts with a shebang (`#!/bin/bash`) which indicates that the script should be interpreted by the Bash shell.
2. The `generate_random_string` function is defined to generate a random alphanumeric string of a specified length.
3. The function takes the desired length as a parameter and initializes an empty string called `string`.
4. The `characters` variable contains all possible characters that can be used in the random string: lowercase letters, uppercase letters, and digits.
5. A loop is executed `length` number of times to generate each character of the random string.
6. Inside the loop, a random index is generated using the `RANDOM` variable modulo the length of the `characters` string, ensuring the selection of a random character from the string.
7. The selected random character is appended to the `string` variable.
8. Once the loop is complete, the generated random string is echoed to the console.
9. The user is prompted to enter the desired length of the random string.
10. The `generate_random_string` function is called with the specified length as an argument.
11. The generated random string is displayed on the console with a message indicating its length.

This code generates a highly differentiated random string of any specified length. Each time the script is executed, a unique random string will be generated.