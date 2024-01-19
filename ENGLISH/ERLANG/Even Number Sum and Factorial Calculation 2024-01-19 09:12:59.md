```erlang
% Define the main function.
main() ->
    % Create a list of numbers from 1 to 100.
    Numbers = lists:seq(1, 100),

    % Use a comprehension to calculate the sum of the numbers in the list.
    Sum = lists:sum([N || N <- Numbers, N rem 2 == 0]),

    % Create a string with the result.
    Result = io_format("The sum of the even numbers in the list is ~p~n", [Sum]),

    % Print the result to the console.
    io:fwrite(Result).

% Define a function to calculate the factorial of a number.
factorial(N) ->
    % Base case: the factorial of 0 is 1.
    if
        N == 0 ->
            1;
        % Recursive case: calculate the factorial of N by multiplying N by the factorial of N-1.
        true ->
            N * factorial(N-1)
    end.
```

Explanation:

1. The `main` function is defined as the entry point of the program.
2. A list of numbers from 1 to 100 is created using the `lists:seq` function.
3. A comprehension is used to filter the list and select only the even numbers.
4. The `lists:sum` function is used to calculate the sum of the selected numbers.
5. The result is formatted into a string using the `io_format` function.
6. The formatted result is printed to the console using the `io:fwrite` function.
7. The `factorial` function is defined to calculate the factorial of a number.
8. The factorial of a number is calculated using a recursive algorithm.
9. The base case is when the number is 0, in which case the factorial is 1.
10. The recursive case is when the number is greater than 0, in which case the factorial is calculated by multiplying the number by the factorial of the number minus 1.