```cool
class Main inherits IO {
    main() : Object {
        var length : Int <- 10;
        var sum : Int <- 0;
        var i : Int <- 0;
        while ( i < length ) loop {
            sum <- sum + i;
            i <- i + 1;
        } pool;
        out_int(sum);
        return 0;
    }
}
```

Explanation:

1. Class Definition:

   ```cool
   class Main inherits IO {
   ```

   - We define a class named `Main` that inherits from the built-in class `IO`. This means that `Main` will have access to input/output operations.

2. Main Method:

   ```cool
   main() : Object {
   ```

   - We define the main method with the return type `Object`. This is the entry point of our program.

3. Variable Declarations:

   ```cool
   var length : Int <- 10;
   var sum : Int <- 0;
   var i : Int <- 0;
   ```

   - We declare three integer variables:
     - `length` is initialized to 10, representing the number of iterations in our loop.
     - `sum` is initialized to 0, which will be used to store the sum of integers.
     - `i` is initialized to 0, which will be used as the loop counter.

4. While Loop:

   ```cool
   while ( i < length ) loop {
   ```

   - We use a `while` loop to iterate from 0 to `length-1`.

5. Loop Body:

   ```cool
   sum <- sum + i;
   i <- i + 1;
   ```

   - Inside the loop, we add the current value of `i` to `sum` and increment `i`.

6. Output:

   ```cool
   out_int(sum);
   ```

   - After the loop, we use the `out_int` method to print the value of `sum` to the console.

7. Return Statement:

   ```cool
   return 0;
   ```

   - Finally, we return 0 to indicate a successful program execution.