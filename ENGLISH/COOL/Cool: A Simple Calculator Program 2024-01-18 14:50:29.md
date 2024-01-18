```cool
class Main is
  main() : Int -> Object is
    self.printInt(self.sum(1, 2)) + self.printInt(self.sub(2, 1))
  ;
  sum : Int * Int -> Int is
    *1 + *2
  ;
  sub : Int * Int -> Int is
    *1 - *2
  ;
  printInt : Int -> Object is
    IO.out_int(*1)
  ;
  program() : Object is
    self.main()
  ;
end Main
```
Explanation:

1. **`class Main is`**: This line declares a new class named `Main`. All code within this class will be indented.

2. **`main() : Int -> Object is`**: This line declares a method named `main` within the `Main` class. It takes an integer as input and returns an object (in this case, an integer).

3. **`self.printInt(self.sum(1, 2)) + self.printInt(self.sub(2, 1))`**: This line calls three methods: `sum`, `sub`, and `printInt`.
   - `self.sum(1, 2)` calls the `sum` method with the arguments 1 and 2, which returns the result of adding them (3).
   - `self.sub(2, 1)` calls the `sub` method with the arguments 2 and 1, which returns the result of subtracting them (1).
   - `self.printInt(3)` and `self.printInt(1)` call the `printInt` method with the arguments 3 and 1, respectively. This prints the values to the console.

4. **`sum : Int * Int -> Int is`**: This line declares a method named `sum` within the `Main` class. It takes two integers as input and returns an integer.

5. **`*1 + *2`**: This line adds the two input integers and returns the result.

6. **`sub : Int * Int -> Int is`**: This line declares a method named `sub` within the `Main` class. It takes two integers as input and returns an integer.

7. **`*1 - *2`**: This line subtracts the second input integer from the first input integer and returns the result.

8. **`printInt : Int -> Object is`**: This line declares a method named `printInt` within the `Main` class. It takes an integer as input and returns an object (in this case, nothing).

9. **`IO.out_int(*1)`**: This line uses the `out_int` method from the `IO` class to print the input integer to the console.

10. **`program() : Object is`**: This line declares a method named `program` within the `Main` class. It takes no arguments and returns an object (in this case, nothing).

11. **`self.main()`**: This line calls the `main` method within the `Main` class.

12. **`end Main`**: This line ends the definition of the `Main` class.

When you run this program, it will print the following output to the console:

```
31
```

This is because the `sum` method returns 3, the `sub` method returns 1, and the `printInt` method prints each of these values to the console.