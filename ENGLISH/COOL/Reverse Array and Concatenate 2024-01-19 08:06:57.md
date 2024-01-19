```cool
class Main is
  main() : Object =>
    let
      array := Array.new(5);
      int i := 0;
      string str := ("hello\n");
    in
      for i := 0 to 4 do
        array[i] := i;
      end;
      for i := 4 to 0 by -1 do
        str := str + string_of_int(array[i]);
      end;
      str
    end;
end Main;
```

This code is a bit more complex than the previous ones. It creates an array of integers, initializes it with values from 0 to 4, and then uses a loop to concatenate the string representation of each array element to a string. Finally, it returns the string.

The code is explained step by step below:

1. The class `Main` is defined.
2. The method `main` is defined. This method has no parameters and returns an object.
3. A local variable `array` is declared and initialized to a new array of integers with a size of 5.
4. A local variable `i` is declared and initialized to 0.
5. A local variable `str` is declared and initialized to the string `"hello\n"`.
6. A `for` loop is used to iterate over the elements of the array from 0 to 4.
7. Inside the loop, the value of `i` is used to access the current element of the array, and the value of the element is assigned to the current element of the array.
8. Another `for` loop is used to iterate over the elements of the array from 4 to 0, decrementing by 1 each time.
9. Inside the loop, the value of `i` is used to access the current element of the array, and the string representation of the element is concatenated to the string `str`.
10. Finally, the string `str` is returned.

When you run this code, it will output the following:

```
hello
43210
```