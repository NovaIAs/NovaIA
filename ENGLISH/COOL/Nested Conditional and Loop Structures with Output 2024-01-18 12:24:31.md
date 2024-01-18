```cool
class Main is
  main(): Object =>
    let i: Int <- 0 in
    let j: Int <- 10 in
    if i < j then
      let a: Int <- 1 in
      let b: Int <- 2 in
      while a < b loop
        let i: Int <- 0 in
        let j: Int <- 10 in
        if i < j then
          let c: Int <- 3 in
          let d: Int <- 4 in
          while c < d loop
            let x: Int <- 5 in
            let y: Int <- 6 in
            if x < y then
              let z: Int <- 7 in
              let w: Int <- 8 in
              while z < w loop
                let x: Int <- 5 in
                let y: Int <- 6 in
                if x < y then
                  let z: Int <- 7 in
                  let w: Int <- 8 in
                  while z < w loop
                    output_int(z);
                    z <- z + 1
                  end;
                  output_nl()
                end
              end;
            else
              output_int(x);
              x <- x + 1
            end
          end;
          output_nl()
        end;
        output_nl()
      end
    end;
    0
end Main
```

Explanation:

This COOL code consists of a main class named "Main" that contains a single method named "main."

The "main" method initializes two integer variables, "i" and "j," with the values 0 and 10, respectively.

It then uses an "if" statement to check if "i" is less than "j." If this condition is true, it executes the code block inside the "if" statement.

Inside the "if" block, it initializes two more integer variables, "a" and "b," with the values 1 and 2, respectively.

It then enters a "while" loop that continues as long as "a" is less than "b."

Inside the "while" loop, it initializes two more integer variables, "c" and "d," with the values 3 and 4, respectively.

It then enters another "while" loop that continues as long as "c" is less than "d."

Inside the second "while" loop, it initializes two more integer variables, "x" and "y," with the values 5 and 6, respectively.

It then enters a third "while" loop that continues as long as "x" is less than "y."

Inside the third "while" loop, it initializes two more integer variables, "z" and "w," with the values 7 and 8, respectively.

It then enters a fourth "while" loop that continues as long as "z" is less than "w."

Inside the fourth "while" loop, it outputs the value of "z" to the console using the "output_int" function.

It then increments the value of "z" by 1.

It then exits the fourth "while" loop.

It then enters another "if" statement to check if "x" is less than "y." If this condition is true, it executes the code block inside the "if" statement.

Inside the "if" block, it initializes two more integer variables, "z" and "w," with the values 7 and 8, respectively.

It then enters another "while" loop that continues as long as "z" is less than "w."

Inside the fifth "while" loop, it again outputs the value of "z" to the console using the "output_int" function.

It then increments the value of "z" by 1.

It then exits the fifth "while" loop.

It then exits the second "if" statement.

It then outputs a newline character to the console using the "output_nl" function.

It then exits the first "while" loop.

It then outputs a newline character to the console using the "output_nl" function.

It then exits the second "while" loop.

Finally, it returns 0 from the "main" method, which is the standard way to indicate successful termination of a program in COOL.

Overall, this COOL code creates a nested loop structure with multiple "if" statements and uses the "output_int" and "output_nl" functions to output values and newline characters to the console, resulting in a complex and differentiated code.