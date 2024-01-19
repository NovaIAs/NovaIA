```cool
class Main is
  main() : Int is
    let
      local x : Int <- 10;
      if x < 11 then
        if (x+1)*2 = 20 then
          x+1
        else
          x*2
        fi
      else
        x*3
      fi
    in
      x
    end
  end Main
```

This code shows a more complex example of the COOL language. It creates a local variable `x` and assigns it the value of 10.
It then checks if `x` is less than 11, and if it is, it checks if `(x+1)*2` is equal to 20. If it is, it returns `x+1`, otherwise it returns `x*2`.
If `x` is not less than 11, it returns `x*3`.
The code also makes use of the `let` keyword to create a local variable, and the `in` keyword to define the scope of the variable.