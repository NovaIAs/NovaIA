```tcl
set data [[list 1 2] [list 3 4] [list 5 6]]
foreach row $data {
    foreach col $row {
        puts "Element: $col"
    }
    puts ""
}
```

Explanation:

1. `set data [[list 1 2] [list 3 4] [list 5 6]]`: This line creates a nested list called `data`. The list contains three sublists, each containing two elements.

2. `foreach row $data { ... }`: This line starts a foreach loop that iterates over each sublist in the `data` list. The variable `row` will be assigned each sublist in turn.

3. `foreach col $row { ... }`: This line starts a nested foreach loop that iterates over each element in the current `row`. The variable `col` will be assigned each element in turn.

4. `puts "Element: $col"`: This line uses the `puts` command to print the value of the current `col` variable to the console. The `$col` variable contains the current element in the current `row`.

5. `puts ""`: This line prints an empty line to the console to separate the output from each row.

When this code is executed, it will print the following output:

```
Element: 1
Element: 2

Element: 3
Element: 4

Element: 5
Element: 6
```