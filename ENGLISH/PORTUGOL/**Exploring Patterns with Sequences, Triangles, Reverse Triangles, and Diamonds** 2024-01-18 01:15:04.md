```portugal
program curious_code;

procedure display_sequence(sequence_length: integer);
begin
    for i := 1 to sequence_length do
    begin
        write_integer(i);
        write_string(" ");
    end;
    write_new_line;
end display_sequence;

procedure display_triangle(triangle_height: integer);
begin
    for i := 1 to triangle_height do
    begin
        for j := 1 to i do
            write_string("* ");
        write_new_line;
    end;
end display_triangle;

procedure display_reverse_triangle(triangle_height: integer);
begin
    for i := triangle_height downto 1 do
    begin
        for j := 1 to i do
            write_string("* ");
        write_new_line;
    end;
end display_reverse_triangle;

procedure display_diamond(diamond_height: integer);
begin
    for i := 1 to diamond_height do
    begin
        for j := 1 to diamond_height - i do
            write_string(" ");
        for j := 1 to 2 * i - 1 do
            write_string("*");
        write_new_line;
    end;
    for i := diamond_height - 1 downto 1 do
    begin
        for j := 1 to diamond_height - i do
            write_string(" ");
        for j := 1 to 2 * i - 1 do
            write_string("*");
        write_new_line;
    end;
end display_diamond;

begin
    write_string("Displaying Sequences:");
    write_new_line;
    display_sequence(10);

    write_string("Displaying Triangles:");
    write_new_line;
    display_triangle(5);

    write_string("Displaying Reverse Triangles:");
    write_new_line;
    display_reverse_triangle(5);

    write_string("Displaying Diamonds:");
    write_new_line;
    display_diamond(5);
end curious_code.
```

Explanation:

1. **Displaying Sequences:**
   - The `display_sequence` procedure takes an integer `sequence_length` as input.
   - Inside a `for` loop, it prints numbers from 1 to `sequence_length` with spaces in between.

2. **Displaying Triangles:**
   - The `display_triangle` procedure takes an integer `triangle_height` as input.
   - It uses nested `for` loops to print a right triangle of asterisks, starting from 1 asterisk at the top and increasing by 1 asterisk per row.

3. **Displaying Reverse Triangles:**
   - The `display_reverse_triangle` procedure takes an integer `triangle_height` as input.
   - It uses nested `for` loops to print a reverse (upside-down) right triangle of asterisks, starting from `triangle_height` asterisks at the top and decreasing by 1 asterisk per row.

4. **Displaying Diamonds:**
   - The `display_diamond` procedure takes an integer `diamond_height` as input.
   - It uses nested `for` loops to print a diamond shape using asterisks.
   - The diamond is created by first printing upper triangles and then lower triangles.

The code is written in a modular way using procedures to improve readability and code reusability.