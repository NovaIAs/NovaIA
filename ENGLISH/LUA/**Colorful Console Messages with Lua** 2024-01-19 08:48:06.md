```lua
-- Create a custom table of colors
local colors = {
    ["red"] = {255, 0, 0},
    ["green"] = {0, 255, 0},
    ["blue"] = {0, 0, 255},
    ["yellow"] = {255, 255, 0},
    ["pink"] = {255, 0, 255},
    ["purple"] = {128, 0, 128},
    ["orange"] = {255, 128, 0}
}

-- Define a function to print a colored message to the console
function print_colored(text, color)
    -- Check if the color is valid
    if not colors[color] then
        error("Invalid color: " .. color)
    end

    -- Get the RGB values of the color
    local r, g, b = colors[color]

    -- Set the console color
    io.write("\033[38;2;" .. r .. ";" .. g .. ";" .. b .. "m")

    -- Print the text
    print(text)

    -- Reset the console color
    io.write("\033[0m")
end

-- Print a greeting message in red
print_colored("Hello, world!", "red")

-- Print a warning message in yellow
print_colored("Warning: This is a warning!", "yellow")

-- Print a success message in green
print_colored("Success: This is a success message!", "green")

-- Iterate over the colors table and print each color's name and RGB values
for color, rgb in pairs(colors) do
    print(color .. ": " .. rgb[1] .. ", " .. rgb[2] .. ", " .. rgb[3])
end
```
Explanation:

1. **Custom Color Table (`colors`):**
   - We define a custom table called `colors` that maps color names (e.g., "red") to their corresponding RGB values (e.g., {255, 0, 0}).

2. **`print_colored` Function:**
   - This function takes two arguments: `text` (the message to print) and `color` (the name of the color to use).
   - It first checks if the specified color is valid by looking it up in the `colors` table. If the color is invalid, it throws an error.
   - It then gets the RGB values of the specified color from the `colors` table.
   - It sets the console color using ANSI escape codes to the specified RGB values.
   - It prints the text to the console.
   - Finally, it resets the console color to the default.

3. **Printing Colored Messages:**
   - We call the `print_colored` function with different arguments to print text in different colors to the console.
   - For example, we print a greeting message in red, a warning message in yellow, and a success message in green.

4. **Iterating Over the Colors Table:**
   - We use a `for` loop to iterate over the `colors` table.
   - For each color, we print its name and RGB values to the console.

This code demonstrates how to create a custom color table, define a function to print colored messages, and use it to print text in different colors to the console. It also shows how to iterate over a table and access its values.