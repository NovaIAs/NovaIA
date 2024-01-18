```lua
-- Define a custom print function to add color and style to output
local function prettyPrint(text, color, style)
    local colorCode, styleCode = getColorCode(color), getStyleCode(style)
    io.write("\033[" .. colorCode .. ";" .. styleCode .. "m" .. text .. "\033[0m")
end

-- Define a function to get the ANSI color code for a given color name
local function getColorCode(color)
    local colorCodes = {
        black = 0,
        red = 1,
        green = 2,
        yellow = 3,
        blue = 4,
        magenta = 5,
        cyan = 6,
        white = 7
    }
    return colorCodes[color] or 0
end

-- Define a function to get the ANSI style code for a given style name
local function getStyleCode(style)
    local styleCodes = {
        bold = 1,
        underline = 4,
        blink = 5,
        reverse = 7
    }
    return styleCodes[style] or 0
end

-- Create a table of random numbers between 1 and 100
local randomNumbers = {}
for i = 1, 100 do
    randomNumbers[i] = math.random(1, 100)
end

-- Print the random numbers in a colorful table format
for i = 1, 100 do
    local number = randomNumbers[i]
    local color, style = getColorAndStyle(number)
    prettyPrint(number, color, style)
    if i % 10 == 0 then
        print()
    else
        print("\t")
    end
end

-- Define a function to get the color and style for a given number
local function getColorAndStyle(number)
    local color, style
    if number < 25 then
        color, style = "red", "bold"
    elseif number < 50 then
        color, style = "yellow", "underline"
    elseif number < 75 then
        color, style = "green", "blink"
    else
        color, style = "blue", "reverse"
    end
    return color, style
end
```
Explanation:

1. **Custom Print Function (`prettyPrint`)**: This function adds color and style to the output using ANSI escape codes. It takes three arguments: `text` (the text to print), `color` (the desired color), and `style` (the desired style).
2. **Color and Style Codes (`getColorCode` and `getStyleCode`)**: These helper functions convert the given color and style names into their corresponding ANSI codes.
3. **Random Numbers Table**: A table `randomNumbers` is created, containing 100 random numbers between 1 and 100.
4. **Colorful Table Format**: A loop iterates through the random numbers, printing each number in a specific color and style based on its value. The `prettyPrint` function is used to add color and style to the output.
5. **Color and Style Selection (`getColorAndStyle`)**: This function determines the color and style for a given number. Different color and style combinations are assigned based on the number's range (e.g., numbers less than 25 are printed in red and bold).
6. **Printing the Results**: The numbers are printed in a colorful table format, with 10 numbers per row.

This code uses a combination of custom functions, loops, and conditional statements to create a visually appealing and dynamic output.