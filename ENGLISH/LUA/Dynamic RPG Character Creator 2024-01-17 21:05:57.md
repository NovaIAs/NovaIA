```lua
-- Script Name: "Dynamic RPG Character Creator"

-- Importing necessary libraries
local string = require("string")
local math = require("math")
local table = require("table")
local random = require("random")

-- Creating the main function
function createCharacter()

    -- 1. Character Class Selection
    print("Welcome to the Dynamic RPG Character Creator!")
    print("Choose your character class:")
    print("1. Warrior: Strong and skilled in melee combat.")
    print("2. Mage: Powerful magic user with a variety of spells.")
    print("3. Rogue: Agile and cunning with stealth and trickery.")

    -- Getting user input for class selection
    local classChoice = io.read()

    -- 2. Character Name Input
    print("Enter a name for your character:")

    -- Getting user input for character name
    local characterName = io.read()

    -- 3. Character Attributes Generation
    -- Generating random attributes based on class choice
    local strength = math.random(1, 10)
    local dexterity = math.random(1, 10)
    local intelligence = math.random(1, 10)
    local vitality = math.random(1, 10)

    -- Adjusting attributes based on class choice
    if classChoice == 1 then -- Warrior
        strength = strength + 2
        dexterity = dexterity - 1
    elseif classChoice == 2 then -- Mage
        intelligence = intelligence + 2
        vitality = vitality - 1
    elseif classChoice == 3 then -- Rogue
        dexterity = dexterity + 2
        strength = strength - 1
    end

    -- 4. Character Equipment Generation
    -- Creating a table of potential equipment
    local equipment = {
        ["Weapon"] = {"Sword", "Dagger", "Staff", "Bow"},
        ["Armor"] = {"Plate", "Leather", "Cloth", "Chain"},
        ["Accessory"] = {"Amulet", "Ring", "Cloak", "Belt"}
    }

    -- Randomly generating equipment based on class choice
    local weapon = equipment.Weapon[math.random(#equipment.Weapon)]
    local armor = equipment.Armor[math.random(#equipment.Armor)]
    local accessory = equipment.Accessory[math.random(#equipment.Accessory)]

    -- 5. Character Summary Display
    -- Displaying the character's information
    print("Character Summary:")
    print("Name:", characterName)
    print("Class:", classChoice == 1 and "Warrior" or classChoice == 2 and "Mage" or "Rogue")
    print("Attributes:")
    print("Strength:", strength)
    print("Dexterity:", dexterity)
    print("Intelligence:", intelligence)
    print("Vitality:", vitality)
    print("Equipment:")
    print("Weapon:", weapon)
    print("Armor:", armor)
    print("Accessory:", accessory)

end

-- Running the createCharacter function
createCharacter()
```

**Explanation:**

1. **Character Class Selection:** The user is prompted to choose a character class from Warrior, Mage, or Rogue. The class choice affects the character's attributes and equipment.

2. **Character Name Input:** The user is prompted to enter a name for their character.

3. **Character Attributes Generation:** Random attributes (strength, dexterity, intelligence, and vitality) are generated based on the chosen class. The attributes are adjusted to reflect the class's strengths and weaknesses.

4. **Character Equipment Generation:** A table of potential equipment is created. Equipment (weapon, armor, and accessory) is randomly selected based on the chosen class.

5. **Character Summary Display:** The character's information (name, class, attributes, and equipment) is displayed in a summary format.

This code generates a unique character with randomly generated attributes and equipment based on the chosen class. The user can input a name for their character, and the code displays the character's summary.