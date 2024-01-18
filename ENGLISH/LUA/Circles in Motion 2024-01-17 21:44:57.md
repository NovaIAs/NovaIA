```lua
-- Create a new Lua table to store our data
local data = {}

-- Define a function to generate a random number between 0 and 1
function random(max)
  return math.random(1, max)
end

-- Define a function to generate a random color
function randomColor()
  -- Generate a random number for each of the three color channels (red, green, blue)
  local red = random(255)
  local green = random(255)
  local blue = random(255)

  -- Return the color as a string in the format "rgb(red, green, blue)"
  return string.format("rgb(%d, %d, %d)", red, green, blue)
end

-- Define a function to create a new circle object
function createCircle(x, y, radius, color)
  -- Create a new table to store the circle's data
  local circle = {}

  -- Set the circle's position and radius
  circle.x = x
  circle.y = y
  circle.radius = radius

  -- Set the circle's color
  circle.color = color

  -- Return the circle object
  return circle
end

-- Define a function to draw a circle to the screen
function drawCircle(circle)
  -- Set the drawing color to the circle's color
  love.graphics.setColor(circle.color)

  -- Draw a circle at the circle's position with the circle's radius
  love.graphics.circle("fill", circle.x, circle.y, circle.radius)
end

-- Define a function to update the game state
function update(dt)
  -- Update the position of each circle
  for i = 1, #data do
    data[i].x = data[i].x + random(-5, 5)
    data[i].y = data[i].y + random(-5, 5)
  end
end

-- Define a function to draw the game state
function draw()
  -- Clear the screen
  love.graphics.clear()

  -- Draw each circle
  for i = 1, #data do
    drawCircle(data[i])
  end
end

-- Main game loop
function love.loop()
  -- Update the game state
  update(love.timer.getDelta())

  -- Draw the game state
  draw()
end

-- Function to handle key presses
function love.keypressed(key)
  -- If the user presses the escape key, quit the game
  if key == "escape" then
    love.event.quit()
  end
end

-- Function to handle window resize events
function love.resize(w, h)
  -- Set the camera to match the new window size
  love.graphics.setCamera(0, 0, w, h)
end

-- Start the game
love.run()
```

This is a Lua code for a simple game where circles move around the screen. The game uses the "love" library, which is a popular Lua library for creating 2D games.

The code first defines a function to generate a random number, a function to generate a random color, and a function to create a new circle object.

Then, it defines a function to draw a circle to the screen, a function to update the game state, and a function to draw the game state.

The main game loop is defined in the `love.loop()` function, which is called by the `love` library to run the game. The game loop updates the game state and draws the game state each frame.

The `love.keypressed()` function is called when a key is pressed. If the escape key is pressed, the game quits.

The `love.resize()` function is called when the window is resized. It sets the camera to match the new window size.

Finally, the `love.run()` function is called to start the game.