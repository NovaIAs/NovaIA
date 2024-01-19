```lua
-- initial setup
local tickCount = 0

local function tick()
    print("tick " .. tickCount)
    tickCount = tickCount + 1
end

local coroutineList = {}

coroutine.create(function()
    while true do
        coroutine.yield()
        tick()
    end
end)

-- game loop
while true do
    print("game loop " .. tickCount)
    coroutine.resume(coroutineList)
end
```

Explanation:

1. `tickCount` variable is used to keep track of the number of ticks that have occurred.

2. The `tick` function prints the current tick count and increments it by 1.

3. A list called `coroutineList` is created to store coroutines.

4. A coroutine is created using `coroutine.create`. This coroutine will run indefinitely, yielding every time it is called. The `tick` function is called inside the coroutine to print the tick count.

5. The main game loop runs indefinitely, printing the tick count and resuming all the coroutines in the `coroutineList`.

This code effectively creates a simple game loop that runs concurrently with the `tick` coroutine. The `tick` coroutine prints the tick count every time it is called, while the game loop prints the tick count and resumes the `tick` coroutine every tick.