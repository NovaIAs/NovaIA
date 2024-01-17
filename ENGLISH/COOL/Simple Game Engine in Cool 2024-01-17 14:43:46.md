```cool
class World {
    inherits Object;
    name: String;
    actorList: List[Actor];

    constructor(name: String, actorList: List[Actor]) {
        self.name := name;
        self.actorList := actorList;
    }

    -- Get the number of actors in the world.
    getNumActors(): Int {
        return self.actorList.size();
    }

    -- Get the actor at the given index.
    getActor(index: Int): Actor {
        return self.actorList.get(index);
    }

    -- Run the simulation for one tick.
    tick(): Void {
        for actor in self.actorList do
            actor.act();
        end for;
    }
}

class Actor {
    inherits Object;
    world: World;
    position: Point;
    facing: Direction;

    constructor(world: World, position: Point, facing: Direction) {
        self.world := world;
        self.position := position;
        self.facing := facing;
    }

    -- Get the world the actor is in.
    getWorld(): World {
        return self.world;
    }

    -- Get the actor's position.
    getPosition(): Point {
        return self.position;
    }

    -- Get the actor's facing direction.
    getFacing(): Direction {
        return self.facing;
    }

    -- Set the actor's facing direction.
    setFacing(facing: Direction): Void {
        self.facing := facing;
    }

    -- Act for one tick.
    act(): Void {
        -- Move forward.
        self.position := self.position + self.facing.toVector();

        -- Check if we hit a wall.
        if self.position.x < 0 or self.position.x > self.world.width - 1 or
            self.position.y < 0 or self.position.y > self.world.height - 1 then
            -- Bounce off the wall.
            self.facing := self.facing.opposite();
        end if;

        -- Check if we hit another actor.
        for other in self.world.actorList do
            if self.position == other.position and self != other then
                -- Collision!
                self.handleCollision(other);
            end if;
        end for;
    }

    -- Handle a collision with another actor.
    handleCollision(other: Actor): Void {
        -- Bounce off each other.
        self.facing := self.facing.opposite();
        other.facing := other.facing.opposite();
    }
}

class Point {
    inherits Object;
    x: Int;
    y: Int;

    constructor(x: Int, y: Int) {
        self.x := x;
        self.y := y;
    }

    -- Add a vector to the point.
    add(vector: Vector): Point {
        return Point.new(self.x + vector.x, self.y + vector.y);
    }
}

class Vector {
    inherits Object;
    x: Int;
    y: Int;

    constructor(x: Int, y: Int) {
        self.x := x;
        self.y := y;
    }

    -- Get the opposite vector.
    opposite(): Vector {
        return Vector.new(-self.x, -self.y);
    }
}

class Direction {
    inherits Object;
    UP: Direction;
    DOWN: Direction;
    LEFT: Direction;
    RIGHT: Direction;

    constructor() {
        self.UP := Direction.new("UP");
        self.DOWN := Direction.new("DOWN");
        self.LEFT := Direction.new("LEFT");
        self.RIGHT := Direction.new("RIGHT");
    }

    -- Get the opposite direction.
    opposite(): Direction {
        switch self do
            case self.UP:
                return Direction.DOWN;
            case self.DOWN:
                return Direction.UP;
            case self.LEFT:
                return Direction.RIGHT;