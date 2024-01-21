```d
import std.algorithm, std.stdio, std.range;

// Simulación básica de una colonia de hormigas.

class Position {
    immutable int x, y;
    const op bitwiseShiftLeft(int n) pure @safe {
        Position(x << n, y << n);
    }
    const op bitwiseShiftRight(int n) pure @safe {
        Position(x >> n, y >> n);
    }
    const op +~ (Position rhs) pure @safe {
        Position(x + rhs.x, y + rhs.y);
    }
    @property type Coord { int x, y; }
    const op Coord +~(Position p) pure @safe {
        Coord(x + p.x, y + p.y);
    }
    @property immutable int distanceFromOrigin pure @safe {
        std.sqrt(x * x + y * y);
    }
}

class Ant {
    immutable int antennaLength;
    mutable Position position;
    const this() pure @safe {
        position = Position(0, 0);
        antennaLength = 10;
    }
    const this(Position p) pure @safe {
        position = p;
        antennaLength = 10;
    }
    const this(int al) pure @safe {
        position = Position(0, 0);
        antennaLength = al;
    }
    const this(int al, Position p) pure @safe {
        position = p;
        antennaLength = al;
    }
    const @property Position getPos() pure @safe {
        position;
    }
    enum Dir { up, down, left, right }
    const @property Dir getDir() pure @safe {
        if (position == Position(0, 0)) { up; }
        if (position.x == 0) {
            if (position.y > 0) { down; }
            else { up; }
        }
        if (position.y == 0) {
            if (position.x > 0) { right; }
            else { left; }
        }
        if (position.x > 0) {
            if (position.y > 0) { up; }
            else { down; }
        }
        if (position.x < 0) {
            if (position.y > 0) { up; }
            else { down; }
        }
        left; // para evitar el warning de "unused enumeration member"
    }
    const int getUnsigned8BitDir() pure @safe {
        switch (getDir()) {
            case up: 0;
            case down: 1;
            case left: 2;
            case right: 3;
        }
    }
    const @property Coord getCoord() pure @safe {
        Coord(position.x, position.y);
    }
    @property immutable Position getPos8Bit() pure @safe {
        switch (getUnsigned8BitDir()) {
            case 0: position;
            case 1: position - (0, 1);
            case 2: position - (1, 0);
            case 3: position + (1, 0);
        }
    }
    const @property Position getAntennaPos() pure @safe {
        antennaLength.to!Position +~ getPos8Bit();
    }
    @property @nogc immutable Coord getAntennaCoord() pure @safe {
        getCoord() +~ getPos8Bit();
    }
    const void setPos(Position p) pure @trusted {
        position = p;
    }
    const void setPos(int x, int y) pure @trusted {
        position.x = x;
        position.y = y;
    }
    const void turn(Dir d) pure @safe {
        switch (getDir()) {
            case up:
                switch (d) {
                    case up: break;
                    case down: setPos(setPos8Bit() +~(0, 1)); break;
                    case left: setPos(setPos8Bit() -~(1, 0)); break;
                    case right: setPos(setPos8Bit() +~(1, 0)); break;
                }
                break;
            case down:
                switch (d) {
                    case up: setPos(setPos8Bit() -~(0, 1)); break;
                    case down: break;
                    case left: setPos(setPos8Bit() +~(1, 0)); break;
                    case right: setPos(setPos8Bit() -~(1, 0)); break;
                }
                break;
            case left:
                switch (d) {
                    case up: setPos(setPos8Bit() +~(0, 1)); break;
                    case down: setPos(setPos8Bit() -~(0, 1)); break;
                    case left: break;
                    case right: setPos(setPos8Bit() -~(0, 1)); break;
                }
                break;
            case right:
                switch (d) {
                    case up: setPos(setPos8Bit() +~(0, 1)); break;
                    case down: setPos(setPos8Bit() -~(0, 1)); break;
                    case left: setPos(setPos8Bit() +~(0, 1)); break;
                    case right: break;
                }
                break;
        }
    }
}

class Colony {
    immutable size_t population;
    immutable size_t numObstacles;
    immutable size_t bounds;
    mutable Ant[] ants;
    mutable int[,] obstacles;
    const this() pure @safe {
        numObstacles = 0;
        population = 0;
        bounds = 10;
        reset();
    }
    const this(int pop, int obst, int b) pure @safe {
        population = pop;
        numObstacles = obst;
        bounds = b;
        reset();
    }
    const @property immutable size_t getPopulation() pure @safe {
        population;
    }
    const @property immutable size_t getNumObstacles() pure @safe {
        numObstacles;
    }
    const @property immutable size_t getBounds() pure @safe {
        bounds;
    }
    pure void reset() @safe {
        resize!obstacles(bounds, bounds);
        resize!ants(population);
        for (size_t i = 0; i < population; ++i) {
            ants[i].setPos(Position((bounds >> 1), (bounds >> 1)));
        }
        for (size_t i = 0; i < numObstacles; ++i) {
            int x, y;
            do {
                x = std.random(int, bounds - 2) + 1;
                y = std.random(int, bounds - 2) + 1;
            } while (obstacles[x][y] != 0);
            obstacles[x][y] = 1;
        }
    }
    pure Position getClosestObstacle(Position p) @safe {
        Position result;
        int bestDistance = bounds * bounds;
        for (size_t i = 0; i < bounds; ++i) {
            for (size_t j = 0; j < bounds; ++j) {
                if (obstacles[i][j] == 1) {
                    Position pos(i, j);
                    int distance = pos.distanceFromOrigin;
                    if (distance < bestDistance) {
                        bestDistance = distance;
                        result = pos;
                    }
                }
            }
        }
        result;
    }
    const void sim(size_t numSteps) pure @safe {
        for (size_t step = 0; step < numSteps; ++step) {
            auto obstaclesList = obstacles.to!flat!Array.shuffle();
            obstaclesList.map!(o) {
                if (o[0] == 0 && o[1] == 0) {
                    return;
                }
                switch (ants[0].getDir()) {
                    case up:
                        if (obstaclesList[0][0] == 0) {
                            ants[0].setPos(-bounds, 0);
                        }
                        if (obstaclesList[0][1] == 0) {
                            ants[0].setPos(0, -bounds);
                        }
                        Position target = getClosestObstacle(ants[0].getAntennaPos());
                        int dX = target.x - ants[0].getPos().x;
                        int dY = target.y - ants[0].getPos().y;
                        if (dY > 0) {
                            ants[0].turn(Ant.Dir.right);
                        } else if (dY < 0) {
                            ants[0].turn(Ant.Dir.left);
                        } else {
                            if (dX > 0) {
                                ants[0].turn(Ant.Dir.up);
                            } else if (dX < 0) {
                                ants[0].turn(Ant.Dir.down);
                            }
                        }
                        break;
                    case down:
                        if (obstaclesList[0][0] == 0) {
                            ants[0].setPos(bounds, 0);
                        }
                        if (obstaclesList[0][1] == 0) {
                            ants[0].setPos(0, bounds);
                        }
                        target = getClosestObstacle(ants[0].getAntennaPos());
                        dX = target.x - ants[0].getPos().x;
                        dY = target.y - ants[0].getPos().y;
                        if (dY < 0) {
                            ants[0].turn(Ant.Dir.right);
                        } else if (dY > 0) {
                            ants[0].turn(Ant.Dir.left);
                        } else {
                            if (dX > 0) {
                                ants[0].turn(Ant.Dir.up);
                            } else if (dX < 0) {
                                ants[0].turn(Ant.Dir.down);
                            }
                        }
                        break;
                    case left:
                        if (obstaclesList[0][0] == 0) {
                            ants[0].setPos(0, -bounds);
                        }
                        if (obstaclesList[0][1] == 0) {
                            ants[0].setPos(bounds, 0);
                        }
                        target = getClosestObstacle(ants[0].getAntennaPos());
                        dX = target.x - ants[0].getPos().x;
                        dY = target.y - ants[0].getPos().y;
                        if (dX < 0) {
                            ants[0].turn(Ant.Dir.right);
                        } else if (dX > 0) {
                            ants[0].turn(Ant.Dir.left);
                        } else {
                            if (dY > 0) {
                                ants[0].turn(Ant.Dir.up);
                            } else if (dY < 0) {
                                ants[0].turn(Ant.Dir.down);
                            }
                        }
                        break;
                    case right:
                        if (obstaclesList[0][0] == 0) {
                            ants[0].setPos(0, bounds);
                        }
                        if (obstaclesList[0][1] == 0) {
                            ants[0].setPos(-bounds, 0);
                        }
                        target = getClosestObstacle(ants[0].getAntennaPos());
                        dX = target.x - ants[0].getPos().x;
                        dY = target.y - ants[0].getPos().y;
                        if (dX > 0) {
                            ants[0].turn(Ant.Dir.right);
                        } else if (dX < 0) {
                            ants[0].turn(Ant.Dir.left);
                        } else {
                            if (dY > 0) {
                                ants[0].turn(Ant.Dir.up);
                            } else if (dY < 0) {
                                ants[0].turn(Ant.Dir.down);