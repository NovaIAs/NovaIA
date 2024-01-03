Certainly! Here's a large and differentiated Objective-C code that generates a random maze using a depth-first search algorithm:

```objective-c
#import <Foundation/Foundation.h>

#define WIDTH 21
#define HEIGHT 21

@interface MazeGenerator : NSObject

@property (nonatomic, strong) NSMutableArray *maze;

- (void)generateMaze;

@end

@implementation MazeGenerator

- (instancetype)init {
    self = [super init];
    if (self) {
        _maze = [NSMutableArray array];
        for (int i = 0; i < HEIGHT; i++) {
            NSMutableArray *row = [NSMutableArray array];
            for (int j = 0; j < WIDTH; j++) {
                [row addObject:@"#"];
            }
            [_maze addObject:row];
        }
    }
    return self;
}

- (void)generateMaze {
    [self visitCellAtRow:1 column:1];
}

- (void)visitCellAtRow:(NSInteger)row column:(NSInteger)column {
    NSMutableArray *neighbors = [NSMutableArray array];
    
    // Check top neighbor
    if (row - 2 >= 0 && [self.maze[row - 2][column] isEqualToString:@"#"] && [self.maze[row - 1][column] isEqualToString:@"#"]) {
        [neighbors addObject:@[@(row - 2), @(column)]];
    }
    
    // Check right neighbor
    if (column + 2 < WIDTH && [self.maze[row][column + 2] isEqualToString:@"#"] && [self.maze[row][column + 1] isEqualToString:@"#"]) {
        [neighbors addObject:@[@(row), @(column + 2)]];
    }
    
    // Check bottom neighbor
    if (row + 2 < HEIGHT && [self.maze[row + 2][column] isEqualToString:@"#"] && [self.maze[row + 1][column] isEqualToString:@"#"]) {
        [neighbors addObject:@[@(row + 2), @(column)]];
    }
    
    // Check left neighbor
    if (column - 2 >= 0 && [self.maze[row][column - 2] isEqualToString:@"#"] && [self.maze[row][column - 1] isEqualToString:@"#"]) {
        [neighbors addObject:@[@(row), @(column - 2)]];
    }
    
    if (neighbors.count > 0) {
        int randomIndex = arc4random_uniform((int)neighbors.count);
        NSArray *randomNeighbor = neighbors[randomIndex];
        NSInteger neighborRow = [randomNeighbor[0] integerValue];
        NSInteger neighborColumn = [randomNeighbor[1] integerValue];
        
        // Remove the wall between the current cell and the randomly chosen neighbor
        if (neighborRow < row) {
            self.maze[row - 1][column] = @" ";
        } else if (neighborRow > row) {
            self.maze[row + 1][column] = @" ";
        } else if (neighborColumn < column) {
            self.maze[row][column - 1] = @" ";
        } else if (neighborColumn > column) {
            self.maze[row][column + 1] = @" ";
        }
        
        // Mark the randomly chosen neighbor as visited
        self.maze[neighborRow][neighborColumn] = @" ";
        
        // Recursively visit the neighbor
        [self visitCellAtRow:neighborRow column:neighborColumn];
        
        // Recursively visit other unvisited neighbors
        for (NSArray *cell in neighbors) {
            NSInteger cellRow = [cell[0] integerValue];
            NSInteger cellColumn = [cell[1] integerValue];
            [self visitCellAtRow:cellRow column:cellColumn];
        }
    }
}

- (void)printMaze {
    for (NSArray *row in self.maze) {
        for (NSString *cell in row) {
            printf("%s", [cell UTF8String]);
        }
        printf("\n");
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MazeGenerator *mazeGenerator = [[MazeGenerator alloc] init];
        [mazeGenerator generateMaze];
        [mazeGenerator printMaze];
    }
    return 0;
}
```

This Objective-C code defines a class MazeGenerator that generates and prints a random maze using a depth-first search algorithm. The maze is represented as a 2-dimensional array, where "#" represents walls and " " represents open paths.

The `MazeGenerator` class contains methods for initializing the maze, generating the maze using depth-first search, visiting cells, and printing the maze. The `generateMaze` method starts the generation process by calling `visitCellAtRow:column:` with initial cell coordinates. The `visitCellAtRow:column:` method recursively visits neighboring cells, removes walls between visited cells, and continues to visit unvisited neighbors until there are no more neighbors.

The `printMaze` method simply prints the maze to the console.

The `main` function initializes an instance of `MazeGenerator`, generates the maze, and prints it.

Please note that this code is a simplified implementation for educational purposes and may not be optimized for performance or error handling. Feel free to modify it according to your specific needs.