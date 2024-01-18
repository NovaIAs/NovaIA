```d
import std.stdio, std.algorithm;

template Region {
    immutable midpoint;

    immutable minX, minY, maxX, maxY;

    this(Point pm, Point p1, Point p2) pure nothrow {
        midpoint = pm;
        minY = min(p1.y, p2.y);
        minX = min(p1.x, p2.x);
        maxY = max(p1.y, p2.y);
        maxX = max(p1.x, p2.x);
    }

    this(Point pm, double dx, double dy)纯净nothrow{
        midpoint = pm;
        minX = pm.x-dx;
        maxX = pm.x+dx;
        minY = pm.y-dy;
        maxY = pm.y+dy;
    }

    immutable Center(Point _midpoint) pure nothrow {
        return new Region(_midpoint, 0, 0);
    }

    immutable Distance(Region b) pure nothrow {
        return midpoint.Distance(b.midpoint);
    }

    immutable bool Contains(Point p)纯净nothrow{
        return (minX <= p.x && maxX >= p.x && minY <= p.y && maxY >= p.y);
    }

    immutable bool Intersects(Region b) pure nothrow {
        // Find the four quadrants relative to B.
        bool quad00 = (minX < B.minX) && (maxX < B.minX);
        bool quad01 = (minX < B.maxX) && (maxX < B.maxX);
        bool quad10 = (minX > B.minX) && (maxX > B.minX);
        bool quad11 = (minX > B.maxX) && (maxX > B.maxX);

        bool quad0 = (quad00 || quad01);
        bool quad1 = (quad10 || quad11);

        // The regions intersect if any corners of B are in region A,
        // or any corners of A are in region B, or both regions
        // share a quadrant.
        return (quad0 && B.Contains(Point(minX, minY))) ||
               (quad0 && B.Contains(Point(minX, maxY))) ||
               (quad1 && B.Contains(Point(maxX, minY))) ||
               (quad1 && B.Contains(Point(maxX, maxY))) ||
               quad0 || quad1;
    }

    immutable Rects() pure nothrow {
        in outRects = Rects();
        outRects ~= [Center(Point((minX + maxX) / 2.0, (minY + maxY) / 2.0))];
        if (maxX - minX > maxY - minY) {
            outRects ~= [Center(Point(maxX - (maxX - minX) / 3.0,
                                      (minY + maxY) / 2.0)),
                         Center(Point(minX + (maxX - minX) / 3.0,
                                      (minY + maxY) / 2.0))];
        } else {
            outRects ~= [Center(Point((minX + maxX) / 2.0,
                                      maxY - (maxY - minY) / 3.0)),
                         Center(Point((minX + maxX) / 2.0,
                                      minY + (maxY - minY) / 3.0))];
        }
        return outRects;
    }

    immutable Rect() pure nothrow {
        return new Region(midpoint, (maxX - minX) / 2.0, (maxY - minY) / 2.0);
    }

    immutable Region() pure nothrow {
        return new Region(midpoint, 0, 0);
    }

    immutable static Region FromRects(Rects subRects) pure nothrow {
        immutable initialRegion = Region.FromRect(subRects[0]);
        immutable r = fold!right!Rects(subRects, initialRegion,
                                   (Region a, Region b)纯净nothrow{
            return Region.Union(a, b);
        });
        return r;
    }

    immutable static Region FromRect(Rect subRect) pure nothrow {
        return new Region(subRect.midpoint, subRect.dx, subRect.dy);
    }

    immutable Region Centered(Point newCenter) pure nothrow {
        return new Region(newCenter, (maxX - minX) / 2.0, (maxY - minY) / 2.0);
    }

    immutable Region Moved(Point delta)纯净nothrow {
        return new Region(midpoint + delta, (maxX - minX) / 2.0, (maxY - minY) / 2.0);
    }

    immutable Region Union(Region b)纯净nothrow {
        immutable minX = min(this.minX, b.minX);
        immutable minY = min(this.minY, b.minY);
        immutable maxX = max(this.maxX, b.maxX);
        immutable maxY = max(this.maxY, b.maxY);
        immutable Centered mid = Centered(Point((minX + maxX) / 2.0,
                                                  (minY + maxY) / 2.0));
        return mid.Centered(Point(abs(minX + maxX - (maxX - minX))/2.0,
                                 abs(minY + maxY - (maxY - minY))/2.0));
    }

    immutable static Region Union(Rects subRects)纯净nothrow {
        immutable minX = fold!right!Rects(subRects, 1000000.0,
                                   (double a, Rect b)纯净nothrow{
            return min(a, b.minX);
        });
        immutable minY = fold!right!Rects(subRects, 1000000.0,
                                   (double a, Rect b)纯净nothrow{
            return min(a, b.minY);
        });
        immutable maxX = fold!left!Rects(subRects, -1000000.0,
                                   (double a, Rect b)纯净nothrow{
            return max(a, b.maxX);
        });
        immutable maxY = fold!left!Rects(subRects, -1000000.0,
                                   (double a, Rect b)纯净nothrow{
            return max(a, b.maxY);
        });
        immutable Centered mid = Centered(Point((minX + maxX) / 2.0,
                                                  (minY + maxY) / 2.0));
        return mid.Centered(Point(abs(minX + maxX - (maxX - minX))/2.0,
                                 abs(minY + maxY - (maxY - minY))/2.0));
    }
}

template QTree {
    immutable Region region;
    immutable list!QTree subTrees;

    Region() pure nothrow {
        this.region = Region();
    }

    immutable void insert(Point p)纯净nothrow!throws(CoordinateError) {
        if (subTrees.length == 4) {
            insertIntoRects(subTrees, p);
        } else {
            subTrees ~= Region.Rects() !map filter!(!(p.Distance(region.midpoint) > region.Distance(subTree.region)));
            insertIntoRects(subTrees, p);
        }
    }

    immutable void insertIntoRects(list!QTree subTrees, Point p)纯净nothrow!throws(CoordinateError) {
        immutable in outSubTree = subTrees[0];
        if (p.Distance(outSubTree.region.midpoint) <= outSubTree.region.Distance(subTree.region)) {
            assert(p.Distance(outSubTree.region.midpoint) > outSubTree.region.Distance(outSubTree.region));
            subTrees[0] = Region.FromRect(outSubTree.region.Rect());
            subTrees[1] = Region.FromRect(outSubTree.region.Moved(Point(-outSubTree.region.dx,
                                                                       outSubTree.region.dy)));
            subTrees[2] = Region.FromRect(outSubTree.region.Moved(Point(outSubTree.region.dx,
                                                                       -outSubTree.region.dy)));
            subTrees[3] = Region.FromRect(outSubTree.region.Moved(Point(-outSubTree.region.dx,
                                                                       -outSubTree.region.dy)));
        }
        subTrees ~= Region.Rects() !map filter!(!(p.Distance(region.midpoint) > region.Distance(subTree.region)));
        iter!subTree;
        in outSubTree = subTree;
        if (outSubTree.region.Contains(p)) {
            outSubTree.insert(p);
        }
        subTree.insert(p);
    }

    immutable bool isContained(Point p) pure nothrow {
        if (region.Contains(p)) {
            iter!subTree;
            if (subTree.region.Contains(p)) {
                return subTree.isContained(p);
            }
        }
        return false;
    }

    immutable in void find(Point p)纯净nothrow!throws(CoordinateError) {
        if (!region.Contains(p)) {
            throw new CoordinateError("The point is not in this tree");
        }
        iter!subTree;
        if (subTree.region.Contains(p)) {
            subTree.find(p);
            return;
        }
        Point pForChild = p;
        if (pForChild.x > region.midpoint.x) {
            pForChild.x = region.midpoint.x + (region.midpoint.x - region.minX);
        } else {
            pForChild.x = region.midpoint.x - (region.maxX - region.midpoint.x);
        }
        if (pForChild.y > region.midpoint.y) {
            pForChild.y = region.midpoint.y + (region.midpoint.y - region.minY);
        } else {
            pForChild.y = region.midpoint.y - (region.maxY - region.midpoint.y);
        }
        find(pForChild);
    }

    immutable in void mark(Point p)纯净nothrow!throws(CoordinateError) {
        if (!region.Contains(p)) {
            throw new CoordinateError("The point is not in this tree");
        }
        if (subTrees.length == 4) {
            iter!subTree;
            if (subTree.region.Contains(p)) {
                subTree.mark(p);
                return;
            }
        } else {
            subTrees ~= Region.Rects();
            iter!subTree;
            if (subTree.region.Contains(p)) {
                subTree.mark(p);
                return;
            }
        }
    }

    immutable void growBy(Point p)纯净nothrow {
        immutable double oldDx = region.dx;
        immutable double oldDy = region.dy;
        immutable double halfOldDx = oldDx / 2.0;
        immutable double halfOldDy = oldDy / 2.0;
        immutable double scaledDist = p.Distance(region.midpoint);
        immutable double dx = max(halfOldDx, scaledDist);
        immutable double dy = max(halfOldDy, scaledDist);
        immutable double deltaX = dx - halfOldDx;
        immutable double deltaY = dy - halfOldDy;
        region = region.Moved(Point(deltaX, deltaY));
        region = region.Centered(Point((region.midpoint.x + p.x) / 2.0,
                                       (region.midpoint.y + p.y) / 2.0));
        iter!subTree;
        subTree.growBy(p);
    }
}


template Point {
    immutable double x;
    immutable double y;

    immutable void init(double nx, double ny) pure {
        x = nx;
        y = ny;
    }

    immutable double Distance(Point b) pure nothrow {
        return sqrt((b.x - x) ^ 2 + (b.y - y) ^ 2);
    }

    immutable static double Distance(Point a, Point b) pure nothrow {
        return sqrt((b.x - a.x) ^ 2 + (b.y - a.y) ^ 2);
    }

    immutable double DistanceSquared(Point b)纯净nothrow {
        return (b.x - x) ^ 2 + (b.y - y) ^ 2;
    }

    immutable static double DistanceSquared(Point a, Point b) pure nothrow {
        return (b.x - a.x) ^ 2 + (b.y - a.y) ^ 2;
    }

    immutable Point Moved(Point delta)纯净nothrow {
        Point result;
        result.x = x + delta.x;
        result.y = y + delta.y;
        return result;
    }

    immutable void Move(Point delta)纯净nothrow {
        x += delta.x;
        y += delta.y;
    }
}


template CoordinateError {
    immutable string msg;

    immutable void init(string nmsg) pure {
        msg = nmsg;
    }

    immutable string getMessage() pure {
        return msg;
    }
}


main() {
    int numElements = 15000;
    Point[] points = new Point[numElements];
    immutable double canvasWidth = 1000.0;
    immutable double canvasHeight = 1000.0;
    immutable double xRange = canvasWidth * 0.85;
    immutable double yRange = canvasHeight * 0.85;
    for (int i = 0; i < numElements; i++) {
        points[i] = new Point();
        points[i].init(random.float(0, xRange), random.float(0, yRange));
    }
    immutable in QTree tree = new QTree();
    iter!point;
    tree.insert(point);

    Point p = new Point();
    p.init(600.0, 10.0);
    tree.find(p);
    p.Move(Point(0.0, -2.0));
    tree.mark(p);

    p.init(615.0, 50.0);
    tree.growBy(p);

    printf("Inserted %d points in QTree", numElements);
    printf(" ");
    printf("Find took %s at distance %f", tree.isContained(p) ? "YES" : "NO", p.Distance(tree.region.midpoint));
    printf(" ");
    printf("Growth involves %d points", tree.region.dx * tree.region.dy / Point.DistanceSquared(tree.region.midpoint, Point(0, 0)));
}
```

**Explanation:**

1. **Data Structures:**
   - `Region`: Represents a rectangular region in 2D space. It has fields like `midpoint`, `dx` (width), `dy` (height), and a list of sub-regions (`subTrees`).
   - `QTree`: Represents a Quadtree data structure. It contains a `region` and a list of subtrees (`subTrees`).
   - `Point`: Represents a point in 2D space with `x` and `y` coordinates.

2. **QTree Operations:**
   - `insert(Point p)`: Inserts a point into the QTree, recursively dividing the region into quadrants if needed.
   - `isContained(Point p)`: Checks if a point is contained within the QTree's region.
   - `find(Point p)`: Finds the exact subtree containing a given point.
   - `mark(Point p)`: Marks the subtree containing a given point.
   - `growBy(Point p)`: Grows the QTree to include a given point if it's outside the current region.

3. **Point Operations:**
   - `Distance(Point b)`: Calculates the Euclidean distance between two points.
   - `DistanceSquared(Point b)`: Calculates the squared Euclidean distance between two points.
   - `Moved(Point delta)`: Returns a new point shifted by the given delta.
   - `Move(Point delta)`: Moves the current point by the given delta.

4. **Main Function:**
   - Generates an array of `numElements` random points within a specified range.
   - Inserts all points into a QTree.
   - Performs various operations on the QTree, such as finding a point, marking a point, and growing the tree.
   - Prints information about the QTree, including the number of inserted points, the result of the find operation, and the number of points affected by the growth operation.

This code demonstrates a complex and efficient implementation of a QTree data structure for organizing and querying 2D points. It showcases QTree operations like insertion, finding, marking, and growing, along with operations on points like calculating distances and moving points.