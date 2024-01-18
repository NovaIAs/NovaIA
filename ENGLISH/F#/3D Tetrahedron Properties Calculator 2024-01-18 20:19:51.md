```f#
// Define a data type to represent a point in 3D space.
type Point3D = { X: float; Y: float; Z: float }

// Define a function to calculate the distance between two points in 3D space.
let distance (p1: Point3D) (p2: Point3D) =
    sqrt((p1.X - p2.X) ** 2.0 + (p1.Y - p2.Y) ** 2.0 + (p1.Z - p2.Z) ** 2.0)

// Define a data type to represent a triangle in 3D space.
type Triangle3D = { V0: Point3D; V1: Point3D; V2: Point3D }

// Define a function to calculate the area of a triangle in 3D space.
let area (t: Triangle3D) =
    let v0v1 = distance t.V0 t.V1
    let v0v2 = distance t.V0 t.V2
    let v1v2 = distance t.V1 t.V2
    let s = (v0v1 + v0v2 + v1v2) / 2.0
    sqrt(s * (s - v0v1) * (s - v0v2) * (s - v1v2))

// Define a data type to represent a tetrahedron in 3D space.
type Tetrahedron3D = { V0: Point3D; V1: Point3D; V2: Point3D; V3: Point3D }

// Define a function to calculate the volume of a tetrahedron in 3D space.
let volume (t: Tetrahedron3D) =
    let a = area { V0 = t.V0; V1 = t.V1; V2 = t.V2 }
    let b = area { V0 = t.V0; V1 = t.V2; V2 = t.V3 }
    let c = area { V0 = t.V0; V1 = t.V3; V2 = t.V1 }
    let d = area { V0 = t.V1; V1 = t.V2; V2 = t.V3 }
    let p = (a + b + c + d) / 2.0
    (1.0 / 3.0) * sqrt(4.0 * p * (p - a) * (p - b) * (p - c) * (p - d))

// Define a function to calculate the circumradius of a tetrahedron in 3D space.
let circumradius (t: Tetrahedron3D) =
    let v0v1 = distance t.V0 t.V1
    let v0v2 = distance t.V0 t.V2
    let v0v3 = distance t.V0 t.V3
    let v1v2 = distance t.V1 t.V2
    let v1v3 = distance t.V1 t.V3
    let v2v3 = distance t.V2 t.V3
    let abc = area { V0 = t.V0; V1 = t.V1; V2 = t.V2 }
    let acd = area { V0 = t.V0; V1 = t.V2; V2 = t.V3 }
    let adb = area { V0 = t.V0; V1 = t.V3; V2 = t.V1 }
    let bcd = area { V0 = t.V1; V1 = t.V2; V2 = t.V3 }
    let r = 3.0 * (v0v1 * v0v2 * v0v3 * abc * acd * adb * bcd) /
            ((v0v1 + v0v2 + v0v3) * (v0v1 + v0v2 - v0v3) * (v0v1 - v0v2 + v0v3) * (-v0v1 + v0v2 + v0v3))
    r

// Define a function to calculate the inradius of a tetrahedron in 3D space.
let inradius (t: Tetrahedron3D) =
    let v0v1 = distance t.V0 t.V1
    let v0v2 = distance t.V0 t.V2
    let v0v3 = distance t.V0 t.V3
    let v1v2 = distance t.V1 t.V2
    let v1v3 = distance t.V1 t.V3
    let v2v3 = distance t.V2 t.V3
    let abc = area { V0 = t.V0; V1 = t.V1; V2 = t.V2 }
    let acd = area { V0 = t.V0; V1 = t.V2; V2 = t.V3 }
    let adb = area { V0 = t.V0; V1 = t.V3; V2 = t.V1 }
    let bcd = area { V0 = t.V1; V1 = t.V2; V2 = t.V3 }
    let r = 3.0 * abc * acd * adb * bcd /
            ((v0v1 + v0v2 + v0v3) * (v0v1 + v0v2 - v0v3) * (v0v1 - v0v2 + v0v3) * (-v0v1 + v0v2 + v0v3))
    r

// Define a function to calculate the centroid of a tetrahedron in 3D space.
let centroid (t: Tetrahedron3D) =
    let v0 = t.V0
    let v1 = t.V1
    let v2 = t.V2
    let v3 = t.V3
    { X = (v0.X + v1.X + v2.X + v3.X) / 4.0; Y = (v0.Y + v1.Y + v2.Y + v3.Y) / 4.0; Z = (v0.Z + v1.Z + v2.Z + v3.Z) / 4.0 }

// Define a function to calculate the orthocenter of a tetrahedron in 3D space.
let orthocenter (t: Tetrahedron3D) =
    let v0 = t.V0
    let v1 = t.V1
    let v2 = t.V2
    let v3 = t.V3
    // Calculate the centroid of the tetrahedron.
    let g = centroid t
    // Calculate the vectors from the centroid to each vertex.
    let v0g = { X = v0.X - g.X; Y = v0.Y - g.Y; Z = v0.Z - g.Z }
    let v1g = { X = v1.X - g.X; Y = v1.Y - g.Y; Z = v1.Z - g.Z }
    let v2g = { X = v2.X - g.X; Y = v2.Y - g.Y; Z = v2.Z - g.Z }
    let v3g = { X = v3.X - g.X; Y = v3.Y - g.Y; Z = v3.Z - g.Z }
    // Calculate the cross product of the vectors from the centroid to two opposite vertices.
    let v0v1xv2v3 = {
        X = v0g.Y * v2g.Z - v0g.Z * v2g.Y;
        Y = v0g.Z * v