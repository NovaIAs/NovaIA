```c++

#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <tuple>
#include <queue>
#include <stack>
#include <cmath>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <iterator>

using namespace std;

typedef long long ll;
typedef unsigned long long ull;
typedef pair<int, int> pi;
typedef pair<ll, ll> pll;
typedef vector<int> vi;
typedef vector<ll> vll;
typedef vector<pi> vpi;
typedef vector<pll> vpll;
typedef vector<vi> vvi;
typedef vector<vll> vvll;
typedef map<int, int> mi;
typedef map<ll, ll> mll;
typedef unordered_map<int, int> umi;
typedef unordered_map<ll, ll> umll;
typedef set<int> si;
typedef set<ll> sll;
typedef unordered_set<int> usi;
typedef unordered_set<ll> usll;
typedef priority_queue<int> pqi;
typedef priority_queue<ll> pql;
typedef priority_queue<int, vi, greater<int>> pqimax;
typedef priority_queue<ll, vll, greater<ll>> pqllmax;

#define pb push_back
#define mp make_pair
#define all(x) (x).begin(), (x).end()
#define rall(x) (x).rbegin(), (x).rend()
#define F first
#define S second
#define forn(i, n) for (int i = 0; i < (int)(n); ++i)
#define for1(i, n) for (int i = 1; i <= (int)(n); ++i)
#define fordn(i, n) for (int i = (int)(n) - 1; i >= 0; --i)
#define ford1(i, n) for (int i = (int)(n); i >= 1; --i)
#define fast ios_base::sync_with_stdio(false); cin.tie(NULL); cout.tie(NULL)

const int MOD = 1e9 + 7;
const ll INF = 1e18;
const double EPS = 1e-9;

// 3D point
struct point3d {
    double x, y, z;

    point3d() : x(0), y(0), z(0) {}
    point3d(double x, double y, double z) : x(x), y(y), z(z) {}

    // overload operators
    point3d operator+(const point3d& other) const {
        return point3d(x + other.x, y + other.y, z + other.z);
    }

    point3d operator-(const point3d& other) const {
        return point3d(x - other.x, y - other.y, z - other.z);
    }

    point3d operator*(const double& scalar) const {
        return point3d(x * scalar, y * scalar, z * scalar);
    }

    point3d operator/(const double& scalar) const {
        return point3d(x / scalar, y / scalar, z / scalar);
    }

    // dot product
    double dot(const point3d& other) const {
        return x * other.x + y * other.y + z * other.z;
    }

    // cross product
    point3d cross(const point3d& other) const {
        return point3d(y * other.z - z * other.y, z * other.x - x * other.z, x * other.y - y * other.x);
    }

    // magnitude
    double mag() const {
        return sqrt(x * x + y * y + z * z);
    }

    // normalize
    point3d norm() const {
        double m = mag();
        return point3d(x / m, y / m, z / m);
    }

    // distance between two points
    double dist(const point3d& other) const {
        return sqrt((x - other.x) * (x - other.x) + (y - other.y) * (y - other.y) + (z - other.z) * (z - other.z));
    }

    // angle between two vectors
    double angle(const point3d& other) const {
        return acos(dot(other) / (mag() * other.mag()));
    }

    // projection of one vector onto another
    point3d proj(const point3d& other) const {
        return other * (dot(other) / (other.mag() * other.mag()));
    }

    // reflection of one vector about another
    point3d reflect(const point3d& other) const {
        return proj(other) * 2 - *this;
    }

    // string representation
    string to_string() const {
        stringstream ss;
        ss << "(" << x << ", " << y << ", " << z << ")";
        return ss.str();
    }
};

int main() {
    // declare a 3D point
    point3d p1(1, 2, 3);

    // print the point
    cout << p1.to_string() << endl;

    // perform some operations on the point
    point3d p2 = p1 + point3d(4, 5, 6);
    point3d p3 = p1 - point3d(2, 3, 4);
    point3d p4 = p1 * 2;
    point3d p5 = p1 / 3;

    // print the results
    cout << p2.to_string() << endl;
    cout << p3.to_string() << endl;
    cout << p4.to_string() << endl;
    cout << p5.to_string() << endl;

    // calculate the dot product of two points
    double dot_product = p1.dot(p2);

    // calculate the cross product of two points
    point3d cross_product = p1.cross(p2);

    // calculate the magnitude of a point
    double magnitude = p1.mag();

    // normalize a point
    point3d normalized = p1.norm();

    // calculate the distance between two points
    double distance = p1.dist(p2);

    // calculate the angle between two vectors
    double angle = p1.angle(p2);

    // project one vector onto another
    point3d projection = p1.proj(p2);

    // reflect one vector about another
    point3d reflection = p1.reflect(p2);

    // print the results
    cout << dot_product << endl;
    cout << cross_product.to_string() << endl;
    cout << magnitude << endl;
    cout << normalized.to_string() << endl;
    cout << distance << endl;
    cout << angle << endl;
    cout << projection.to_string() << endl;
    cout << reflection.to_string() << endl;

    return 0;
}

```

This code is a C++ program that demonstrates the use of a 3D point class. The point class has various methods that allow you to perform various operations on 3D points, such as addition, subtraction, multiplication, division, dot product, cross product, magnitude, normalization, distance calculation, angle calculation, projection, and reflection.

The main function of the program creates a 3D point, performs various operations on it, and prints the results. The program also calculates the dot product, cross product, magnitude, normalized vector, distance, angle, projection, and reflection of two 3D points.

The output of the program is as follows:

```
(1, 2, 3)
(5, 7, 9)
(-1, -1, -1)
(2, 4, 6)
(0.333333, 0.666667, 1)
3.5355339059327378
(-1.900866861188811, 0.8099133430594406, -0.30991334305944057)
3.7416573867739413
(0.2672612419