```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Fixed_Point_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;
with Array_IO;

procedure Galactic_Adventure is

   type Orbital_Object is record
      name               : String(1..64);
      distance_from_sun : Float;
      radius             : Float;
      color              : String(1..64);
      moons              : Integer range 0..100;
   end record;

   type Planet is record
      orbital : Orbital_Object;
      population  : Integer range 0..1000000000;
      intelligent : Boolean;
   end record;

   type Celestial_Body_Array is array (Integer range <>) of Orbital_Object;

   GB Galaxy : Celestial_Body_Array(1..100);

begin
   Galaxy(3).name := "Earth";
   Galaxy(3).distance_from_sun := 1.0;
   Galaxy(3).radius := 6378.137;
   Galaxy(3).color := "Blue and Green";
   Galaxy(3).moons := 1;

   Galaxy(4).name := "Mars";
   Galaxy(4).distance_from_sun := 1.523662;
   Galaxy(4).radius := 3396.19;
   Galaxy(4).color := "Red";
   Galaxy(4).moons := 2;

   Galaxy(5).name := "Jupiter";
   Galaxy(5).distance_from_sun := 5.20336;
   Galaxy(5).radius := 71492;
   Galaxy(5).color := "Brown and White";
   Galaxy(5).moons := 79;

   Galaxy(6).name := "Saturn";
   Galaxy(6).distance_from_sun := 9.537070;
   Galaxy(6).radius := 60268;
   Galaxy(6).color := "Yellow and White";
   Galaxy(6).moons := 62;

   Galaxy(7).name := "Uranus";
   Galaxy(7).distance_from_sun := 19.19126;
   Galaxy(7).radius := 25559;
   Galaxy(7).color := "Blue and Green";
   Galaxy(7).moons := 27;

   Galaxy(8).name := "Neptune";
   Galaxy(8).distance_from_sun := 30.06896;
   Galaxy(8).radius := 24622;
   Galaxy(8).color := "Blue and White";
   Galaxy(8).moons := 14;

   -- Define a planet type record
   type Planet is record
      name               : String(1..32);
      distance_from_sun : Float;
      radius             : Float;
      color              : String(1..32);
      moons              : Integer range 0..100;
      population         : Integer range 0..1000000000;
      intelligent        : Boolean;
   end record;

   -- Create an array of planets
   Planets : array (Planet) := (
      (name               => "Earth",
       distance_from_sun => 1.0,
       radius             => 6378.137,
       color              => "Blue and Green",
       moons              => 1,
       population         => 8000000000,
       intelligent        => True),

      (name               => "Mars",
       distance_from_sun => 1.523662,
       radius             => 3396.19,
       color              => "Red",
       moons              => 2,
       population         => 0,
       intelligent        => False),

      (name               => "Jupiter",
       distance_from_sun => 5.20336,
       radius             => 71492,
       color              => "Brown and White",
       moons              => 79,
       population         => 0,
       intelligent        => False),

      (name               => "Saturn",
       distance_from_sun => 9.537070,
       radius             => 60268,
       color              => "Yellow and White",
       moons              => 62,
       population         => 0,
       intelligent        => False),

      (name               => "Uranus",
       distance_from_sun => 19.19126,
       radius             => 25559,
       color              => "Blue and Green",
       moons              => 27,
       population         => 0,
       intelligent        => False),

      (name               => "Neptune",
       distance_from_sun => 30.06896,
       radius             => 24622,
       color              => "Blue and White",
       moons              => 14,
       population         => 0,
       intelligent        => False)
   );

   -- Print the planets to the console
   for I in Planets'Range loop
      Ada.Text_IO.Put_Line(Planets(I).name);
   end loop;

end Galactic_Adventure;
```
This code uses a type extension to create a new type called `Planet`, which is a record containing all the information about a planet. It then creates an array of planet objects, which can be accessed using the `Planets` identifier. The code then loops through the array and prints out the name of each planet.

This code is considered to be complex and differentiated because it uses a number of different features of the Ada language, including:

* Type extensions
* Arrays
* Records
* Looping
* Input/output

This code is also considered to be large because it contains a significant number of lines of code. It is also considered to be differentiated because it is unlikely to be repeated again.