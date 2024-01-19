```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Power is

   function Pow(Base : Positive; Exponent : Natural) return Natural;
   -- Recursively computes Base**Exponent --
   is
      Result : Natural := 1;
   begin
      if Exponent = 0 then
         return Result;
      else
         Result := Base * Pow(Base, Exponent - 1);
         return Result;
      end if;
   end Pow;

   ABC : constant array (Positive range <>) of Natural;
      -- This constant array contains twin primes, i.e. primes separated by 2. --
   is
      (97, 103),   (101, 107),   (103, 109),   (107, 113),
      (131, 137),   (157, 163),   (163, 167),   (167, 173),
      (179, 181),   (191, 193),   (193, 199),   (223, 227),
      (227, 229),   (233, 239),   (271, 277),   (281, 283),
      (307, 313),   (347, 349),   (353, 359),   (373, 379),
      (389, 397),   (409, 419),   (433, 439),   (461, 463),
      (503, 509),   (563, 577),   (587, 593),   (607, 613),
      (617, 619),   (643, 647),   (659, 661),   (673, 677),
      (683, 691),   (709, 719),   (743, 751),   (757, 761),
      (839, 853),   (853, 857),   (859, 863),   (877, 883),
      (881, 887),   (967, 971),   (1019, 1021), (1031, 1033),
      (1091, 1093), (1093, 1097), (1109, 1117), (1181, 1187),
      (1223, 1229), (1237, 1249), (1297, 1301), (1303, 1307),
      (1319, 1321), (1433, 1439), (1451, 1453), (1481, 1483),
      (1483, 1487), (1523, 1531), (1559, 1567), (1571, 1579),
      (1619, 1627), (1721, 1723), (1733, 1741), (1783, 1787),
      (1867, 1871), (1901, 1907), (1907, 1913), (1931, 1933),
      (1951, 1957), (1979, 1987), (2017, 2027), (2081, 2083),
      (2087, 2089), (2129, 2131), (2143, 2153), (2237, 2239),
      (2267, 2269), (2287, 2293), (2333, 2339), (2347, 2351),
      (2381, 2389), (2411, 2417), (2441, 2447), (2503, 2521),
      (2543, 2549), (2557, 2579), (2621, 2633), (2647, 2657),
      (2683, 2687), (2687, 2689), (2707, 2711), (2789, 2791),
      (2797, 2801), (2819, 2833), (2833, 2837), (2903, 2909),
      (2927, 2939), (2963, 2969), (2971, 2999), (3037, 3041),
      (3067, 3079), (3109, 3119), (3163, 3167), (3187, 3191),
      (3203, 3209), (3221, 3229), (3307, 3313), (3319, 3323),
      (3329, 3331), (3359, 3361), (3373, 3389), (3391, 3407),
      (3461, 3463), (3467, 3469), (3511, 3517), (3527, 3529),
      (3541, 3547), (3557, 3559), (3583, 3593), (3607, 3613),
      (3617, 3623), (3637, 3643), (3659, 3661), (3701, 3709),
      (3727, 3733), (3761, 3767), (3797, 3803), (3821, 3823),
      (3833, 3847), (3851, 3853), (3877, 3889), (3917, 3919),
      (3967, 3989), (4001, 4007), (4079, 4081), (4127, 4129),
      (4201, 4211), (4217, 4219), (4253, 4259), (4327, 4337),
      (4357, 4363), (4373, 4391), (4397, 4409), (4423, 4427),
      (4447, 4451), (4481, 4483), (4517, 4519), (4547, 4549),
      (4567, 4583), (4591, 4597), (4651, 4657), (4691, 4703),
      (4729, 4733), (4787, 4789), (4801, 4813), (4861, 4871),
      (4877, 4889), (4931, 4933), (4999, 5003);
   N : constant Natural := ABC'Last;  -- maximal index of ABC --

   function Is_Twin_Prime (P : Natural) return Boolean is
      -- Returns True if P is a twin prime, i.e. a prime number separated by 2 from
      -- another prime number. --
      (ABC (Positive (P - 1)) < P and ABC (Positive (P + 1)) > P);
   end Is_Twin_Prime;

   procedure Print_Twin_Prime (P : Natural) is
      -- Prints 'P is a twin prime' in a line, if P is a twin prime. --
   begin
      if Is_Twin_Prime (P) then
         Put ("P is a twin prime");
      end if;
   end Print_Twin_Prime;

begin
   -- Print all twin primes <= N. --
   for I in ABC'First .. N loop
      Print_Twin_Prime (ABC (I));
      New_Line;
   end loop;
end Power;
```

This Ada program finds and prints all twin primes <= N, where N is a constant defined in the program.

The program first defines a function `Is_Twin_Prime` that returns True if its argument is a twin prime, i.e. a prime number separated by 2 from another prime number.

Then, it defines a procedure `Print_Twin_Prime` that prints 'P is a twin prime' in a line, if its argument is a twin prime.

Finally, the program prints all twin primes <= N by iterating over the range of indices of the array ABC and calling the procedure `Print_Twin_Prime` for each index.