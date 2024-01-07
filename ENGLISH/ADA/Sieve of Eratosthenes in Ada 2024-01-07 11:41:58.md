```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Primes is
   type Prime_List is array (Positive range <>) of Positive;
   function Append_Prime (P: Prime_List; N: Positive) return Prime_List;
   function Sieve_Of_Eratosthenes (N: Positive) return Prime_List;
begin
   for P in Sieve_Of_Eratosthenes (1000) loop
      Put (P'Image (0));
   end loop;
   New_Line;
end Primes;

function Append_Prime (P: Prime_List; N: Positive) return Prime_List is
begin
   if P'Last = 0 then
      return Prime_List'(0 => N);
   else
      return Prime_List'(P'Range => P, P'Last + 1 => N);
   end if;
end Append_Prime;

function Sieve_Of_Eratosthenes (N: Positive) return Prime_List is
   type State_Array is array (Positive range <>) of Boolean;
   function Allocate_State (N: Positive) return State_Array;
   function Mark_Multiples (S: State_Array; P: Positive) return State_Array;
   function Collect_Primes (S: State_Array) return Prime_List;

   S: State_Array;
begin
   S := Allocate_State (N);
   for P in 2 .. N loop
      if S(P) then
         S := Mark_Multiples (S, P);
      end if;
   end loop;
   return Collect_Primes (S);
end Sieve_Of_Eratosthenes;

function Allocate_State (N: Positive) return State_Array is
begin
   return (others => True);
end Allocate_State;

function Mark_Multiples (S: State_Array; P: Positive) return State_Array is
begin
   for I in P * 2 .. S'Last by P loop
      S(I) := False;
   end loop;
   return S;
end Mark_Multiples;

function Collect_Primes (S: State_Array) return Prime_List is
   function Append_If_Prime (L: Prime_List; I: Positive) return Prime_List;
begin
   return Prime_List'(L'Range => L, L'Last + 1 => I);
end Append_If_Prime;

   L: Prime_List := (0 => 0);
begin
   for I in S'Range loop
      if S(I) then
         L := Append_If_Prime (L, I);
      end if;
   end loop;
   return L;
end Collect_Primes;
```

This code implements the Sieve of Eratosthenes algorithm to find all prime numbers up to a given limit.

The main procedure, `Primes`, calls the function `Sieve_Of_Eratosthenes` to find all prime numbers up to 1000, and then prints them out.

The function `Sieve_Of_Eratosthenes` takes a positive integer `N` as input, and returns a list of all prime numbers up to `N`.

The function works by creating an array of booleans, called `S`, which is indexed by the integers from 2 to `N`. The array `S` is initially set to `True` for all indices.

The function then loops through the integers from 2 to `N`, and for each integer `P`, it marks all multiples of `P` in the array `S` as `False`.

After the loop is finished, the array `S` contains a list of all prime numbers up to `N`, because the only indices that are still set to `True` are the indices of prime numbers.

The function `Collect_Primes` takes the array `S` as input, and returns a list of all prime numbers up to `N`.

The function works by looping through the indices of the array `S`, and for each index `I` that is set to `True`, it appends `I` to the list of prime numbers.

The function `Append_Prime` takes a list of prime numbers `P` and a positive integer `N`, and returns a new list of prime numbers that is the same as `P`, but with `N` appended to the end.

The function `Append_If_Prime` takes a list of prime numbers `L` and a positive integer `I`, and returns a new list of prime numbers that is the same as `L`, but with `I` appended to the end, if `I` is a prime number.

The function `Allocate_State` takes a positive integer `N` as input, and returns an array of booleans, called `S`, which is indexed by the integers from 2 to `N`. The array `S` is initially set to `True` for all indices.

The function `Mark_Multiples` takes an array of booleans `S` and a positive integer `P` as input, and returns a new array of booleans that is the same as `S`, but with all multiples of `P` marked as `False`.