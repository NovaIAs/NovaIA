```ada
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;
with Ada.Containers.Vectors.Generic_Sorting;
use Ada.Containers.Vectors;

with Ada.Containers.Fixed_Points;
use Ada.Containers.Fixed_Points;

with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers.Doubly_Linked_Lists;

with Ada.Containers.Maps;
use Ada.Containers.Maps;

with Ada.Containers.Ordered_Maps;
use Ada.Containers.Ordered_Maps;

with Ada.Containers.Sets;
use Ada.Containers.Sets;

with Ada.Containers.Ordered_Sets;
use Ada.Containers.Ordered_Sets;

with Ada.Containers.Generic_Array_Sort;
use Ada.Containers.Generic_Array_Sort;

with Ada.Containers.Doubly_Linked_Lists.Generic_Sorting;
use Ada.Containers.Doubly_Linked_Lists.Generic_Sorting;

with Ada.Containers.Indefinite_Vectors;
use Ada.Containers.Indefinite_Vectors;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
use Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Ada.Containers.Indefinite_Maps;
use Ada.Containers.Indefinite_Maps;

with Ada.Containers.Indefinite_Ordered_Maps;
use Ada.Containers.Indefinite_Ordered_Maps;

with Ada.Containers.Indefinite_Sets;
use Ada.Containers.Indefinite_Sets;

with Ada.Containers.Indefinite_Ordered_Sets;
use Ada.Containers.Indefinite_Ordered_Sets;

with Ada.Containers.Generic_Array_Sort;
use Ada.Containers.Generic_Array_Sort;

with Ada.Containers.Indefinite_Doubly_Linked_Lists.Generic_Sorting;
use Ada.Containers.Indefinite_Doubly_Linked_Lists.Generic_Sorting;

with Ada.Integer_Bounded_IO; use Ada.Integer_Bounded_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Command_Line; use Ada.Command_Line;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Strings.Bounded; use Ada.Strings.Bounded;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Strings.Ordered_Maps; use Ada.Strings.Ordered_Maps;

with Ada.Strings.Sets; use Ada.Strings.Sets;

with Ada.Strings.Ordered_Sets; use Ada.Strings.Ordered_Sets;

with Ada.Strings.Hash; use Ada.Strings.Hash;

with Ada.Strings.Hash_Maps; use Ada.Strings.Hash_Maps;

with Ada.Strings.Hash_Ordered_Maps; use Ada.Strings.Hash_Ordered_Maps;

with Ada.Strings.Hash_Sets; use Ada.Strings.Hash_Sets;

with Ada.Strings.Hash_Ordered_Sets; use Ada.Strings.Hash_Ordered_Sets;

with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

with Ada.Strings.Bounded.Text_IO; use Ada.Strings.Bounded.Text_IO;

with Ada.Strings.Fixed.Text_IO; use Ada.Strings.Fixed.Text_IO;

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Ada.Characters.Latin_1.Wide_Strings; use Ada.Characters.Latin_1.Wide_Strings;

with Ada.Characters.Latin_1.Wide_Wide_Strings; use Ada.Characters.Latin_1.Wide_Wide_Strings;

with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;

with Ada.Wide_Characters.Latin_1; use Ada.Wide_Characters.Latin_1;

with Ada.Wide_Characters.Latin_1.Wide_Strings; use Ada.Wide_Characters.Latin_1.Wide_Strings;

with Ada.Wide_Characters.Latin_1.Wide_Wide_Strings; use Ada.Wide_Characters.Latin_1.Wide_Wide_Strings;

with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with Ada.Wide_Wide_Characters.Latin_1; use Ada.Wide_Wide_Characters.Latin_1;

with Ada.Wide_Wide_Characters.Latin_1.Wide_Strings; use Ada.Wide_Wide_Characters.Latin_1.Wide_Strings;

with Ada.Wide_Wide_Characters.Latin_1.Wide_Wide_Strings; use Ada.Wide_Wide_Characters.Latin_1.Wide_Wide_Strings;

with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;

with Ada.Strings.UTF_Encoding.Strings; use Ada.Strings.UTF_Encoding.Strings;

with Ada.Strings.UTF_Encoding.Wide_Strings; use Ada.Strings.UTF_Encoding.Wide_Strings;

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings; use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Ada.Strings.UTF_Encoding.Text_IO; use Ada.Strings.UTF_Encoding.Text_IO;

with Ada.Strings.UTF_Encoding.Strings.Text_IO; use Ada.Strings.UTF_Encoding.Strings.Text_IO;

with Ada.Strings.UTF_Encoding.Wide_Strings.Text_IO; use Ada.Strings.UTF_Encoding.Wide_Strings.Text_IO;

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Text_IO; use Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Text_IO;

with Ada.Strings.UTF_Encoding.Strings.Maps; use Ada.Strings.UTF_Encoding.Strings.Maps;

with Ada.Strings.UTF_Encoding.Ordered_Maps; use Ada.Strings.UTF_Encoding.Ordered_Maps;

with Ada.Strings.UTF_Encoding.Sets; use Ada.Strings.UTF_Encoding.Sets;

with Ada.Strings.UTF_Encoding.Ordered_Sets; use Ada.Strings.UTF_Encoding.Ordered_Sets;

with Ada.Strings.UTF_Encoding.Hash_Maps; use Ada.Strings.UTF_Encoding.Hash_Maps;

with Ada.Strings.UTF_Encoding.Hash_Ordered_Maps; use Ada.Strings.UTF_Encoding.Hash_Ordered_Maps;

with Ada.Strings.UTF_Encoding.Hash_Sets; use Ada.Strings.UTF_Encoding.Hash_Sets;

with Ada.Strings.UTF_Encoding.Hash_Ordered_Sets; use Ada.Strings.UTF_Encoding.Hash_Ordered_Sets;

with Ada.Strings.UTF_Encoding.Generic_Sorting; use Ada.Strings.UTF_Encoding.Generic_Sorting;

with Ada.Strings.UTF_Encoding.Generic_Array_Sort; use Ada.Strings.UTF_Encoding.Generic_Array_Sort;

with Ada.Strings.UTF_Encoding.Doubly_Linked_Lists; use Ada.Strings.UTF_Encoding.Doubly_Linked_Lists;

with Ada.Strings.UTF_Encoding.Doubly_Linked_Lists.Generic_Sorting; use Ada.Strings.UTF_Encoding.Doubly_Linked_Lists.Generic_Sorting;

with Ada.Strings.UTF_Encoding.Indefinite_Vectors; use Ada.Strings.UTF_Encoding.Indefinite_Vectors;

with Ada.Strings.UTF_Encoding.Indefinite_Doubly_Linked_Lists; use Ada.Strings.UTF_Encoding.Indefinite_Doubly_Linked_Lists;

with Ada.Strings.UTF_Encoding.Indefinite_Maps; use Ada.Strings.UTF_Encoding.Indefinite_Maps;

with Ada.Strings.UTF_Encoding.Indefinite_Ordered_Maps; use Ada.Strings.UTF_Encoding.Indefinite_Ordered_Maps;

with Ada.Strings.UTF_Encoding.Indefinite_Sets; use Ada.Strings.UTF_Encoding.Indefinite_Sets;

with Ada.Strings.UTF_Encoding.Indefinite_Ordered_Sets; use Ada.Strings.UTF_Encoding.Indefinite_Ordered_Sets;

with Ada.Strings.UTF_Encoding.Indefinite_Doubly_Linked_Lists.Generic_Sorting; use Ada.Strings.UTF_Encoding.Indefinite_Doubly_Linked_Lists.Generic_Sorting;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Command_Line; use Ada.Command_Line;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded