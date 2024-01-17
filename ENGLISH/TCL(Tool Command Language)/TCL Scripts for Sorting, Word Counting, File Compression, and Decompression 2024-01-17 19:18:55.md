```tcl
proc mergeLists {list1 list2} {
  if {[llength $list1] == 0} {
    return $list2
  } elseif {[llength $list2] == 0} {
    return $list1
  } elseif {[lrange $list1 0 0] < [lrange $list2 0 0]} {
    return [cons [lrange $list1 0 0] [mergeLists [lrange $list1 1 end] $list2]]
  } else {
    return [cons [lrange $list2 0 0] [mergeLists $list1 [lrange $list2 1 end]]]
  }
}

proc mergeSort {list} {
  if {[llength $list] <= 1} {
    return $list
  } else {
    set mid [expr {[llength $list] / 2}]
    set left [mergeSort [lrange $list 0 [expr {$mid - 1}]]]
    set right [mergeSort [lrange $list $mid end]]
    return [mergeLists $left $right]
  }
}

proc countWordsInFile {file} {
  set numWords 0
  foreach line [split [read $file] \n] {
    set words [split $line]
    incr numWords [llength $words]
  }
  return $numWords
}

proc compressFile {input output} {
  set file [open $input r]
  set compressed [open $output w]
  set prevChar ""
  foreach byte [read $file -nonewline] {
    if {$byte == $prevChar} {
      incr prevCount
    } else {
      if {$prevCount > 0} {
        puts $compressed [format "%s %d" $prevChar $prevCount]
      }
      set prevChar $byte
      set prevCount 1
    }
  }
  close $file
  close $compressed
}

proc decompressFile {input output} {
  set file [open $input r]
  set decompressed [open $output w]
  foreach line [split [read $file] \n] {
    set parts [split $line]
    for {set i 0} {$i < [lindex $parts 1]} {incr i} {
      puts $decompressed [lindex $parts 0]
    }
  }
  close $file
  close $decompressed
}

```

Explanation:

1. Merge Lists:
   - `mergeLists {list1 list2}`: This procedure takes two sorted lists, `list1` and `list2`, and merges them into a single sorted list.

2. Merge Sort:
   - `mergeSort {list}`: This procedure implements the merge sort algorithm to sort a given list in ascending order.
   - It uses recursion to divide the list into smaller sublists, sort them, and then merge them back together to obtain the final sorted list.

3. Count Words in a File:
   - `countWordsInFile {file}`: This procedure counts the number of words in a text file specified by `file`.
   - It reads the file line by line, splits each line into words, and increments a counter `numWords` accordingly.

4. Compress a File:
   - `compressFile {input output}`: This procedure compresses the contents of an input file `input` and writes the compressed data to an output file `output`.
   - It uses a simple compression algorithm that identifies and counts consecutive occurrences of characters in the input file and stores these counts along with the characters in the output file.

5. Decompress a File:
   - `decompressFile {input output}`: This procedure decompresses the contents of a compressed file `input` and writes the decompressed data to an output file `output`.
   - It reads the compressed file, extracts the characters and their counts, and reconstructs the original data by repeating the characters as per their respective counts.