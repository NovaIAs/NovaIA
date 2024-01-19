```tcl
proc extract_numbers {text} {
    set result {}
    set pattern "[0-9]+"
    foreach match [regexp -all $pattern $text] {
        lappend result $match
    }
    return $result
}

proc find_words {text} {
    set result {}
    set pattern "[[:alpha:]]+"
    foreach match [regexp -all $pattern $text] {
        lappend result $match
    }
    return $result
}

proc clean_text {text} {
    set result ""
    foreach char $text {
        if {[string is word $char] || [string is digit $char] || [string is space $char]} {
            lappend result $char
        }
    }
    return $result
}

proc count_occurrences {text} {
    set result {}
    foreach word $text {
        incr result($word)
    }
    return $result
}

proc create_word_cloud {data} {
    set max_val [lrange $data end end]
    foreach word $data {
        set size [expr {int(($data($word) / $max_val) * 50) + 10}]
        puts "`format %3s %3d`" $size $data($word)
    }
}

set text "This is a sample text. It has 11 words and 10 numbers. This is another sentence. It has 13 words and 12 numbers."

set clean_text [clean_text $text]
set numbers [extract_numbers $clean_text]
set words [find_words $clean_text]
set word_counts [count_occurrences $words]

puts "Numbers:"
puts "[join $numbers \n]"
puts "\nWords:"
puts "[join $words \n]"
puts "\nWord Counts:"
puts "[lreverse [lsort -integer -decreasing [lmap $word_counts {list $word $value}]]]"
puts "\nWord Cloud:"
create_word_cloud $word_counts
```

**Explanation:**

This Tcl script performs the following tasks:

1. **Extract Numbers:**
   - Uses the `extract_numbers` procedure to extract all numbers from the given text.
   - The `regexp -all` command is used with the `[0-9]+` pattern to match one or more consecutive digits.

2. **Find Words:**
   - Uses the `find_words` procedure to extract all words from the text.
   - The `regexp -all` command is used with the `[[:alpha:]]+` pattern to match one or more consecutive alphabetic characters.

3. **Clean Text:**
   - Uses the `clean_text` procedure to remove all non-word, non-digit, and non-space characters from the text.

4. **Count Occurrences:**
   - Uses the `count_occurrences` procedure to count the occurrences of each word in the cleaned text.
   - The `lmap` command is used to convert each word to a list containing the word and its count.
   - The `lreverse` and `lsort` commands are used to sort the list of word counts in decreasing order.

5. **Create Word Cloud:**
   - Uses the `create_word_cloud` procedure to generate a word cloud based on the word counts.
   - The size of each word in the word cloud is proportional to its count.

The final output of the script includes the extracted numbers, found words, word counts, and a word cloud based on the word counts.