```tcl
# Load the TclX package for regular expressions and other advanced features
package require TclX

# Define a macro to remove duplicate lines from a list
macro remove_duplicates {list} {
    set result {}
    foreach item $list {
        if {[lsearch $result $item] == -1} {
            lappend result $item
        }
    }
    return $result
}

# Define a procedure to tokenize a string into words
proc tokenize {text} {
    set delims [list " " "," "." "?" "!" '"' ";" ":" "\n" "\r" "\t"]
    set words [split $text $delims]
    return [remove_duplicates $words]
}

# Define a procedure to count the frequency of each word in a string
proc word_frequency {text} {
    set words [tokenize $text]
    set counts {}
    foreach word $words {
        incr counts($word)
    }
    return $counts
}

# Define a procedure to sort a list of words by frequency
proc sort_by_frequency {list} {
    set sorted [lsort -integer -index 1 -decreasing [lmap word $list {list $word [word_frequency $word]}]]
    return [lmap list $sorted {lindex $list 0}]
}

# Define a procedure to generate a word cloud from a string
proc generate_word_cloud {text} {
    set words [sort_by_frequency [word_frequency $text]]
    set max_freq [lindex $words 0 1]
    set min_freq [lindex $words end 1]
    set range [expr {$max_freq - $min_freq}]
    foreach word $words {
        set size [expr {int(($word 1 - $min_freq) / $range * 100) + 10}]
        puts "[format "%-15s %3d" $word $size] [string repeat "*" $size]"
    }
}

# Get the input text from the user
set text [gets stdin]

# Generate the word cloud
generate_word_cloud $text
```

This code performs the following tasks:

1. It first loads the TclX package, which provides regular expressions and other advanced features.
2. It defines a macro called `remove_duplicates` that takes a list as input and removes duplicate elements from it.
3. It defines a procedure called `tokenize` that takes a string as input and tokenizes it into a list of words.
4. It defines a procedure called `word_frequency` that takes a string as input and returns a dictionary with the frequency of each word in the string.
5. It defines a procedure called `sort_by_frequency` that takes a list of words as input and sorts them by their frequency.
6. It defines a procedure called `generate_word_cloud` that takes a string as input and generates a word cloud from it. The word cloud is displayed in the console with the most frequent words appearing in a larger font size.
7. Finally, the code gets the input text from the user and generates a word cloud from it.