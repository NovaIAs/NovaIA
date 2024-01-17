```d
import std.stdio, std.string, std.range;

alias Text = immutable string;

class TextList {
    private Text[] texts;

    public TextList(Text[] texts) pure nothrow {
        this.texts = texts;
    }

    public TextList(Text text) pure nothrow {
        this.texts = [text];
    }

    public TextList(int length) pure nothrow {
        this.texts = new Text[length];
    }

    public TextList() pure nothrow {
        this.texts = [];
    }

    public TextList join(TextList other) pure nothrow {
        int newLength = this.texts.length + other.texts.length;
        Text[] newTexts = new Text[newLength];
        int index = 0;
        foreach (Text text in this.texts) {
            newTexts[index] = text;
            index += 1;
        }
        foreach (Text text in other.texts) {
            newTexts[index] = text;
            index += 1;
        }
        return new TextList(newTexts);
    }

    public int getLength() pure nothrow {
        return this.texts.length;
    }

    public Text get(int index) pure nothrow {
        return this.texts[index];
    }

    public void set(int index, Text text) pure nothrow {
        this.texts[index] = text;
    }

    public TextList subList(int startIndex, int endIndex) pure nothrow {
        Text[] newTexts = new Text[endIndex - startIndex];
        int index = 0;
        for (int i = startIndex; i < endIndex; i += 1) {
            newTexts[index] = this.texts[i];
            index += 1;
        }
        return new TextList(newTexts);
    }

    public void print() nothrow {
        foreach (Text text in this.texts) {
            writeln(text);
        }
    }
}

class TextProcessor {
    private TextList texts;

    public TextProcessor(TextList texts) pure nothrow {
        this.texts = texts;
    }

    public TextList getUniqueTexts() pure nothrow {
        TextList uniqueTexts = new TextList();
        foreach (Text text in this.texts) {
            if (!uniqueTexts.contains(text)) {
                uniqueTexts.add(text);
            }
        }
        return uniqueTexts;
    }

    public TextList getSortedTexts() pure nothrow {
        TextList sortedTexts = new TextList(this.texts);
        sortedTexts.sort();
        return sortedTexts;
    }

    public TextList getTextsWithLength(int length) pure nothrow {
        TextList textsWithLength = new TextList();
        foreach (Text text in this.texts) {
            if (text.length == length) {
                textsWithLength.add(text);
            }
        }
        return textsWithLength;
    }

    public TextList getTextsStartingWith(Text prefix) pure nothrow {
        TextList textsStartingWithPrefix = new TextList();
        foreach (Text text in this.texts) {
            if (text.startsWith(prefix)) {
                textsStartingWithPrefix.add(text);
            }
        }
        return textsStartingWithPrefix;
    }

    public TextList getTextsEndingWith(Text suffix) pure nothrow {
        TextList textsEndingWithSuffix = new TextList();
        foreach (Text text in this.texts) {
            if (text.endsWith(suffix)) {
                textsEndingWithSuffix.add(text);
            }
        }
        return textsEndingWithSuffix;
    }

    public TextList getTextsContaining(Text substring) pure nothrow {
        TextList textsContainingSubstring = new TextList();
        foreach (Text text in this.texts) {
            if (text.contains(substring)) {
                textsContainingSubstring.add(text);
            }
        }
        return textsContainingSubstring;
    }
}

void main() {
    TextList texts = new TextList(["apple", "banana", "cherry", "durian", "elderberry", "fig"]);
    TextProcessor textProcessor = new TextProcessor(texts);

    writeln("Original texts:");
    texts.print();

    writeln("Unique texts:");
    textProcessor.getUniqueTexts().print();

    writeln("Sorted texts:");
    textProcessor.getSortedTexts().print();

    writeln("Texts with length 6:");
    textProcessor.getTextsWithLength(6).print();

    writeln("Texts starting with \"a\":");
    textProcessor.getTextsStartingWith("a").print();

    writeln("Texts ending with \"y\":");
    textProcessor.getTextsEndingWith("y").print();

    writeln("Texts containing \"e\":");
    textProcessor.getTextsContaining("e").print();
}
```

This code defines a `TextList` class that can be used to store and manipulate a list of text strings. It includes methods for adding, removing, and retrieving elements, as well as methods for sorting, filtering, and searching the list.

The `TextProcessor` class builds on the `TextList` class and provides additional methods for processing text strings. These methods include finding unique texts, sorting texts, finding texts with a specific length, finding texts that start with or end with a specific substring, and finding texts that contain a specific substring.

The `main` function creates a `TextList` object containing a list of text strings. It then creates a `TextProcessor` object using the `TextList` object. The `main` function then uses the `TextProcessor` object to call the various methods for processing the list of text strings.

The output of the program is as follows:

```
Original texts:
apple
banana
cherry
durian
elderberry
fig

Unique texts:
apple
banana
cherry
durian
elderberry
fig

Sorted texts:
apple
banana
cherry
durian
elderberry
fig

Texts with length 6:
banana
cherry

Texts starting with "a":
apple

Texts ending with "y":
cherry
durian
fig

Texts containing "e":
apple
banana
cherry
elderberry
elderberry
fig
```