```PERL

# Declare a hash to store the available colors
%colours = (
    'red'   => '#FF0000',
    'orange' => '#FFA500',
    'yellow' => '#FFFF00',
    'green'  => '#008000',
    'blue'   => '#0000FF',
    'indigo' => '#4B0082',
    'violet' => '#8B00FF'
);

# Declare a hash to store the available shapes
%shapes = (
    'circle'   => 'o',
    'square'   => '[]',
    'triangle' => '^',
    'star'     => '*'
);

# Declare a hash to store the available sizes
%sizes = (
    'small'  => 10,
    'medium' => 20,
    'large'  => 30
);

# Declare an array to store the available options
@options = (
    'color',
    'shape',
    'size'
);

# Get the user's input
print "What color do you want? ";
my $color = <STDIN>;
chomp $color;

print "What shape do you want? ";
my $shape = <STDIN>;
chomp $shape;

print "What size do you want? ";
my $size = <STDIN>;
chomp $size;

# Validate the user's input
if (!exists $colours{$color}) {
    print "Invalid color.\n";
    exit;
}

if (!exists $shapes{$shape}) {
    print "Invalid shape.\n";
    exit;
}

if (!exists $sizes{$size}) {
    print "Invalid size.\n";
    exit;
}

# Get the corresponding code for the selected options
my $color_code = $colours{$color};
my $shape_code = $shapes{$shape};
my $size_code = $sizes{$size};

# Create the string representation of the selected options
my $selected_options = "Color: $color, Shape: $shape, Size: $size";

# Display the selected options
print "You selected the following options:\n";
print "$selected_options\n";

# Create the HTML code for the selected options
my $html = "<html>\n<body>\n";
$html .= "<p style=\"color: $color_code; font-size: $size_codepx\">$shape_code</p>\n";
$html .= "</body>\n</html>\n";

# Display the HTML code
print "The HTML code for the selected options is:\n";
print "$html";

```

This code is a PERL program that allows the user to select a color, shape, and size for a graphical object. The program then generates HTML code that can be used to display the object on a web page.

The program first declares three hashes to store the available colors, shapes, and sizes. It then declares an array to store the available options.

Next, the program gets the user's input for the color, shape, and size. It then validates the user's input to make sure that it is valid.

If the user's input is valid, the program gets the corresponding code for the selected options. It then creates a string representation of the selected options and displays it to the user.

Finally, the program creates the HTML code for the selected options and displays it to the user.