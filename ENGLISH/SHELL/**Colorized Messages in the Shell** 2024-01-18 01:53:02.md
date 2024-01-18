```shell
#!/bin/sh

# Define the colors to be used in the script
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[0;37m'

# Print a message to the console in red
echo "${RED}This is a message in red."

# Print a message to the console in green
echo "${GREEN}This is a message in green."

# Print a message to the console in yellow
echo "${YELLOW}This is a message in yellow."

# Print a message to the console in blue
echo "${BLUE}This is a message in blue."

# Print a message to the console in magenta
echo "${MAGENTA}This is a message in magenta."

# Print a message to the console in cyan
echo "${CYAN}This is a message in cyan."

# Print a message to the console in white
echo "${WHITE}This is a message in white."

# Reset the color to the default value
echo -e "${NC}This is a message in the default color."
```

Explanation:

* The first line of the script specifies the shell that will be used to execute the script. In this case, it is the Bourne shell (`/bin/sh`).
* The next few lines define the colors that will be used in the script. This is done using the `echo` command with the `-e` option, which allows you to interpret backslash escapes in the string. The `\033[0;31m` escape sequence sets the color to red, and the `m` at the end of the escape sequence resets the color to the default value.
* The `echo` command is then used to print a message to the console in each of the defined colors.
* The `-e` option is used again to interpret the `\033[0;37m` escape sequence, which sets the color to white.
* The `echo` command is then used to print a message to the console in the default color.