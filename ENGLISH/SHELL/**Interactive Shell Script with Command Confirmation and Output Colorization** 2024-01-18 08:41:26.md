```
#!/bin/sh

# Define colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Display usage information
usage()
{
    echo "Usage: $0 [options] [--] <command>" >&2
    echo -e "Options:" >&2
    echo "  -h, --help        Display this help and exit" >&2
    echo "  -v, --verbose     Enable verbose output" >&2
    echo "  -d, --debug       Enable debug output" >&2
    echo "  -f, --force       Force the execution of the command" >&2
    echo -e "" >&2
    echo "Examples:" >&2
    echo "  $0 -v 'echo hello world'" >&2
    echo "  $0 -f 'rm -rf /tmp/*'" >&2
}

# Parse command line options
VERBOSE=0
DEBUG=0
FORCE=0
while [ "$1" != "--" ] && [ -n "$1" ]; do
    case "$1" in
        -h|--help)
            usage
            exit 0
            ;;
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -d|--debug)
            DEBUG=1
            shift
            ;;
        -f|--force)
            FORCE=1
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            echo -e "${RED}Error:${NC} Invalid option: $1" >&2
            usage
            exit 1
            ;;
    esac
done

# Check if the command was specified
if [ $# -lt 1 ]; then
    echo -e "${RED}Error:${NC} Missing command" >&2
    usage
    exit 1
fi

# Get the command and its arguments
COMMAND="$1"
shift

# Display command information if verbose is enabled
if [ $VERBOSE -eq 1 ]; then
    echo -e "${YELLOW}Command:${NC} $COMMAND"
    echo -e "${YELLOW}Arguments:${NC} $@"
fi

# Display debug information if debug is enabled
if [ $DEBUG -eq 1 ]; then
    echo -e "${MAGENTA}Verbose output is enabled"
    echo -e "${MAGENTA}Debug output is enabled"
    echo -e "${MAGENTA}Force option is $FORCE"
fi

# Execute the command
if [ $FORCE -eq 1 ]; then
    eval "$COMMAND" "$@"
else
    read -p "Are you sure you want to execute this command? (y/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        eval "$COMMAND" "$@"
    else
        echo -e "${GREEN}Command canceled"
    fi
fi

```

**Explanation:**

1. Define colors for output: We define ANSI color codes for different messages to make the output more visually appealing.
2. Display usage information: The `usage()` function displays information about how to use the script and its options.
3. Parse command line options: We use a `while` loop to parse command line options using the `getopts` command.
4. Check if the command was specified: We check if the user provided a command to execute.
5. Get the command and its arguments: We extract the command and its arguments from the command line.
6. Display command information if verbose is enabled: If the `-v` or `--verbose` option is specified, we display information about the command and its arguments.
7. Display debug information if debug is enabled: If the `-d` or `--debug` option is specified, we display additional information about the script's execution.
8. Execute the command: We execute the command using `eval`, which allows us to pass arguments to the command dynamically.
9. Confirmation prompt if force option is not specified: If the `-f` or `--force` option is not specified, we prompt the user to confirm the execution of the command.