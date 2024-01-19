```shell
#!/bin/bash

# Define colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Check if the user is root
if [[ $EUID -ne 0 ]]; then
  echo -e "${RED}This script must be run as root.${NC}"
  exit 1
fi

# Define the main menu options
main_menu_options=(
  "Install Apache"
  "Configure Apache"
  "Start Apache"
  "Stop Apache"
  "Restart Apache"
  "Exit"
)

# Define the install Apache function
install_apache() {
  echo -e "${YELLOW}Installing Apache...${NC}"
  apt-get update
  apt-get install -y apache2
  echo -e "${GREEN}Apache installed successfully.${NC}"
}

# Define the configure Apache function
configure_apache() {
  echo -e "${YELLOW}Configuring Apache...${NC}"
  a2enmod rewrite
  a2enmod ssl
  echo -e "${GREEN}Apache configured successfully.${NC}"
}

# Define the start Apache function
start_apache() {
  echo -e "${YELLOW}Starting Apache...${NC}"
  systemctl start apache2
  echo -e "${GREEN}Apache started successfully.${NC}"
}

# Define the stop Apache function
stop_apache() {
  echo -e "${YELLOW}Stopping Apache...${NC}"
  systemctl stop apache2
  echo -e "${GREEN}Apache stopped successfully.${NC}"
}

# Define the restart Apache function
restart_apache() {
  echo -e "${YELLOW}Restarting Apache...${NC}"
  systemctl restart apache2
  echo -e "${GREEN}Apache restarted successfully.${NC}"
}

# Define the exit function
exit_script() {
  echo -e "${YELLOW}Exiting the script...${NC}"
  exit 0
}

# Display the main menu
while true; do
  clear
  echo -e "${CYAN}Apache Web Server Management Script${NC}"
  echo
  for ((i = 0; i < ${#main_menu_options[@]}; i++)); do
    echo "$((i + 1)). ${main_menu_options[i]}"
  done
  echo
  read -p "Enter your choice [1-${#main_menu_options[@]}]: " choice

  # Handle the user's choice
  case $choice in
    1)
      install_apache
      ;;
    2)
      configure_apache
      ;;
    3)
      start_apache
      ;;
    4)
      stop_apache
      ;;
    5)
      restart_apache
      ;;
    6)
      exit_script
      ;;
    *)
      echo -e "${RED}Invalid choice. Please enter a number between 1 and ${#main_menu_options[@]}.${NC}"
      ;;
  esac

  # Pause for 3 seconds before looping back to the main menu
  sleep 3
done
```

Explanation:

This script is a menu-driven program that allows the user to manage an Apache web server on a Linux system.

The script first defines a list of colors that are used to format the output.

It then checks if the user is running the script as root. If the user is not root, the script exits with an error message.

The script then defines the main menu options, which are:

* Install Apache
* Configure Apache
* Start Apache
* Stop Apache
* Restart Apache
* Exit

The script then defines a series of functions that correspond to each of the main menu options.

The `install_apache` function installs Apache on the system.

The `configure_apache` function configures Apache by enabling the rewrite and ssl modules.

The `start_apache` function starts the Apache web server.

The `stop_apache` function stops the Apache web server.

The `restart_apache` function restarts the Apache web server.

The `exit_script` function exits the script.

The script then enters a loop that displays the main menu and waits for the user to select an option.

When the user selects an option, the script calls the corresponding function to perform the selected action.

After the selected action is performed, the script pauses for 3 seconds before looping back to the main menu.