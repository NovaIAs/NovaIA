```
#!/bin/bash

# This script creates a dynamic menu of available network interfaces and allows the user to select one for configuration.

# Define network interface variables
eth0_status=$(ip link show eth0 | grep -o "UP")
eth1_status=$(ip link show eth1 | grep -o "UP")
wlan0_status=$(ip link show wlan0 | grep -o "UP")

# Check the status of each network interface
if [ "$eth0_status" == "UP" ]; then
  eth0_active=1
else
  eth0_active=0
fi

if [ "$eth1_status" == "UP" ]; then
  eth1_active=1
else
  eth1_active=0
fi

if [ "$wlan0_status" == "UP" ]; then
  wlan0_active=1
else
  wlan0_active=0
fi

# Create a dynamic menu of available network interfaces
clear
echo "Network Interface Configuration"
echo
if [ "$eth0_active" == "1" ]; then
  echo "1. eth0"
fi
if [ "$eth1_active" == "1" ]; then
  echo "2. eth1"
fi
if [ "$wlan0_active" == "1" ]; then
  echo "3. wlan0"
fi
echo "4. Quit"

# Read the user's choice
read -p "Enter your choice: " choice

# Perform the selected action
case "$choice" in
  1)
    ifconfig eth0 up
    echo "eth0 is now active."
    ;;
  2)
    ifconfig eth1 up
    echo "eth1 is now active."
    ;;
  3)
    ifconfig wlan0 up
    echo "wlan0 is now active."
    ;;
  4)
    exit 0
    ;;
  *)
    echo "Invalid choice."
    ;;
esac

# Display the new IP address
ip addr show dev $interface | grep "inet " | awk '{print $2}'
```

This script first checks the status of each network interface and then creates a dynamic menu of available network interfaces. The user is then prompted to select an interface from the menu. Once an interface is selected, the script performs the appropriate action (e.g., activating the interface or displaying the IP address).

Here is a more detailed explanation of the code:

* **Lines 1-3:** Define the network interface variables. These variables will be used to store the status of each network interface.
* **Lines 5-12:** Check the status of each network interface. The `ip link show` command is used to get the status of each interface. The `grep` command is then used to filter the output of the `ip link show` command and extract the status of the interface.
* **Lines 14-19:** Create a dynamic menu of available network interfaces. The `clear` command is used to clear the screen. The `echo` command is then used to display the menu options.
* **Line 21:** Read the user's choice. The `read` command is used to read the user's choice from the keyboard.
* **Lines 23-39:** Perform the selected action. The `case` statement is used to perform the selected action. The `ifconfig` command is used to activate or deactivate a network interface. The `ip addr show` command is used to display the IP address of a network interface.
* **Line 41:** Display the new IP address. The `ip addr show` command is used to display the IP address of the selected network interface. The `grep` command is then used to filter the output of the `ip addr show` command and extract the IP address.

The script is designed to be easy to use and understand. It is also designed to be flexible, allowing the user to select from a variety of network interfaces.