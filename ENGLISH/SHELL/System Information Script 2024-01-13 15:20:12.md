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

# Define functions
function print_error() {
  echo -e "${RED}$1${NC}"
}

function print_success() {
  echo -e "${GREEN}$1${NC}"
}

function print_warning() {
  echo -e "${YELLOW}$1${NC}"
}

function print_info() {
  echo -e "${BLUE}$1${NC}"
}

function print_debug() {
  echo -e "${CYAN}$1${NC}"
}

# Check if the user is root
if [[ $EUID -ne 0 ]]; then
  print_error "This script must be run as root."
  exit 1
fi

# Check if the necessary commands are installed
for command in bc df du free iptables netstat ping traceroute; do
  if ! command -v $command &> /dev/null; then
    print_error "The command '$command' is not installed."
    exit 1
  fi
done

# Get the system information
system_info=$(uname -a)
kernel_version=$(uname -r)
cpu_model=$(cat /proc/cpuinfo | grep "model name" | cut -d: -f2 | uniq)
cpu_cores=$(grep -c ^processor /proc/cpuinfo)
memory_total=$(free -m | grep Mem: | awk '{print $2}')
memory_used=$(free -m | grep Mem: | awk '{print $3}')
memory_free=$(free -m | grep Mem: | awk '{print $4}')
swap_total=$(free -m | grep Swap: | awk '{print $2}')
swap_used=$(free -m | grep Swap: | awk '{print $3}')
swap_free=$(free -m | grep Swap: | awk '{print $4}')
disk_usage=$(df -h / | grep /dev/ | awk '{print $5}')
disk_total=$(df -h / | grep /dev/ | awk '{print $2}')
disk_used=$(df -h / | grep /dev/ | awk '{print $3}')
disk_free=$(df -h / | grep /dev/ | awk '{print $4}')
network_interfaces=$(ip addr | grep -o '^[0-9]\+:.*$' | cut -d: -f2 | tr -d ' ')
ip_address=$(ip addr | grep -o 'inet [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+' | head -n1 | cut -d' ' -f2)
gateway=$(ip route | grep default | awk '{print $3}')
dns_servers=$(grep -o 'nameserver [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+' /etc/resolv.conf | cut -d' ' -f2)
open_ports=$(netstat -tln | grep LISTEN | awk '{print $4}' | cut -d: -f2 | sort -n | uniq)
active_connections=$(netstat -an | grep ESTABLISHED | wc -l)
running_processes=$(ps -A | wc -l)
uptime=$(uptime | awk '{print $3,$4}')
load_average=$(uptime | awk '{print $8,$9,$10}')

# Print the system information
clear
echo -e "${GREEN}System Information${NC}"
echo -e "----------------------------"
echo -e "System: ${system_info}"
echo -e "Kernel Version: ${kernel_version}"
echo -e "CPU Model: ${cpu_model}"
echo -e "CPU Cores: ${cpu_cores}"
echo -e "Memory Total: ${memory_total}MB"
echo -e "Memory Used: ${memory_used}MB"
echo -e "Memory Free: ${memory_free}MB"
echo -e "Swap Total: ${swap_total}MB"
echo -e "Swap Used: ${swap_used}MB"
echo -e "Swap Free: ${swap_free}MB"
echo -e "Disk Usage: ${disk_usage}"
echo -e "Disk Total: ${disk_total}"
echo -e "Disk Used: ${disk_used}"
echo -e "Disk Free: ${disk_free}"
echo -e "Network Interfaces: ${network_interfaces}"
echo -e "IP Address: ${ip_address}"
echo -e "Gateway: ${gateway}"
echo -e "DNS Servers: ${dns_servers}"
echo -e "Open Ports: ${open_ports}"
echo -e "Active Connections: ${active_connections}"
echo -e "Running Processes: ${running_processes}"
echo -e "Uptime: ${uptime}"
echo -e "Load Average: ${load_average}"

# Exit the script
exit 0
```

This script is a system information script that prints out a lot of useful information about your system. It includes information about the kernel version, CPU model, CPU cores, memory usage, swap usage, disk usage, network interfaces, IP address, gateway, DNS servers, open ports, active connections, running processes, uptime, and load average.

The script is written in bash and uses a variety of commands to gather the information, including uname, free, df, du, ip, netstat, ping, traceroute, and ps.

The script is well-commented and easy to follow. It also uses a variety of colors to make the output more readable.

To use the script, simply save it to a file and make it executable. Then, run the script with the following command:

```
./system_info.sh
```

The script will then print out the system information to the console.