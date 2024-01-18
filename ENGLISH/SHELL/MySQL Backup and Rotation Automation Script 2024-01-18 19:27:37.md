```shell script
### Shell Script to Automate MySQL Backups and Rotations

# Define Database Credentials
DB_HOST="localhost"
DB_USER="root"
DB_PASS="password"
DB_NAME="mydb"

# Backup Directory
BACKUP_DIR="/var/backups/mysql"
mkdir -p "$BACKUP_DIR"

# Date and Time Variables
NOW=$(date +"%Y-%m-%d_%H-%M")
TODAY=$(date +"%Y-%m-%d")

# Create a New Backup
mysqldump --host="$DB_HOST" --user="$DB_USER" --password="$DB_PASS" --databases "$DB_NAME" > "$BACKUP_DIR/$DB_NAME-$NOW.sql"

# Compress the Backup
gzip "$BACKUP_DIR/$DB_NAME-$NOW.sql"

# Remove Backups Older than 7 Days
find "$BACKUP_DIR" -type f -name "$DB_NAME-*.sql.gz" -mtime +7 -exec rm -f {} \;

# Keep the Last 5 Backups
find "$BACKUP_DIR" -type f -name "$DB_NAME-*.sql.gz" | sort -nr | tail -n +6 | xargs rm -f

# Create a Backup Summary Report
echo "MySQL Backup Summary Report" > "$BACKUP_DIR/$TODAY-report.txt"
echo "--------------------------------" >> "$BACKUP_DIR/$TODAY-report.txt"
echo "Backup Date: $TODAY" >> "$BACKUP_DIR/$TODAY-report.txt"
echo "Database Name: $DB_NAME" >> "$BACKUP_DIR/$TODAY-report.txt"
echo "Number of Backups: $(ls -1 "$BACKUP_DIR" | wc -l)" >> "$BACKUP_DIR/$TODAY-report.txt"
echo "Latest Backup: $(ls -1 "$BACKUP_DIR" | head -n 1)" >> "$BACKUP_DIR/$TODAY-report.txt"

# Email the Backup Summary Report
echo "Subject: MySQL Backup Summary Report - $TODAY" | mail -s - "recipient@example.com" < "$BACKUP_DIR/$TODAY-report.txt"

echo "MySQL backup and rotation completed successfully."

Explanation:
This complex shell script automates regular MySQL database backups and rotations. Let's break down the code:

1.	Define Database Credentials: This section contains the variables for the database host, user, password, and name.

2.	Backup Directory: It creates a directory to store the backups.

3.	Date and Time Variables: These variables capture the current date and time in specific formats.

4.	Create a New Backup: The 'mysqldump' command is used to create a raw SQL dump of the database. It's then compressed using 'gzip'.

5.	Remove Backups Older than 7 Days: This command removes backups older than 7 days.

6.	Keep the Last 5 Backups: This command ensures that only the most recent 5 backups are kept.

7.	Create a Backup Summary Report: A summary report of the backups is created, including backup date, database name, and other details.

8.	Email the Backup Summary Report: It emails the backup summary report to a specified recipient.

9.	Completion Message: The script displays a message to indicate successful backup and rotation.

With this script, MySQL backups and rotations are automated, providing a reliable solution for database backups and rotations.
```