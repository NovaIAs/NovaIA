```perl
use strict;
use warnings;
use Data::Dumper;
use List::MoreUtils qw(uniq);
use JSON;
use DBI;

my $config = JSON->new->decode(<<JSON);
{
    "database": {
        "driver": "mysql",
        "host": "localhost",
        "database": "mydb",
        "username": "root",
        "password": ""
    },
    "tables": {
        "users": {
            "columns": {
                "id": {
                    "type": "int",
                    "primary_key": true
                },
                "name": {
                    "type": "varchar(255)"
                },
                "email": {
                    "type": "varchar(255)",
                    "unique": true
                },
                "password": {
                    "type": "varchar(255)"
                },
                "created_at": {
                    "type": "timestamp",
                    "default": "CURRENT_TIMESTAMP"
                },
                "updated_at": {
                    "type": "timestamp",
                    "default": "CURRENT_TIMESTAMP",
                    "on_update": "CURRENT_TIMESTAMP"
                }
            }
        },
        "posts": {
            "columns": {
                "id": {
                    "type": "int",
                    "primary_key": true
                },
                "user_id": {
                    "type": "int",
                    "foreign_key": {
                        "table": "users",
                        "column": "id"
                    }
                },
                "title": {
                    "type": "varchar(255)"
                },
                "content": {
                    "type": "text"
                },
                "created_at": {
                    "type": "timestamp",
                    "default": "CURRENT_TIMESTAMP"
                },
                "updated_at": {
                    "type": "timestamp",
                    "default": "CURRENT_TIMESTAMP",
                    "on_update": "CURRENT_TIMESTAMP"
                }
            }
        },
        "comments": {
            "columns": {
                "id": {
                    "type": "int",
                    "primary_key": true
                },
                "post_id": {
                    "type": "int",
                    "foreign_key": {
                        "table": "posts",
                        "column": "id"
                    }
                },
                "user_id": {
                    "type": "int",
                    "foreign_key": {
                        "table": "users",
                        "column": "id"
                    }
                },
                "content": {
                    "type": "text"
                },
                "created_at": {
                    "type": "timestamp",
                    "default": "CURRENT_TIMESTAMP"
                },
                "updated_at": {
                    "type": "timestamp",
                    "default": "CURRENT_TIMESTAMP",
                    "on_update": "CURRENT_TIMESTAMP"
                }
            }
        }
    }
}
JSON;

my $dbh = DBI->connect(
    "$config->{database}{driver}:database=$config->{database}{database};host=$config->{database}{host}",
    $config->{database}{username},
    $config->{database}{password}
);

my @tables = keys %{$config->{tables}};
foreach my $table (@tables) {
    my $columns = $config->{tables}{$table}{columns};
    my @columns = keys %{$columns};

    my $sql = "CREATE TABLE IF NOT EXISTS `$table` (";
    foreach my $column (@columns) {
        my $column_config = $columns->{$column};
        $sql .= "`$column` $column_config->{type}";

        if ($column_config->{primary_key}) {
            $sql .= " PRIMARY KEY";
        }

        if ($column_config->{unique}) {
            $sql .= " UNIQUE";
        }

        if ($column_config->{foreign_key}) {