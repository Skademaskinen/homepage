import sys
import os
import subprocess
import json

print("migrating db...")

tables = [
    "guestbook",
    "snake",
    "users",
    "valid_tokens",
    "visits"
]

for table in tables:
    columns = [(item["name"], item["type"]) for item in json.loads(subprocess.check_output(["sqlite3", sys.argv[-1], f"PRAGMA table_info({table})", "-json"]).decode())]
    #print(columns)
    data = subprocess.check_output(["sqlite3", sys.argv[-1], f"SELECT * FROM {table}", "-json"]).decode()
    if data:
        rows = json.loads(data)
        #print(rows)
        rows_formatted = [", ".join([f"'{row[column]}'" if column_type == "VARCHAR" else f"{row[column]}" for column, column_type in columns]) for row in rows]
        query = f"insert into homepage.{table} values {',\n'.join([f'({row})' for row in rows_formatted])}"
        with open("/tmp/query.sql", "w") as file:
            file.write(query)
        print(query)
        os.system("mariadb -u homepage < /tmp/query.sql")