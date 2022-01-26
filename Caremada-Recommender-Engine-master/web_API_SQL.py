import socket
import threading
import json
import atexit
import mysql.connector as sql_connector
from machine_learning.content_based.content_based import ContentBased
from pandas import DataFrame


PORT = 5050
HEADER = 4096  # BITS FOR THE MESSAGE
# SERVER = socket.gethostbyname(socket.gethostname())
SERVER = "127.0.0.1"
ADDR = (SERVER, PORT)
server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server.bind(ADDR)
FORMAT = 'utf-8'

# Create a DB connection
sql_connection = sql_connector.connect(user='user', password='pass', database='caremada')

# Intialize a cursor to run SQL commands
mycursor = sql_connection.cursor()


collection = None
filter_list = None


def start():
    atexit.register(clean_up)
    server.listen()
    print(f"[LISTENING] Server is listening on {SERVER}")
    while True:
        conn, addr = server.accept()
        thread = threading.Thread(target=handle_client, args=(conn, addr))
        thread.start()
        print(f"[ACTIVE CONNECTION] {threading.active_count() - 1}")


def handle_client(conn, addr):
    # print(f"[NEW CONNECTION] {addr} connected.")
    msg = conn.recv(HEADER).decode(FORMAT)
    json_msg = json.loads(msg)

    try:
        algorithm_t = json_msg['algorithm_t']
        table_name = json_msg['tableName']
        pkey_column_name = json_msg['pkey_column_name']
        pkey_val = json_msg['pkey_val']


        # dataset = list(collection.find())
        # df = DataFrame(dataset)

        filter_list = ["official_occupation", "travel_radius"]
        mycursor.execute("SELECT * FROM caregiver")

        myresult = mycursor.fetchall()
        mylist = myresult
        mydf = DataFrame(mylist)

        mycursor.execute("SELECT column_name FROM information_schema.columns WHERE table_name='caregiver';")
        mycolumnnames = mycursor.fetchall()
        my_col_list = list(mycolumnnames)
        formatted_names = [''.join(i) for i in my_col_list] 
        mydf.columns = formatted_names
        print(mydf)

        cb = ContentBased(pkey_column_name, pkey_val, mydf, filter_list)
        recommendations = cb.get_recommendations(10)

        conn.send(json.dumps(recommendations, default=str).encode(FORMAT))
        conn.close()
    except:
        conn.send(json.dumps([], default=str).encode(FORMAT))
        conn.close()


def clean_up():
    print('web_API.py exiting...')

start()
