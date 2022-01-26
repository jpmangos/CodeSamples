import socket
import threading
import json
import atexit
from pymongo import MongoClient
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

# client = MongoClient('mongodb+srv://admin-alex:caremada6@cluster0.5inmr.mongodb.net/caremadaDB?retryWrites=true&w=majority')
client = MongoClient('mongodb://localhost:27017')
db = client['caremadaDB']
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

    print('abc')
    try:
        algorithm_t = json_msg['algorithm_t']
        table_name = json_msg['tableName']
        pkey_column_name = json_msg['pkey_column_name']
        pkey_val = json_msg['pkey_val']

        if table_name == "movies":
            collection = db['movies']
            filter_list = ["Title", "Genre", "Director", "Actors"]
        elif table_name == "caregivers":
            collection = db['caregivers']
            filter_list = ["Name", "Occupation", "Services", "Availability", "Location"]

        dataset = list(collection.find())
        df = DataFrame(dataset)

        cb = ContentBased(pkey_column_name, pkey_val, df, filter_list)
        recommendations = cb.get_recommendations(10)

        conn.send(json.dumps(recommendations, default=str).encode(FORMAT))
        conn.close()
    except:
        conn.send(json.dumps([], default=str).encode(FORMAT))
        conn.close()


def clean_up():
    print('web_API.py exiting...')

start()
