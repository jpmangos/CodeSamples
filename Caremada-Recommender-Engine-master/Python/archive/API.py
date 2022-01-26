import sys
from pymongo import MongoClient
from machine_learning.content_based.content_based import ContentBased
from pandas import DataFrame

def connectMongoDBLocal():
    pass


if __name__ == "__main__":

    print([
        {"key1": "value1"},
        {"key2": "value2"},
        {"key3": "value3"},
        {"key4": "value4"}, 
    ])

    # client = MongoClient('mongodb://localhost:27017/')
    # # client = MongoClient('mongodb+srv://admin-alex:caremada6@cluster0.5inmr.mongodb.net/caremadaDB?retryWrites=true&w=majority')
    # db = client['caremadaDB']
    # collection = []
    # filter_list = []

    # key = sys.argv[2]
    # prim_key = sys.argv[3]

    # if sys.argv[4] == "movies":
    #     collection = db['movies']
    #     filter_list = [prim_key, "Title", "Genre", "Director", "Actors"]
    # else:
    #     collection = db['caregivers']
    #     filter_list = [prim_key, "Name", "Occupation", "Services", "Availability", "Location"]

    # result = collection.find()
    # df = DataFrame(list(result))
    # cb = ContentBased(key, df, filter_list).filter()

    # for e in cb[:10]:
    #     print(e.to_json())































# import csv
# import json
# import pandas as pd
# from pymongo import MongoClient
#
# csvfile = open('C:\\Users\\Alex\\OneDrive\\Semester 6\\Indust App Dev\\Software_Project\\Python\\data\\sample_movie_dataset.csv')
# reader = csv.DictReader(csvfile)
# client = MongoClient('mongodb://localhost:27017/')
#
# db = client['moviesDB']
# collection = db['movies']
# header = ['Rank', 'Title', 'Genre', 'Description', 'Director', 'Actors', 'Year', 'Runtime (Minutes)', 'Rating', 'Votes', 'Revenue (Millions)', 'Metascore']
#
# for each in reader:
#     row = {}
#     for field in header:
#         row[field] = each[field]
#
#     collection.insert_one(row)
