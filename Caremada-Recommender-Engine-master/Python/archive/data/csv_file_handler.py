import os
import pandas as pd


class CsvFileHandler:
    def __init__(self, path, file_name):
        if not isinstance(path, str) or not isinstance(file_name, str):
            raise TypeError('Invalid parameter set; expected (str, str)')
        self.file_path = os.path.join(path, file_name)
        if not os.path.exists(self.file_path) or not file_name.endswith('.csv'):
            raise ValueError(f'file \'{file_name}\' could not be located using path \'{path}\'')
        self.__data_frame = None

    """Accessor method for the private field __data_frame"""
    def get_data_frame(self):
        if self.__data_frame is None:
            self.extract_data()
        return self.__data_frame

    """Reads the content_based of the CSV file and transform it into a DataFrame object which is stored in the __data_frame field.
    The DataFrame object is represented as a two-dimensional data structure (dictionary where key = str and value = list)
    """
    def extract_data(self):
        self.__data_frame = pd.read_csv(self.file_path)
        if self.__data_frame is None:
            raise RuntimeError(f"Failed to read from from CSV file using path: {self.file_path}")
        self.__data_frame.fillna(0, inplace=True)
