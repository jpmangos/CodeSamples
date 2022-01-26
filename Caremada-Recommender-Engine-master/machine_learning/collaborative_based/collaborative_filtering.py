from pandas import DataFrame, Series
import numpy as np


class CollaborativeBased:
    def __init__(self, user_profile_movie_data, user_id):
        if not isinstance(user_profile_movie_data, DataFrame) or not isinstance(user_id, int):
            raise TypeError('Invalid argument set; expected (int, DataFrame)')
        self.user_profile_movie_data = user_profile_movie_data
        self.user_id = user_id
        self.__recommended_results = []


    def get_recommended_results(self):
        return self.__recommended_results

    def generate_recommended_results(self):
        table = self.user_profile_movie_data.pivot_table(index="Users", columns="Movie", values="Rating")
        if self.user_id not in table.index or len(table.loc[self.user_id].dropna()) < 5:
            return False

        table = table.fillna(0)
        pivot_table = table.values
        user_rating = np.mean(pivot_table, axis=1).reshape(-1, 1)
        pt_demeaned = pivot_table - user_rating

        u, s, vt = np.linalg.svd(pt_demeaned, full_matrices=False)
        sigma = np.diag(s)
        all_user_predicted_rating = np.dot(np.dot(u, sigma), vt) + user_rating

        predication_table = DataFrame(all_user_predicted_rating, columns=table.columns)
        sorted_user_prediction = predication_table.iloc[self.user_id].sort_values(ascending=False)
        sorted_user_prediction = sorted_user_prediction.to_frame("rating")
        filter_data = self.user_profile_movie_data.merge(sorted_user_prediction, how="outer", left_on="Movie",
                                                         right_on="Movie").sort_values("rating", ascending=False)
        top10 = Series(filter_data["Movie"]).unique()[5:16]
        self.movie_listing(top10)
        return True

    def movie_listing(self, top_10):
        from archive.driver import get_datagram

        path_movies_csv = "..\\data"
        file_movies_csv = "sample_movie_dataset.csv"
        data_frame = get_datagram(path_movies_csv, file_movies_csv)
        movie_index=[]
        for movie in top_10:
            movie_index = data_frame[data_frame.Title == movie].index[0]
            self.__recommended_results.append(data_frame.iloc[movie_index][1:])
