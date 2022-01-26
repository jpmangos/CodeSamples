from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from pandas import Series, DataFrame
from bson import ObjectId

"""A class that handles data and sorts by text similarity using cosine similarity.

    This classes purposes is to sort the data based on cosine similarity
    and provide utility functions to be used by the API. It also contains methods to generate 
    statistical information.

    Typical usage example:
    cb = ContentBased("itemID", databaseTable, [featureOne, featureTwo, etc,])
    RecommendationList = cb.getRecommendation(10)
"""


class ContentBased:
    """Content-based classed to process the data, and provide recommendations. Uses Cosine Similarity.

    This class takes intializes the the class atrributes with the provided data then calls
    it's own method sort_frame() to process and sort the Dataframe data. The data is is maintained as a dataframe sorted by
    cosine similarity with the lowest indexs being the most similar.

    Args:
        selected_key(ObjectId): The key which to generate recommendations for.
        df(df/list): Dataframe to process.
        selected_features(string): Features on which to execute text similarity algorithms.
        pkey_col: Primary column to index from.

    Atrributes:
        Identical to Args.

    """

    def __init__(self, pkey_col, selected_key, df, selected_features):
        """Class intializer. Calls the sort_frame() function."""
        if not isinstance(pkey_col, str) or not isinstance(selected_key, str) or not isinstance(df, DataFrame) or not \
                (selected_features and isinstance(selected_features, list)):
            raise TypeError('Invalid parameter set; expected: str, str, DataFrame, list')
        self.selected_key = selected_key
        self.selected_features = selected_features
        self.sorted_df = df
        self.pkey_col = pkey_col
        self.sort_frame()

    def sort_frame(self):
        """Intialization function that sort the incoming DataFrame by cosine similarity.

        Uses the sklearn functions to apply cosine similarity to the selected key, then adds to the dataframe and
        sorts by that column, and sorts in-place.

        Args:
            None.

        Atrributes:
            recommendation_index(ObjectId): The id of selected_feature.
            cv: The sklearn CountVectorizer Class.
            cos_similarity: sklearn's kernal matrix.
            score_series: A pandas series containing all the cosine similarity values for the selected key.

        Returns:
            None.
        """
        recommendation_index = None

        for key, index in zip(self.sorted_df[self.pkey_col], self.sorted_df.index):
            if str(key) == self.selected_key:
                recommendation_index = index

        self.sorted_df['combined_features'] = self.sorted_df[self.selected_features].apply(lambda x: str(x), axis=1)

        # vectorize and process cosine similarity and append to dataframe
        cv = CountVectorizer()
        count_matrix = cv.fit_transform(self.sorted_df["combined_features"])
        cos_similarity = cosine_similarity(count_matrix)
        score_series = Series(cos_similarity[recommendation_index])
        self.sorted_df['Cosine Similarity'] = score_series
        self.sorted_df.sort_values(by=['Cosine Similarity'], inplace=True, ascending=False)

    def get_recommendations(self, num_of_rec, sort_method=None):
        """Provides recommendations

        This function performs operations on the sorted DataFrame provide a list of recommendations. Contains sorting features.

        Args:
            num_of_rec(int): How many recommendation to return.
            sort_method(string): Enables sorting by a secondary parameter. Must be one of the columns in the existing sorted_df.

        Atrributes:
            df_for_api(DataFrame): The local dataFrame to perform operations on.

        Returns:
            dictionary: Formatted dictionary containing the recommendations and associated values.
        """
        df_for_api = self.sorted_df

        if sort_method is not None:
            df_for_api.sort_values(by=['Cosine Similarity', sort_method], inplace=True, ascending=False)

        # Drop columns for aesthetics
        df_for_api.drop(['combined_features'], axis=1, inplace=True)
        # df_for_api.drop(['Cosine Similarity'], axis=1, inplace=True)
        df_for_api.drop([self.pkey_col], axis=1, inplace=True)  # hide primary key from the recommendations

        return df_for_api.iloc[1:num_of_rec + 1].fillna('').to_dict('records')
