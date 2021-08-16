import pickle
import sklearn


def analyse_tweet(tweet):
    if type(tweet) != list:
        tweet = [tweet]
    loaded_model = pickle.load(open('../models/logistic_model.sav', 'rb'))
    loaded_vectorizer = pickle.load(open('../models/vectorizer.sav', 'rb'))
    tweets_transformed = loaded_vectorizer.transform(tweet)
    prediction = loaded_model.predict(tweets_transformed)
    return prediction
