import unittest
from tweet_analyser import analyse_tweet


class TweetsTestCase(unittest.TestCase):
    def test_random_pos_tweets(self):
        tweets = ['The film was super', 'I am so happy to see you', 'It was okay']
        predictions = [analyse_tweet(tweet) for tweet in tweets]
        self.assertEqual(predictions, [4, 4, 4])

    def test_random_neg_tweets(self):
        tweets = ['I hate you', 'I love thrillers, but this film sucks',
                  'You look terrible.']
        predictions = [analyse_tweet(tweet) for tweet in tweets]
        self.assertEqual(predictions, [0, 0, 0])


if __name__ == '__main__':
    unittest.main()
