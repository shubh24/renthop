import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer

train_file =  "train.json"
test_file = "test.json"
train_df = pd.read_json(train_file)
test_df = pd.read_json(test_file)

train_text = train_df["description"].tolist()
train_text += test_df["description"].tolist()

tfidf = TfidfVectorizer(stop_words=u'english',ngram_range=(1,2),lowercase=True,max_df = 0.80, min_df = 0.1, use_idf = True, max_features = 100)

tfs = tfidf.fit_transform(train_text)
feature_names = tfidf.get_feature_names()

import cPickle as pickle
pickle.dump(feature_names, open('feature_names.pkl', 'wb'))

print feature_names