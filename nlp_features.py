import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
import sklearn

train_file =  "train.json"
test_file = "test.json"
train_df = pd.read_json(train_file)
test_df = pd.read_json(test_file)

train_text = train_df["description"].tolist()
train_text += test_df["description"].tolist()

from nltk.corpus import stopwords
stop = list(stopwords.words('english'))
stop.append("kagglemanager")
stop.append("renthop")
stop.append("com")
stop.append("br")
stop.append("email")
stop.append("closets")
stop.append("floors")
stop.append("york")


tfidf = TfidfVectorizer(stop_words=stop,ngram_range=(1,2),lowercase=True,max_df = 0.70, min_df = 0.1, use_idf = True, max_features = 100)

tfs = tfidf.fit_transform(train_text)
feature_names = tfidf.get_feature_names()

# import cPickle as pickle
# pickle.dump(feature_names, open('feature_names.pkl', 'wb'))

cv = sklearn.feature_extraction.text.CountVectorizer(vocabulary=feature_names)
res = cv.fit_transform(train_text).toarray()

for i in range(0, 100):
	print i, feature_names[i]
# print res

import csv
with open("count_vec.csv", "wb") as f:
    writer = csv.writer(f)
    writer.writerows(res)