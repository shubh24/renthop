import json
import urllib2
import pandas as pd
import csv

import reverse_geocoder as rg

# def get_geonames(lat, lng, types):
#     url = 'http://maps.googleapis.com/maps/api/geocode/json' + \
#             '?latlng={},{}&sensor=false'.format(lat, lng)
#     jsondata = json.load(urllib2.urlopen(url))

#     address_comps = jsondata['results'][0]['address_components']

#     filter_method = lambda x: len(set(x['types']).intersection(types))
#     return filter(filter_method, address_comps)

# types = ['neighborhood']

train_file =  "train.json"
test_file = "test.json"
train_df = pd.read_json(train_file)
test_df = pd.read_json(test_file)

train_lat = train_df[["listing_id", "latitude", "longitude"]]
test_lat = test_df[["listing_id", "latitude", "longitude"]]

lat_lon = []
listings = []
for i, j in train_lat.iterrows():
	lat_lon.append((j["latitude"], j["longitude"]))
	listings.append(j["listing_id"])

results = rg.search(lat_lon)
nbd_train = [[listings[i], results[i]['name']] for i in range(0, len(listings))]

with open("neighborhood_train.csv", "wb") as f:

    writer = csv.writer(f, delimiter = ",")

    writer.writerow(["listing_id", "neighborhood"])
    writer.writerows(nbd_train)

lat_lon = []
listings = []
for i, j in test_lat.iterrows():
	lat_lon.append((j["latitude"], j["longitude"]))
	listings.append(j["listing_id"])

results = rg.search(lat_lon)
nbd_test = [[listings[i], results[i]['name']] for i in range(0, len(listings))]

with open("neighborhood_test.csv", "wb") as f:

    writer = csv.writer(f, delimiter = ",")

    writer.writerow(["listing_id", "neighborhood"])
    writer.writerows(nbd_test)