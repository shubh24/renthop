import json
import urllib2
import pandas as pd

def get_geonames(lat, lng, types):
    url = 'http://maps.googleapis.com/maps/api/geocode/json' + \
            '?latlng={},{}&sensor=false'.format(lat, lng)
    jsondata = json.load(urllib2.urlopen(url))

    address_comps = jsondata['results'][0]['address_components']

    filter_method = lambda x: len(set(x['types']).intersection(types))
    return filter(filter_method, address_comps)

types = ['neighborhood']

train_file =  "train.json"
test_file = "test.json"
train_df = pd.read_json(train_file)
test_df = pd.read_json(test_file)

train_lat = train_df["latitude"].tolist()
train_lat += test_df["latitude"].tolist()

train_lon = train_df["longitude"].tolist()
train_lon += test_df["longitude"].tolist()

res = []
import csv

with open("neighborhood.csv", "wb") as f:

    writer = csv.writer(f)
    writer.writerow(["neighborhood"])

    for i in range(0, len(train_lat)): 
        
        lat = train_lat[i]
        lon = train_lon[i]
                        
        geoname = get_geonames(lat, lon, types)[0]

        common_types = set(geoname['types']).intersection(set(types))
        # res.append(geoname['long_name'])

        print geoname["long_name"]
        writer.writerow(geoname["long_name"])        