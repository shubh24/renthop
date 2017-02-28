import pandas as pd
import csv
import reverse_geocoder as rg
from geopy.distance import great_circle

train_file =  "train.json"
test_file = "test.json"
train_df = pd.read_json(train_file)
test_df = pd.read_json(test_file)

train_coords = train_df[["listing_id", "latitude", "longitude"]]
test_coords = test_df[["listing_id", "latitude", "longitude"]]

coded_locs = {}
with open("coded_locs.csv", "r") as f:
    reader = csv.reader(f)
    for row in reader:
        coded_locs[row[0]] = (row[1], row[2])

coded_stations = {}
with open("subways_coords.csv", "r") as f:
    reader = csv.reader(f)
    for row in reader:
        coded_stations[row[0]] = (row[1], row[2])

def go_do(train_var):
    
    lat_lon = []
    listings = []

    if train_var == 1:
        df = train_coords
        out_file = "neighborhood_train.csv"        
    else:
        df = test_coords
        out_file = "neighborhood_test.csv"

    for i, j in df.iterrows():
        lat_lon.append((j["latitude"], j["longitude"]))
        listings.append(int(j["listing_id"]))

    results = rg.search(lat_lon)
    nbd = [[listings[i], results[i]['name']] for i in range(0, len(listings))]

    with open(out_file, "wb") as f:

        writer = csv.writer(f, delimiter = ",")

        writer.writerow(["listing_id", "neighborhood"])
        writer.writerows(nbd)

def coded_locs_dist(train_var):

    results = []

    if train_var == 1:
        df = train_coords
        out_file = "neighborhood_train.csv"        
    else:
        df = test_coords
        out_file = "neighborhood_test.csv"

    for i, j in df.iterrows():
        lat_lon = (j["latitude"], j["longitude"])

        min_dist = 32768

        for loc in coded_locs:
            dist = great_circle(lat_lon, coded_locs[loc]).km

            if dist < min_dist:
                min_dist = dist
                closest_loc = loc

        results.append([int(j["listing_id"]), closest_loc])

    with open(out_file, "wb") as f:
        writer = csv.writer(f, delimiter = ",")
        writer.writerow(["listing_id", "neighborhood"])
        writer.writerows(results)

def dist_from_station(train_var):

    results = []

    if train_var == 1:
        df = train_coords
        out_file = "subway_train.csv"        
    else:
        df = test_coords
        out_file = "subway_test.csv"

    for i, j in df.iterrows():
        lat_lon = (j["latitude"], j["longitude"])
        min_dist = 32768

        for loc in coded_locs:
            dist = great_circle(lat_lon, coded_locs[loc]).km

            if dist < min_dist:
                min_dist = dist

        results.append([int(j["listing_id"]), min_dist])

    with open(out_file, "wb") as f:
        writer = csv.writer(f, delimiter = ",")
        writer.writerow(["listing_id", "distance"])
        writer.writerows(results)


if __name__ == '__main__':
    # go_do(0)
    # go_do(1)
    coded_locs_dist(0)
    coded_locs_dist(1)    
    # dist_from_station(1)
    # dist_from_station(0)