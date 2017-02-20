import pandas as pd
import csv
import reverse_geocoder as rg

train_file =  "train.json"
test_file = "test.json"
train_df = pd.read_json(train_file)
test_df = pd.read_json(test_file)

train_coords = train_df[["listing_id", "latitude", "longitude"]]
test_coords = test_df[["listing_id", "latitude", "longitude"]]

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

if __name__ == '__main__':
    go_do(0)
    go_do(1)