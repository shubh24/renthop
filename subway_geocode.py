from pygeocoder import Geocoder
import csv

def get_coordinates(location):
	return Geocoder.geocode(location).coordinates


if __name__ == '__main__':

	stations = []

	with open("subways.csv", "r") as f:
		reader = csv.reader(f)

		for row in reader:
			if row[0] == "":
				curr_station = prev_station
			else:
				curr_station = row[0]
			stations.append(curr_station + ", " + row[1])

			prev_station = curr_station

	with open("subways_coords.csv", "w") as f:
	
		writer = csv.writer(f, delimiter = ",")

		for i in stations:
			coords = get_coordinates(i)
			lat, lon = coords
			print i, lat, lon
			writer.writerow([i, lat, lon])

