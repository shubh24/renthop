from pygeocoder import Geocoder
import csv

def get_coordinates(location):
	return Geocoder.geocode(location).coordinates

m_nbd = [
"Battery Park City",
"Bowery",
"Chinatown",
"Civic Center",
"East Village",
"Financial District",
"Greenwich Village",
"Little Italy",
"Lower East Side",
"NoHo",
"NoLita",
"SoHo",
"Tribeca",
"Two Bridges",
"West Village",
"Chelsea",
"Flatiron District",
"Garment District",
"Gramercy Park",
"Hell\'s Kitchen",
"Kips Bay",
"Koreatown",
"Midtown East",
"Murray Hill",
"NoMad",
"Stuyvesant Town - Peter Cooper Village",
"Theater District",
"Central Harlem",
"Central Park",
"East Harlem",
"Inwood",
"Upper East Side",
"Upper West Side",
"Washington Heights",
"West Harlem",
"Other",
"Randalls-Wards Island",
"Roosevelt Island"]

b_nbd = [
"Bedford-Stuyvesant",
"Bushwick",
"Greenpoint",
"Williamsburg",
"Boerum Hill",
"Carroll Gardens",
"Cobble Hill",
"Gowanus",
"Greenwood Heights",
"Park Slope",
"Prospect Park",
"Red Hook",
"Sunset Park",
"Windsor Terrace",
"Crown Heights",
"East Flatbush",
"Flatbush",
"Kensington",
"Midwood",
"Ocean Hill",
"Brooklyn Heights",
"Brooklyn Navy Yard",
"Clinton Hill",
"DUMBO",
"Downtown Brooklyn",
"Fort Greene",
"Prospect Heights",
"Vinegar Hill",
"Bath Beach",
"Bay Ridge",
"Bensonhurst",
"Borough Park",
"Dyker Heights",
"Mapleton",
"Southern Brooklyn",
"Brighton Beach",
"Coney Island",
"Gravesend",
"Sheepshead Bay",
"Brownsville",
"Canarsie",
"Cypress Hills",
"East New York",
"Bergen Beach",
"Flatlands",
"Floyd Bennett Airfield",
"Marine Park",
"Mill Basin"]

q_nbd = [
"Astoria",
"Corona",
"East Elmhurst",
"Elmhurst",
"Forest Hills",
"Glendale",
"Jackson Heights",
"Long Island City",
"Maspeth",
"Middle Village",
"Rego Park",
"Ridgewood",
"Sunnyside",
"Woodside",
"Auburndale",
"Bayside",
"College Point",
"Flushing",
"Flushing Meadows-Corona Park",
"Fresh Meadows",
"Glen Oaks",
"Kew Gardens",
"Kew Gardens Hills",
"Whitestone",
"Briarwood",
"Hollis",
"Holliswood",
"Jamaica",
"Jamaica Estates",
"Jamaica Hills",
"South Jamaica",
"St. Albans",
"Forest Park",
"Howard Beach",
"Ozone Park",
"Richmond Hill",
"South Ozone Park",
"Woodhaven",
"Far Rockaway",
"Rockaway Beach"]

bx_nbd = [ 
"Bedford Park",
"Belmont",
"Bronx Park",
"Concourse",
"Concourse Village",
"East Tremont",
"Fordham Heights",
"Fordham Manor",
"Highbridge",
"Hunts Point",
"Kingsbridge",
"Longwood",
"Marble Hill",
"Morris Heights",
"Morrisania",
"Mott Haven",
"Mount Eden",
"Mount Hope",
"Norwood",
"Riverdale",
"University Heights",
"Van Cortlandt Park",
"West Farms",
"Allerton",
"Clason Point",
"Morris Park",
"Parkchester",
"Pelham Bay",
"Pelham Parkway",
"Throgs Neck",
"Unionport",
"Van Nest",
"Wakefield",
"Westchester Village",
"Williamsbridge",
"Woodlawn Heights"]

j_nbd = [
"Bergen - Lafayette",
"Greenville",
"Historic Downtown",
"McGinley Square",
"The Heights",
"The Waterfront",
"West Side"]

s_nbd = [
"East Shore",
"Mid-Island",
"North Shore",
"South Shore",
]

if __name__ == '__main__':

	with open("coded_locs.csv", "w") as f:
		writer = csv.writer(f, delimiter = ",")
		# writer.writerow(["nbd", "lat", "lon"])

		for i in m_nbd:
			coords = get_coordinates(i + ", manhattan")
			lat, lon = coords
			print i, lat, lon
			writer.writerow([i, lat, lon])

		for i in b_nbd:
			coords = get_coordinates(i + ", brooklyn")
			lat, lon = coords
			print i, lat, lon
			writer.writerow([i, lat, lon])

		for i in q_nbd:
			coords = get_coordinates(i + ", queens")
			lat, lon = coords
			print i, lat, lon
			writer.writerow([i, lat, lon])

		for i in bx_nbd:
			coords = get_coordinates(i + ", bronx")
			lat, lon = coords
			print i, lat, lon
			writer.writerow([i, lat, lon])

		for i in j_nbd:
			coords = get_coordinates(i + ", new jersey")
			lat, lon = coords
			print i, lat, lon
			writer.writerow([i, lat, lon])

		for i in s_nbd:
			coords = get_coordinates(i + ", staten island")
			lat, lon = coords
			print i, lat, lon
			writer.writerow([i, lat, lon])