import nbd_geocode
import csv


with open("town_train.csv", "w") as g:

	town_train = csv.writer(g)

	with open("neighborhood_train.csv", "r") as f:
		nbd_train = csv.reader(f)

		for i in nbd_train:
			listing = i[0]
			nbd = i[1]

			if nbd in nbd_geocode.m_nbd:
				town_train.writerow([listing, "manhattan"])

			if nbd in nbd_geocode.b_nbd:
				town_train.writerow([listing, "boston"])

			if nbd in nbd_geocode.q_nbd:
				town_train.writerow([listing, "queens"])

			if nbd in nbd_geocode.bx_nbd:
				town_train.writerow([listing, "bronx"])

			if nbd in nbd_geocode.j_nbd:
				town_train.writerow([listing, "jersey"])

			if nbd in nbd_geocode.s_nbd:
				town_train.writerow([listing, "stratten"])

			if nbd in nbd_geocode.college_nbd:
				town_train.writerow([listing, "college"])

with open("town_test.csv", "w") as g:

	town_test = csv.writer(g)

	with open("neighborhood_test.csv", "r") as f:
		nbd_test = csv.reader(f)

		for i in nbd_test:
			listing = i[0]
			nbd = i[1]

			if nbd in nbd_geocode.m_nbd:
				town_test.writerow([listing, "manhattan"])

			if nbd in nbd_geocode.b_nbd:
				town_test.writerow([listing, "boston"])

			if nbd in nbd_geocode.q_nbd:
				town_test.writerow([listing, "queens"])

			if nbd in nbd_geocode.bx_nbd:
				town_test.writerow([listing, "bronx"])

			if nbd in nbd_geocode.j_nbd:
				town_test.writerow([listing, "jersey"])

			if nbd in nbd_geocode.s_nbd:
				town_test.writerow([listing, "stratten"])

			if nbd in nbd_geocode.college_nbd:
				town_test.writerow([listing, "college"])
