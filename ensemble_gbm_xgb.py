import csv

gbm = []

with open("gbm_30.csv", "r") as f:
	f_reader = csv.reader(f)

	for r in f_reader:
		gbm.append(r)

xgb = []

with open("xgb_2.csv", "r") as f:
	f_reader = csv.reader(f)

	for r in f_reader:
		xgb.append(r)

res = []
for i in range(1, len(gbm), 1):
	listing = gbm[i][0]

	low = (float(gbm[i][3]) + float(xgb[i][1]))/2
	med = (float(gbm[i][2]) + float(xgb[i][2]))/2
	high = (float(gbm[i][1]) + float(xgb[i][3]))/2

	res.append([listing, low, med, high])

with open("gbm_xgb.csv", "w") as f:
	f_writer = csv.writer(f)
	
	f_writer.writerow(["listing_id", "low", "medium", "high"])

	f_writer.writerows(res)