import csv

stack1 = []

print("stack file 1?")
stack_file_1 = raw_input() + ".csv"

print("stack file 2?")
stack_file_2 = raw_input() + ".csv"

print("stack ensemble number?")
ensemble_file = "stack_ensemble_" + raw_input() + ".csv"

with open(stack_file_1, "r") as f:
	f_reader = csv.reader(f)

	for r in f_reader:
		stack1.append(r)

stack2 = []

with open(stack_file_2, "r") as f:
	f_reader = csv.reader(f)

	for r in f_reader:
		stack2.append(r)

res = []
for i in range(1, len(stack1), 1):
	listing = stack1[i][0]

	med = (float(stack1[i][3]) + float(stack2[i][3]))/2
	low = (float(stack1[i][2]) + float(stack2[i][2]))/2
	high = (float(stack1[i][1]) + float(stack2[i][1]))/2

	res.append([listing, low, med, high])

with open(ensemble_file, "w") as f:
	f_writer = csv.writer(f)
	
	f_writer.writerow(["listing_id", "low", "medium", "high"])

	f_writer.writerows(res)