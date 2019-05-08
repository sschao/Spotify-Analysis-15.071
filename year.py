import csv
import os


# filename = "release_date.csv"
# f = open(filename, "w", encoding = 'UTF-8')

filename = "release_date_try.csv"
g = open(filename, "w", encoding = 'UTF-8')
f = csv.writer(g)
f.writerow(["Song", "Release Year"])
row_count = 1
with open("release_dates.csv", encoding = 'UTF-8', errors ="ignore") as csvfile:
	readCSV = csv.reader(csvfile, delimiter = ',')
	readCSV.__next__()

	for row in readCSV:
		year = ""
		song = row[0]
		if song != "":
			for i in range(1950,2020):
				if str(i) in row[0]:
					year = str(i)
					break
			if year != "":
				row_count +=1
				print([row_count,song,year])
				f.writerow([song,year])
				continue
		if row[2] != "" and len(row[2])==4:
			year = row[2]

		elif row[2] != "":
			for i in range(1950,2020):
				if str(i) in row[2]:
					year = str(i)
					break
		elif len(row[1])==4:
			year = row[1]
		elif row[1] == "my_url" or row[1]=="check url (no wikipedia)":
			year = ""

		else:
			for i in range(1950,2020):
				if str(i) in row[1]:
					year = str(i)
					break
			if len(year)!=4:
				for i in range(0,100):
					if "-" + str(i) in row[1]:
						if i>19:
							year = "19" + row[1][-2:]
						else:
							year = "20" + row[1][-2:]
		row_count +=1
		print([row_count,song,year])
		f.writerow([song,year])
				
g.close()

f1 = open("release_date_try.csv", "rt", encoding = 'UTF-8')
f2 = open("release_date.csv", "wt", encoding = 'UTF-8')
for line in f1.readlines():
    if ''.join(line.split(',')).strip() == '':
        continue
    f2.write(line)

f1.close()
f2.close()

os.remove("release_date_try.csv")