import csv
import os


f1 = open("release_date_0_1.csv", "rt", encoding = 'UTF-8')
f2 = open("release_date_no_space.csv", "wt", encoding = 'UTF-8')
for line in f1.readlines():
    if ''.join(line.split(',')).strip() == '':
        continue
    f2.write(line)

f1.close()
f2.close()

os.remove("release_date_0_1.csv")