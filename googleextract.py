import requests
from bs4 import BeautifulSoup
import csv

filename = "number_of_searches_2.csv"
f = open(filename, "w", encoding = 'UTF-8')
f.write("Artist,Number of Searches\n")
def pyGoogleSearch(word):
	address = 'http://www.google.com/search?q='
	newword = address + word
	page = requests.get(newword)
	soup = BeautifulSoup(page.content, 'html.parser')
	phrase_extract = soup.find(id="resultStats")
	# phrase_text = phrase_extract.text
	# val = phrase_extract.text.split(' ')[1].replace(',','')
	# return val
	try:
		phrase_text = phrase_extract.text
		val = phrase_extract.text.split(' ')[1].replace(',','')
		return val
	except AttributeError:
		return("check")

with open("song_info.csv", encoding = 'UTF-8') as csvfile:
	readCSV = csv.reader(csvfile, delimiter = ',')
	for i in range(1362):
		readCSV.__next__()

	for row in readCSV:
		word = row[1]
		val = pyGoogleSearch(word)
		print(word,val)
		f.write('"{}","{}"'.format(word,val))

f.close()

