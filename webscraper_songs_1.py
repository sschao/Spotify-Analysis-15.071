from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from pandas.io.html import read_html
import csv

driver = webdriver.Chrome()
filename = "release_date_1.csv"
f = open(filename, "w", encoding = 'UTF-8')
f.write("Song, Release Date\n")

def Google_webclick(song, artist):
	search_term = song + " " + artist + " wikipedia"
	driver.get('http://www.google.com/search?q=' + search_term)
	results = driver.find_elements_by_xpath('//div[@class="r"]/a/h3')
	results[0].click()
	my_url = driver.current_url
	return my_url
	driver.close()



def get_date(my_url):
	try:
		infoboxes = infoboxes = read_html(my_url, index_col=0, attrs={"class":"infobox vevent"})
		df = infoboxes[0][1]
		weird_dates = df["Released"]
		weird_date = weird_dates.replace('\xa0',' ') #dates appear weird
		for i in range(10):
			date = weird_date.replace('['+str(i)+']','') #footnotes
		return(date)
	except:
		try:
			infoboxes = read_html(my_url, index_col=0, attrs={"class":"infobox vevent haudio"})
			df = infoboxes[0][1]
			weird_dates = df["Released"]
			date = weird_dates.replace('\xa0',' ') #dates appear weird
			for a in range(10):
				date = date.replace('['+str(a)+']','') #footnotes
			return(date)
		except:
			return("my_url")


with open("song_info.csv", encoding = 'UTF-8') as csvfile:
	readCSV = csv.reader(csvfile, delimiter = ',')
	for i in range(9000):
		readCSV.__next__()
	for row in readCSV:
		artist = row[1]
		song = row[0]
		my_url = Google_webclick(song, artist)
		if "en.wikipedia.org" not in my_url:
			date = "check url (no wikipedia)"
		else:
			try:
				date = str(get_date(my_url))
			except:
				date = "check wikipedia url"
		print([song, date])
		f.write(song+ "," + date + "\n")

f.close()