from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import ElementNotVisibleException
from selenium.webdriver.common.by import By
from pandas.io.html import read_html
import csv
import time


driver = webdriver.Chrome()
filename = "release_date_0_1.csv"
g = open(filename, "w", encoding = 'UTF-8')
f = csv.writer(g)
#f = open(filename, "w", encoding = 'UTF-8')
#f.write("Song, Release Date\n")
f.writerow(["Song","Release Date"])

def Google_webclick(song, artist):
	search_term = song + " " + artist + " wikipedia"
	driver.get('http://www.google.com/search?q=' + search_term)
	driver.implicitly_wait(5)
	results = driver.find_elements_by_xpath('//div[@class="r"]/a/h3')
	try:
		results[0].click()
	except IndexError:
		return("Index Error")
	except ElementNotVisibleException:
		return ("Really Check what is going on")

	my_url = driver.current_url
	return my_url
	driver.close()



def get_date(my_url):
	try:
		infoboxes = read_html(my_url, index_col=0, attrs={"class":"infobox vevent"})
		df = infoboxes[0][1]
		weird_dates = df["Released"]
		date = weird_dates.replace('\xa0',' ') #dates appear weird
		for a in range(10):
			date = date.replace('['+str(a)+']','') #footnotes
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

'''
modified row 3413 in song_info because Python cant read &
'''

count = 1
with open("song_info.csv", encoding = 'UTF-8') as csvfile:
	readCSV = csv.reader(csvfile, delimiter = ',')
	for i in range(count): #skip this number of rows
		readCSV.__next__()
	for row in readCSV:
		artist = row[1]
		song = row[0]
		my_url = Google_webclick(song, artist)
		time.sleep(0.3)
		if "en.wikipedia.org" not in my_url:
			date = "check url (no wikipedia)"
		else:
			try:
				date = str(get_date(my_url))
			except:
				date = "check wikipedia url"
		print([count, song + " " + artist + " wikipedia", date])
		#f.write('"{}","{}"'.format(song,date))
		f.writerow([song, date])
		count += 1


g.close()



#driver.quit()