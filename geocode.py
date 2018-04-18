import thread
import csv
import requests
import json
import time

config = json.load(open('config.json'))
with open(config["input_file"], 'rb') as csvfile:
	r = csv.DictReader(csvfile, delimiter=',', quotechar='|')
	i = -1
	l = []
	complete = 0
	for row in r:
		i += 1
		if i == 0:
			continue
		l.append([row[config["address_column"]]])
	
	def geocode(i):
		global complete
		url = "https://developers.onemap.sg/commonapi/search?returnGeom=y&getAddrDetails=n&searchVal=" + l[i][0] + "&pageNum=1"
		print "Geocoding " + l[i][0]
		
		result = json.loads(requests.get(url).text)
		
		if result["found"] == 0:
			l[i].append(config["failed_geocode_output"])
			l[i].append(config["failed_geocode_output"])
			complete += 1
			print "Geocoded " + l[i][0] + " to " + l[i][1] + ", " + l[i][2]
			return
		
		l[i].append(result["results"][0]['LATITUDE'])
		l[i].append(result["results"][0]['LONGITUDE'])
	
		print "Geocoded " + l[i][0] + " to " + l[i][1] + ", " + l[i][2]
		complete += 1
	for i in range(len(l)):
		thread.start_new_thread(geocode, (i, ))
		time.sleep(0.1)

	while complete < len(l):
		pass
	
	o = open(config['output_file'], 'w')
	o.write(config["address_column"] + ",lat,lon\n")
	for loc in l:
		o.write(str(loc[0]) + "," + str(loc[1]) + "," + str(loc[2]) + "\n")
	
	o.close()
