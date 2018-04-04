import sys

def mapper_sf1()
	for line in sys.stdin:
		data = line.strip().split(",")
		if len(data) == 6:
			attr, date, amount = data
			company, attr = attr[0:2]
			print "({0},{1})\t{2}".format(company, attr, cost)
