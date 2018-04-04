import sys

mod_cap_finance_stocks=[]

def mapper_sf1():
	for line in sys.stdin:
		data = line.strip().split(",")
		if len(data) == 3:
			
			attr, date, amount = data
			ticker, attr = attr.split("_")[0:2]
			if ticker in mod_cap_finance_stocks:
				print "{0}\t{1}\t{2}\t{3}".format(date,ticker, attr, cost)

def tokenize(row):
	return row.split(",")

def parse_attr(row):
	ticker, attr = row[0].split("_")[0:2]
	return (row[1],ticker,attr,row[2])
					 
