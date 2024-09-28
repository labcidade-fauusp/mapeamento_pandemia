import csv
from unicodedata import normalize
import pandas as pd

file_form = r"C:\Users\Desktop\Documents\Projetos\Vacinação RMSP 20out2021\Vac_RMSP_20211020_{}.csv"

filesSP = [
	r"C:\Users\Desktop\Downloads\part-00000-3eefe978-a0ca-45b0-ad56-f011ab414a74.c000.csv",
	r"C:\Users\Desktop\Downloads\part-00001-3eefe978-a0ca-45b0-ad56-f011ab414a74.c000.csv",
	r"C:\Users\Desktop\Downloads\part-00002-3eefe978-a0ca-45b0-ad56-f011ab414a74.c000.csv"
]



RMSP = [
	'350390',
	'350570',
	'350660',
	'350900',
	'350920',
	'351060',
	'351300',
	'351380',
	'351500',
	'351510',
	'351570',
	'351630',
	'351640',
	'351830',
	'351880',
	'352220',
	'352250',
	'352310',
	'352500',
	'352620',
	'352850',
	'352940',
	'353060',
	'353440',
	'353910',
	'353980',
	'354330',
	'354410',
	'354500',
	'354680',
	'354730',
	'354780',
	'354870',
	'354880',
	'354995',
	'355030',
	'355250',
	'355280',
	'355645'
	]

file_out = file_form.format('fullDB')
count = 0
total = 0


try:
	for fileSP in filesSP:
		with open(fileSP, 'r', encoding='utf-8') as arquivo:
			db = csv.reader(arquivo, delimiter =';')
			
			for entry in db:
				line = [normalize('NFKD',x).strip() for x in entry[2:]]
				total+=1
				if count==0:
					with open(file_out,'w',newline='',encoding='utf-8') as saida:
						writer = csv.writer(saida,delimiter='	')
						writer.writerow(line)
						count += 1
				if line[5] in RMSP:
					with open(file_out,'a',newline='',encoding='utf-8') as saida:
						writer = csv.writer(saida,delimiter='	')
						writer.writerow(line)
						count += 1
					texto = str(count) + ' de ' + str(total) +' linhas registradas...'
					print(texto)

except Exception as e:
	print(str(e))
finally:
	input('Filtragem finalizada')