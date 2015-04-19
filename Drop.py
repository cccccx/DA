import os
import sys
import csv
import re
import string


i=0
j=0
sum_a=0
df1=[]
sum_x=0

with open('r1.csv') as f:
    r1 = csv.reader(f)
    headers = next(r1)
#df = pd.read_csv('r1.csv', parse_dates=True)
#headers = next(r1)
    for row in r1:
      for i in range(3,138):
        sum_a=sum_a+(int(row[i]))
      if sum_a!=0:
 	    df1.append(row)
      sum_a=0

del_col =[]
for j in range (3,138):
  for i in range (0,len(df1)):
  
    sum_x = sum_x +(int(df1[i][j]))
    #print sum_x
  if sum_x == 0:
    print j
    del_col.append(j)

  sum_x =0
    #data_arr.remove(i)
x=0
y=0
del_col.reverse();
for x in range(0,len(df1)):
  for y in del_col:
    del df1[x][y]


for y in del_col:
    del headers[y]
    df_file = file('r2.csv','wb')
writer = csv.writer(df_file)
writer.writerow(headers)
for item in df1:
 writer.writerow(item)
df_file.close()
