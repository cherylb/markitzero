# -*- coding: utf-8 -*-
"""
Created on Sat Jul 26 22:55:38 2014

@author: Cheryl
"""
from sqlite3 import dbapi2 as sqlite
con = sqlite.connect("C:/SQLlight/toys.sqlite")
curser = con.cursor()

#Select records from database
curser.execute('''SELECT brand,toyname, irritation_score 
FROM brands JOIN toyinfo ON brands.id = toyinfo.brand_id''')

# print out field names
fldnames =[f[0] for f in curser.description]
print fldnames
#print out info
for row in curser:
    print row
    



