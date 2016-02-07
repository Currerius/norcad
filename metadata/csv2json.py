#!/usr/bin/python

 
import json
import pprint
import requests
import couchdb # python2 only

# initialize an empty json file with leading '{'
open("MetaData.json","w").write("{\n")
open("MetaData.json","a").write("  \"Fields\": [")

with open("MetaData.json","a") as a:
  for l in open( "Kravspek_NORCAD_CSV.csv" , "r" ):
    a.write( "    {\n" )
    a.write( "      \"Table\": \"" + l.split(";")[7] + "\" ,\n")
    a.write( "      \"Name\": \"" + l.split(";")[1] + "\" ,\n")
    a.write( "      \"ShortLabel\":\"\" ,\n" )
    a.write( "      \"Label\":\"\" ,\n" )
    a.write( "      \"Values\": [] ,\n" )
    a.write( "      \"ValueLabels\": [] ,\n" )
    a.write( "      \"Type\": \"" + l.split(";")[8].replace('\r\n','') + "\" ,\n")
    a.write( "      \"Unit\":\"\"\n" )
    a.write( "    } ,\n\n" )

# set the closing '}'   
open("MetaData.json","a").write("  ]\n}\n")



open("tmp.json","w").write("Hello")

cols = []
for l in file:
  cols.append(l)


  cols.append(l.split(";"))


def awk_it(instring,index,delimiter=" "):
  try:
    return [instring,instring.split(delimiter)[index-1]][max(0,min(1,index))]
  except:
    return "" 


print "
    {
      \"Name\":\"\" ,
      \"ShortLabel\":\"\" ,
      \"Label\":\"\" ,
      \"Values\": []
      \"ValueLabels\": [] ,
      \"Type\":\"\" ,
      \"Unit\":\"\"
    } ,
"
