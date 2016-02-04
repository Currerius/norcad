#!/usr/bin/python

 
import json
import pprint
import requests
import couchdb # python2 only

survJSON = json.loads(open('survival.json').read())


Events = []
for var in survJSON["Endpoints"]:
  Events.append( var["Event"] )

EventDBaliases = []
for i in range(len(Events)):
  EventDBaliases.append( Events[i]+" AS E"+`i`)



Times = []
for var in survJSON["Endpoints"]:
  Times.append( var["Time"] )

TimeDBaliases = []
for i in range(len(Times)):
  TimeDBaliases.append( Times[i]+" AS T"+`i`)


Predictors = []
for var in survJSON["Predictors"]:
  Predictors.append( var["Name"] )

PredictorDBaliases = []
for i in range(len(Predictors)):
  PredictorDBaliases.append( Predictors[i]+" AS P"+`i`)



Modifiers = []
for var in survJSON["Modifiers"]:
  Modifiers.append( var["Name"] )

ModifierDBaliases = []
for i in range(len(Modifiers)):
  ModifierDBaliases.append( Modifiers[i]+" AS M"+`i`)



Adjustments = []
for var in survJSON["Adjustments"]:
  Adjustments.append( var["Name"] )

AdjustmentDBaliases = []
for i in range(len(Adjustments)):
  AdjustmentDBaliases.append( Adjustments[i]+" AS A"+`i`)



SQLselect = "SELECT "

for var in EventDBaliases:
  SQLselect += (var + ", ")

for var in TimeDBaliases:
  SQLselect += (var + ", ")

for var in PredictorDBaliases:
  SQLselect += (var + ", ")

for var in AdjustmentDBaliases:
  SQLselect += (var + ", ")

for var in ModifierDBaliases:
  SQLselect += (var + ", ")

SQLselect = SQLselect[:-2]



SQLwhere = "WHERE "
for var in survJSON["Population"]["Subset"]:
  SQLwhere += var["Where"]+" AND "

SQLwhere = SQLwhere[:-4]


DBstatement = SQLselect + " FROM PATIENT_INFO P LEFT JOIN LAB_BLOOD_TOTAL B ON P.FORUS_ID=B.FORUS_ID LEFT JOIN hs_medikament_skjema M ON P.FORUS_ID=M.FORUS_ID LEFT JOIN hs_hendelser_CVDNOR_mars14 C ON P.NORCAD_ID=C.NORCAD_ID" + SQLwhere + ";"


# since localhost is default shorten it
db = couchdb.Server()['norcad']

# insert JSON into the db:
db.save(survJSON)


# To find all your documents, simply iterate over the database:

for docs in db: print docs

# Delete a document with a certain id:
# db.delete(db['705e0cf322b35689ed118dec110008fa'])


## check norcad CouchDB in browser visit:
# http://localhost:5984/_utils/database.html?norcad
