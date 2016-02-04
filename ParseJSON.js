#!/usr/bin/node


 
import json
import pprint
import requests
import couchdb # python2 only

## python3
# with open('survival.json', encoding='utf-8') as data_file:
#     survJSON = json.loads(data_file.read())
## or
# survJSON = json.loads(open('survival.json',encoding='utf-8').read())

survJSON = json.loads(open('survival.json').read())

Events = []
for var in survJSON["Endpoints"]:
  Events.append( var["Event"] )

EventDBaliases = []
for i in range(len(Events)):
  EventDBaliases.append( Events[i]+" AS A"+`i`)


Times = []
for var in survJSON["Endpoints"]:
  Times.append( var["Time"] )

TimeDBaliases = []
for i in range(len(Times)):
  TimeDBaliases.append( Times[i]+" AS A"+`i`)



SQLselect = "SELECT "
for var in EventDBaliases:
  SQLselect += (var + ", ")
for var in TimeDBaliases:
  SQLselect += (var + ", ")




Times = []
for var in survJSON["Endpoints"]:  Times.append( var["Time"] )

Predictors = []
for var in survJSON["Predictors"]:  Predictors.append( var["Name"] )

Modifiers = []
for var in survJSON["Modifiers"]:  Modifiers.append( var["Name"] )

Adjustments = []
for var in survJSON["Adjustments"]:  Adjustments.append( var["Name"] )

sqllist = Events + Times + Predictors + Modifiers + Adjustments



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
