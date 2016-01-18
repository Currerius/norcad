import json
from pprint import pprint

data = json.load( open('analyses.json'))

pprint(data)

data["Endpoints"][0]["Comment"]

