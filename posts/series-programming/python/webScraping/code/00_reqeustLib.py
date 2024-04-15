#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 22 18:21:11 2021

@author: Edoardo
"""

# Import Request library
import requests

# Make a request
r = requests.get('https://api.github.com/events')
type(r)
    # "/events" is called an endpoint and determines what aspect we are
    # retriving

# Make a GET request to get the latest position of the ISS from the 
# OpenNotify API
response = requests.get("http://api.open-notify.org/iss-now.json")
response_fail = requests.get("http://api.open-notify.org/ixsas-now.json")

# Check type of repsonse
type(response)

# What attributes are reachable from this object?
print(dir(response))

# Check success or failure of your request
response.status_code
response_fail.status_code

## Passing Arguments
# Some Endpoints will need to pass parameters to the requests.get function
# For example, the ISS Pass endpoint which tells us when the ISS passess over 
# a given location, requires two parameters: latitude and longitude.

# Define latitude and longitude of location of interest (New York City)
parameters = {"lat": 40.71, "lon": -74} 
type(parameters) # should be a DICTIONARY
parameters["lat"]

# Make a get request with the parameters.
response = requests.get("http://api.open-notify.org/iss-pass.json", 
                        params = parameters)

# Print the content of the response (the data the server returned)
print(response.content)
content = response.content
type(content)

## Lists and dictionaries to JSON

# Impot JSON library
import json

# Consider a list
bf_chains = ["Taco Bell", "Shake Shack", "Chipotle"]
type(bf_chains) # it's a list!

# json.dumps: turns a list into a string
bf_chains_string = json.dumps(bf_chains)
type(bf_chains_string)

# json.loads: turns a string into a list
bf_chains_list = json.loads(bf_chains_string)
type(bf_chains_list)

# Or consider the dictionary
fast_food_franchise = {
    "Subway": 24722,
    "McDonalds": 14098,
    "Starbucks": 10821,
    "Pizza Hut": 7600
}
type(fast_food_franchise)

# Dumpt it to string
fff_string = json.dumps(fast_food_franchise)
type(fff_string) # now it's a string

fff_dict = json.loads(fff_string)
type(fff_dict) # back to being a dictionary!

# You can get a reponse as a python object directly:
json_data = response.json()
type(json_data)
type(json_data["response"])
dtype(json_data["response"][0])
first_pass_duration = json_data["response"][0]["duration"]

## Response Headers (meta data)

response.headers["content-type"] 
    # Content-Type is the most important of this infomration

## How many people are in sapce right now:
    
pis = requests.get("http://api.open-notify.org/astros.json")
pis_data = pis.json()
in_space_count = pis_data["number"]
