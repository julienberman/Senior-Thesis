# Data handling and processing
import os
import re
import time
import pandas as pd
import numpy as np
import statistics
import json
import csv
import sys
from datetime import datetime
from typing import List, Tuple, NamedTuple, Set, Dict, Any, Union, Optional
from pathlib import Path

# Scraping
import requests

# Plotting
import matplotlib as plt
from ratelimit import limits, sleep_and_retry

# store FEC API key and other constants
API_KEY = "A5BcxD84ZnaDzIBnKCqWR5ZGfFl56F4sWIgaRIGO"
BASE_URL = 'https://api.open.fec.gov/v1'
CALLS_PER_MINUTE = 120
LOOKUP = {
    ("allen waters", "senate"): "S0RI00067",
    ("andrew horning", "senate"): "S2IN00109",
    ("albert gore", "president"): "P80000912",
    ("erik gerhardt", "senate"): "S2PA00430",
    ("gary swing", "senate"): "S6AZ00340",
    ("james sceniak", "senate"): "S2IN00307",
    ("jo perkins", "senate"): "S4OR00156",
    ("john james", "senate"): "S8MI00372",
    ("kevin o'connor", "senate"): "S0MA00232",
    ("roger marshall", "senate"): "S0KS00315",
    ("ron johnson", "senate"): "S0WI00197",
    ("scott brown", "senate"): "S0MA00109",
    ("bill bledsoe", "senate"): "S6SC04155",
    ("bill cassidy", "senate"): "S4LA00107",
    ("bill redmond", "senate"): "S8NM00135",
    ("bob casey", "senate"): "S6PA00217",
    ("bob conley", "senate"): "S8SC00126",
    ("bob corker", "senate"): "S6TN00216",
    ("bob franks", "senate"): "S8NJ00335",
    ("bob kerrey", "senate"): "S8NE00067",
    ("bob mcdermott", "senate"): "S2HI00155",
    ("bob schaffer", "senate"): "S4CO00171",
    ("charlie melancon", "senate"): "S0LA00154",
    ("chellie pingree", "senate"): "S0ME00038",
    ("chuck hagel", "senate"): "S6NE00087",
    ("dean barkley", "senate"): "S4MN00262",
    ("dick mountjoy", "senate"): "S6CA00501",
    ("dorris haddock", "senate"): "S4NH00054",
    ("earl nelson", "senate"): "S6NE00095",
    ("edward pipkin", "senate"): "S4MD00152",
    ("frank mongiardo", "senate"): "S4KY00059",
    ("helen myers", "senate"): "S4OH00168",
    ("herbert kohl", "senate"): "S6WI00061",
    ("jack carter", "senate"): "S6NV00150",
    ("jack mcmullen", "senate"): "S8VT00059",
    ("jack reed", "senate"): "S6RI00163",
    ("jim barksdale", "senate"): "S6GA00200",
    ("jim durkin", "senate"): "S2IL00127",
    ("jim fulner", "senate"): "S4MI00371",
    ("jim huffman", "senate"): "S0OR00220",
    ("jim jenkins", "senate"): "S4NE00124",
    ("jim martin", "senate"): "S8GA00164",
    ("jim renacci", "senate"): "S8OH00102",
    ("jim risch", "senate"): "S8ID00092",
    ("jim webb", "senate"): "S6VA00127",
    ("joe kyrillos", "senate"): "S2NJ00387",
    ("joe miller", "senate"): "S0AK00121",
    ("joe o'dea", "senate"): "S2CO00241",
    ("joe pinion", "senate"): "S2NY00499",
    ("joe sestak", "senate"): "S0PA00434",
    ("johnny isakson", "senate"): "S6GA00119",
    ("katie mcginty", "senate"): "S6PA00266",
    ("kenneth chase", "senate"): "S6MA00221",
    ("kit bond", "senate"): "S6MO00289",
    ("lee jones", "senate"): "S4KS00085",
    ("len britton", "senate"): "S0VT00114",
    ("maggie hassan", "senate"): "S6NH00091",
    ("mike espy", "senate"): "S8MS00287",
    ("mike johanns", "senate"): "S8NE00117",
    ("mike liffrig", "senate"): "S4ND00053",
    ("mike mcfadden", "senate"): "S4MN00346",
    ("mike mcgavick", "senate"): "S6WA00248",
    ("mike taylor", "senate"): "S2MT00054",
    ("ned lamont", "senate"): "S6CT05066",
    ("peter domenici", "senate"): "S8NM00010",
    ("preston love", "senate"): "S4NE00223",
    ("r. van dam", "senate"): "S4UT00134",
    ("raymond clatworthy", "senate"): "S6DE00073",
    ("richard lion", "senate"): "S6CT05124",
    ("rick berg", "senate"): "S2ND00073",
    ("rick santorum", "senate"): "S4PA00063",
    ("rick weiland", "senate"): "S4SD00056",
    ("robert arlett", "senate"): "S8DE00145",
    ("robert clement", "senate"): "S2TN00066",
    ("scotty boman", "senate"): "S8MI00315",
    ("ted budd", "senate"): "S2NC00505",
    ("teresa keane", "senate"): "S4OR00123",
    ("tim chestnut", "senate"): "S2WY00109",
    ("timothy johnson", "senate"): "S6SD00051",
    ("tom allen", "senate"): "S8ME00080",
    ("tom coburn", "senate"): "S4OK00174",
    ("tom cotton", "senate"): "S4AR00103",
    ("tom harkin", "senate"): "S4IA00020",
    ("tom kean", "senate"): "S6NJ00271",
    ("tom strickland", "senate"): "S6CO00135",
    ("tony campbell", "senate"): "S8MD00328",
    ("w. cochran", "senate"): "S8MS00055",
    ("w. romney", "president"): "P80003353"
}

# get FEC IDs
@sleep_and_retry
@limits(calls=CALLS_PER_MINUTE, period=60)
def get_id(api_key, name, office, year):
    """
    Fetch candidate ID and office from search query
    """
    if (name, office) in LOOKUP:
        return LOOKUP[(name, office)], "n"
    
    office = office[0].upper()
    params = {
        'api_key': api_key,
        "q": name,
        'office': office        
    }
    url = f'{BASE_URL}/candidates'
    response = requests.get(url, params=params)
    
    if response.status_code != 200:
        print(f"Error fetching information for candidate: {name}")
        print(f"Response content: {response.text}")
        return None, response.text
    
    data = response.json()
    results = data.get("results", [])
    
    # only include results that include given election year
    filtered = [
        result for result in results 
        if year in result.get('election_years', [])
    ]
    
    if not filtered:
        return None, "n"
        
    has_multiple = "y" if len(filtered) > 1 else "n"
    return filtered[0]["candidate_id"], has_multiple

def main():
    # read csv
    df_returns = pd.read_csv("temp/data_clean/returns.csv", keep_default_na=False)
    
    # set up search query column
    df_returns["name_first_last"] = df_returns["name_first"] + " " + df_returns["name_last"]

    # collapse to the candidate / office / year level
    candidates = df_returns.groupby(['name_first_last', 'office', 'year']).size().reset_index(name='count')
    candidates['fecid'] = None
    candidates['has_multiple'] = None
    federal_candidates = candidates[candidates["office"] != "governor"]

    # iterate over each row in dataframe
    results = []
    count = 1
    for idx, row in federal_candidates.iterrows():
        office = row["office"]
        name = row["name_first_last"]
        year = row['year']
        print(f"Processing ---- {count} ---- {name}")
        # query API
        result = get_id(API_KEY, name, office, year)
        results.append(result)
        count += 1
        time.sleep(0.5)
    
    # store the results as columns in the collapsed dataframe
    federal_candidates['fecid'], federal_candidates['has_multiple'] = zip(*results)
    
    # merge the collapsed dataframe back into the full dataset of election returns
    merged = pd.merge(left=df_returns, right=federal_candidates, how="left", on=["name_first_last", "office", "year"])

    # clean the merged dataframe
    merged = merged.drop(['name_first_last', 'count', 'has_multiple'], axis=1)
    merged = merged[['fecid'] + [col for col in merged.columns if col != 'fecid']]

    # save to csv
    print("Saving...")
    merged.to_csv("temp/data_clean/candidates.csv", index = False)
    
if __name__ == "__main__":
    main()