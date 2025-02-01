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
import requests
import matplotlib as plt

ACCESS_TOKEN = "EAAMtZAjRm38kBOw1NxlyEZBjEobgdZC7n6YnbxO36VdddKaIoozDOZBIoWw0ffMLKrZAXS3PUddZBwNl1mBDjiEZAd9oG4QZAqriEddrmzroMF5evRKrX3WYBHAHIvLdbAl1BbV45wvjYJ9IvMige8PX9zZBZBK0EHbhpRnuCHVHoWL828lPZBnGi9ol1medQw8IhyufZCtuTkP6S1Ph9LM34BghMUH4A1fhfsA1Lqt7yLIrZCcMZD"
BASE_URL = "https://graph.facebook.com/v21.0/ads_archive/"

def get_ads(id, access_token):
    all_data = []
    params = {
        'access_token': access_token,
        'ad_type': 'POLITICAL_AND_ISSUE_ADS',
        'search_page_ids': id,
        'fields': 'id,page_name,ad_creative_bodies,ad_delivery_start_time,ad_delivery_stop_time,impressions,spend,demographic_distribution,languages,publisher_platforms,bylines',
        'ad_reached_countries': 'US',
        'unmask_removed_content': 'true',
        'limit': 100
    }
    
    try:
        while True:
            response = requests.get('https://graph.facebook.com/v21.0/ads_archive/', params=params)
            data = response.json()
            
            if 'data' in data:
                all_data.extend(data['data'])
                
            if 'paging' in data and 'next' in data['paging'] and 'after' in data['paging']['cursors']:
                params['after'] = data['paging']['cursors']['after']
            else:
                break

    except Exception as e:
        print(f"Error fetching ads for page {id}: {str(e)}")
        return None
        
    return {'data': all_data}


def process_candidate(name, page_id):
    print(f"Processing ads for.....{name}.....(Page ID: {page_id})")
    ads_data = []
    data = get_ads(page_id, ACCESS_TOKEN)
    if not data or 'data' not in data:
        print("Ads not found")
        return []
    print("Data: ", data)
    for ad in data['data']:
        print("in for loop")
        imp_data = ad.get('impressions', {'lower_bound': '0', 'upper_bound': '0'})
        spend_data = ad.get('spend', {'lower_bound': '0', 'upper_bound': '0'})
        impressions = (int(imp_data.get('lower_bound', 0)) + int(imp_data.get('upper_bound', 0)) + 1) / 2
        cost = (int(spend_data.get('lower_bound', 0)) + int(spend_data.get('upper_bound', 0)) + 1) / 2

        entry = {
            'name': name,
            'page_id': str(page_id),
            'ad_id': ad.get('id', ''),
            'start_date': datetime.strptime(ad.get('ad_delivery_start_time', '2000-01-01'), '%Y-%m-%d').strftime("%Y-%m-%d") if ad.get('ad_delivery_start_time') else None,
            'end_date': datetime.strptime(ad.get('ad_delivery_stop_time', '2000-01-01'), '%Y-%m-%d').strftime("%Y-%m-%d") if ad.get('ad_delivery_stop_time') else None,
            'impressions': int(impressions),
            'cost': int(cost),
            'language': ad.get('languages', [''])[0],
            'is_facebook': 1 if 'facebook' in ad.get('publisher_platforms', []) else 0,
            'is_instagram': 1 if 'instagram' in ad.get('publisher_platforms', []) else 0,
            'byline': ad.get('bylines', '').lower(),
            'creative': ad.get('ad_creative_bodies', [''])[0] if ad.get('ad_creative_bodies') else ''
        }

        for gender in ['male', 'female', 'unknown']:
            for age_group in ['18-24', '25-34', '35-44', '45-54', '55-64', '65+']:
                entry[f'share_{gender}_{age_group}'] = None

        for demo in ad.get('demographic_distribution', []):
            gender = demo.get('gender', '').lower()
            age = demo.get('age', '')
            if gender and age:
                entry[f'share_{gender}_{age}'] = float(demo.get('percentage', 0))
        print(entry)
        ads_data.append(entry)
    return ads_data

def get_processed_page_ids():
    try:
        # Read existing CSV if it exists
        existing_df = pd.read_csv("temp/data_clean/facebook_ads.csv")
        return set(existing_df['page_id'].unique())
    except (FileNotFoundError, pd.errors.EmptyDataError):
        return set()

def main():
    df = pd.read_excel('temp/data_raw/facebook_profiles.xlsx')
    processed_page_ids = get_processed_page_ids()
    csv_path = Path("temp/data_clean/facebook_ads.csv")

    try:
        # Process all candidates
        for _, row in df.iterrows():
            name = row['name_first_last']
            
            if pd.notna(row['page_id']):
                page_ids = [row['page_id']]
                if pd.notna(row.get('page_id_2')):
                    page_ids.append(row['page_id_2'])
                
                candidate_ads = []
                for page_id in page_ids:
                    if page_id in processed_page_ids:
                        print(f"Skipping {name} (Page ID: {page_id}) - already processed")
                        continue
                        
                    ads = process_candidate(name, page_id)
                    if ads:
                        candidate_ads.extend(ads)
                        processed_page_ids.add(page_id)
                    time.sleep(20)
                
                # If we got ads for this candidate, save them
                if candidate_ads:
                    df_new = pd.DataFrame(candidate_ads)
                    df_new = df_new.replace({None: np.NaN})
                    # Write with header only if file doesn't exist
                    df_new.to_csv(csv_path, mode='a', header=not csv_path.exists(), index=False)
                    print(f"Saved {len(candidate_ads)} ads for {name}")

    except requests.exceptions.RequestException as e:
        if "613" in str(e) or "rate limit" in str(e).lower():
            print("\nRate limit reached. Restart later to continue.")
            sys.exit(1)
        else:
            raise e

if __name__ == "__main__":
    main()