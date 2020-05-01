from bs4 import BeautifulSoup
import pandas as pd
import requests
df_list = []
for page in range(0, 5000):
    try:
        url = "http://bvbinfo.com/Tournament.asp?ID={}".format(page)
        df_list.append(pd.read_html(url)[3].iloc[:, :8])
        df = pd.concat(df_list)
    except:
        print(url)
df.to_csv('bvb_finishes.csv')
