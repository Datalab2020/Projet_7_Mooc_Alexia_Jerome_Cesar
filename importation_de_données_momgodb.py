import config
import requests
from pprint import pprint
from pymongo import MongoClient
import urllib

client = MongoClient('mongodb://%s:%s@%s/?authSource=%s'% (config.user, urllib.parse.quote(config.password),'127.0.0.1', 'admin'))
db = client['bdd_grp4']
cours_url=input('Adresse du forum FUN-MOOC please? ')
nom_collec=input('Et dans quelle collec tu veux la mettre? ')
collec = db[nom_collec]
cookies = {
    'csrftoken': '58ALZSbDI7EcZWPyzrqzdjjMaTSpNI3B',
    'atuserid': '%7B%22name%22%3A%22atuserid%22%2C%22val%22%3A%22eb9a8987-d6f5-469b-97c5-3dd04c94d801%22%2C%22options%22%3A%7B%22end%22%3A%222022-02-21T15%3A41%3A37.625Z%22%2C%22path%22%3A%22%2F%22%7D%7D',
    'atidvisitor': '%7B%22name%22%3A%22atidvisitor%22%2C%22val%22%3A%7B%22vrn%22%3A%22-602676-%22%7D%2C%22options%22%3A%7B%22path%22%3A%22%2F%22%2C%22session%22%3A15724800%2C%22end%22%3A15724800%7D%7D',
    'acceptCookieFun': 'on',
    'edxloggedin': 'true',
    'edx-user-info': '{\\"username\\": \\"Jerome_Philippe\\"\\054 \\"version\\": 1\\054 \\"email\\": \\"marcusstaile@hotmail.fr\\"\\054 \\"header_urls\\": {\\"learner_profile\\": \\"https://www.fun-mooc.fr/u/Jerome_Philippe\\"\\054 \\"logout\\": \\"https://www.fun-mooc.fr/logout\\"\\054 \\"account_settings\\": \\"https://www.fun-mooc.fr/account/settings\\"}}',
    'edx_session': 'ojobxoqyxvzh75d8w25pqa6iyykwp37j',
}


headers = {
    'User-Agent': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0',
    'Accept': 'application/json, text/javascript, */*; q=0.01',
    'Accept-Language': 'fr,fr-FR;q=0.8,en-US;q=0.5,en;q=0.3',
    'X-CSRFToken': 'NNZmwVbWE252BFgQXhL09fVZNtpO2X9R',
    'X-Requested-With': 'XMLHttpRequest',
    'Connection': 'keep-alive',
    'Referer': cours_url,
}

params = (
    ('ajax', '1'),
    ('resp_skip', '0'),
    ('resp_limit', '200'),
)

response3 = requests.get(cours_url, headers=headers, params=params, cookies=cookies)
print((response3.json()["num_pages"]))
for page in range(1,(response3.json()["num_pages"]+1)):
    params_p = (
    ('ajax', '1'),
    ('page',page),
    ('sort_key', 'comments'),
    ('sort_order', 'desc'),)
    
    response = requests.get(cours_url, headers=headers, params=params_p, cookies=cookies)
    print("")
    print('page:',page)
    result=response.json()
    

    for block in result["discussion_data"]: 
        response2 = requests.get(cours_url+block['commentable_id']+'/threads/'+block['id'], headers=headers, params=params, cookies=cookies)
        result2 = response2.json()
        print(result2["content"]["title"])
        collec.insert_one(result2)
        
        