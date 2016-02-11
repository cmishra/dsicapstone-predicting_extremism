# -*- coding: utf-8 -*-
"""
Created on Mon Dec 28 09:48:15 2015

@author: brian
"""
import requests
import csv
from bs4 import BeautifulSoup
from datetime import *
import pandas as pd
import numpy as np
import PyPDF2
import urllib
#from slate import PDF
#from StringIO import StringIO
#import urllib2

##########################################
#
# Dorothy Day
#
##########################################

url = "http://www.catholicworker.org/dorothyday/browse/index.html"
response = requests.get(url)
soup = BeautifulSoup(response.text)

# first we need to find links to each decade of Dorthy Day
links=soup.find_all("div", { "class" : "col-xs-10" })[0].find_all('a')
links_list = []

for i in range(len(links)):
    links_list.append(soup.find_all("div", { "class" : "col-xs-10" })[0].find_all('a',href=True)[i]['href'])
    
links_list = links_list[:6] # don't need all of these - keep only the relevant ones

# now we want to get a URL for each of her sermons
sublinks_list = []

for i in range(len(links_list)):
    url2 = "http://www.catholicworker.org" + links_list[i]
    response = requests.get(url2)
    soup = BeautifulSoup(response.text)
    for j in range(len(soup.find_all("div", { "class" : "col-xs-8" }))):    
        sublinks_list.append(soup.find_all("div", { "class" : "col-xs-8" })[j].find_all('a')[0]['href'])
    

for k in range(len(sublinks_list)):
    url3 = "http://www.catholicworker.org" + sublinks_list[k]
    response = requests.get(url3)
    soup = BeautifulSoup(response.text)
    text = soup.find_all("div", { "class" : "col-xs-9" })[0].text
    
    filedirectory = 'C:/Users/brian/Documents/UVA/Capstone/DorothyDay/' + sublinks_list[k].split("/")[3] + '.txt'
    
    file = open(filedirectory, 'w', newline='\n')
    file.write(text)
    file.close()
        
        
##########################################
#
# Pastor Anderson
#
##########################################

url = "http://www.faithfulwordbaptist.org/page9.html"
response = requests.get(url)
soup = BeautifulSoup(response.text)


links = soup.find_all("div", { "id" : "fullContent" })[0].find_all('td')[0].find_all('ul')[2].find_all('a',href=True)
links_list = []

for i in range(len(links)):
    links_list.append(soup.find_all("div", { "id" : "fullContent" })[0].find_all('td')[0].find_all('ul')[2].find_all('a',href=True)[i]['href'])
  
    
for k in range(len(links_list)):
    
    if('faithfulwordbaptist' in links_list[k]):
        url2 = links_list[k]
    else:
        url2 = 'http://www.faithfulwordbaptist.org/' + links_list[k]
    
    if('pdf' in url2):
        web_file = urllib.request.urlopen(url2)
        local_file = open("C:/Users/brian/Documents/UVA/Capstone/PastorAnderson/" + links_list[k] + ".pdf", 'wb')
        local_file.write(web_file.read())
        web_file.close()
        local_file.close()
        
        filedirectory = 'C:/Users/brian/Documents/UVA/Capstone/PastorAnderson/' + links_list[k] + '.txt'
            
        pdf = PyPDF2.PdfFileReader(open("C:/Users/brian/Documents/UVA/Capstone/PastorAnderson/" + links_list[k] + ".pdf", "rb"))
        
        for page in pdf.pages:
            file = open(filedirectory, 'w', newline='\n')
            file.write(text)
            file.close()
        
    elif('faithfulwordbaptist' not in links_list[k]):  
        response = requests.get(url2)
        soup = BeautifulSoup(response.text)
        text = soup.find_all("td")[0].text
        text = text.replace("\r"," ")
        text = text.replace("â","")
        
        filedirectory = 'C:/Users/brian/Documents/UVA/Capstone/PastorAnderson/' + links_list[k] + '.txt'

        file = open(filedirectory, 'w', newline='\n', errors='replace')
        file.write(text)
        file.close()

    else:  
        response = requests.get(url2)
        soup = BeautifulSoup(response.text)
        text = soup.find_all("td")[0].text
        text = text.replace("\r"," ")
        
        filedirectory = 'C:/Users/brian/Documents/UVA/Capstone/PastorAnderson/' + links_list[k].split("/")[3] + '.txt'
        
        file = open(filedirectory, 'w', newline='\n')
        file.write(text)
        file.close()



##########################################
#
# Unitarian
#
##########################################

url = "http://www.firstuucolumbus.org/worship-and-music/sermons"
response = requests.get(url)
soup = BeautifulSoup(response.text)

# first we need to find links to each decade of Dorthy Day
links=soup.find_all("div", { "class" : "contentpaneopen" })[0].find_all('td')
links_list = []

for i in range(len(links)):
    try:
        links_list.append(soup.find_all("div", { "class" : "contentpaneopen" })[0].find_all('td')[i].find_all('a')[0]['href'])
    except:
        continue 
    
# now we want to get a URL for each of the sermons
sublinks_list = links_list[8:]
sublinks_list = [sublink for sublink in sublinks_list if 'mp3' not in sublink]
links_list = links_list[:7]

for j in range(len(links_list)):
    url2 = "http://www.firstuucolumbus.org" + links_list[j]
    response = requests.get(url2)
    soup = BeautifulSoup(response.text)
    for k in range(8,len(soup.find_all("div", { "class" : "contentpaneopen" })[0].find_all('a',href=True))):    
        sublinks_list.append(soup.find_all("div", { "class" : "contentpaneopen" })[0].find_all('a',href=True)[k]['href'])
    
sublinks_list = [sublink for sublink in sublinks_list if 'mp3' not in sublink]

'''
def getPDFContent(path):
    content = ""
    p = file(path, "rb")
    pdf = PyPDF2.PdfFileReader(p)
    for i in range(0, pdf.getNumPages()):
        content += pdf.getPage(i).extractText() + "\n"
    content = " ".join(content.replace(u"\xa0", " ").strip().split())
    return content
'''

for k in range(len(sublinks_list)):
    print(k)

    if('pdf' in sublinks_list[k].lower()):
        url3 = sublinks_list[k]
        url3 = url3.replace(" ","%20")
       
        try:
            web_file = urllib.request.urlopen(url3)
        except:
            continue 
        
        local_file = open("C:/Users/brian/Documents/UVA/Capstone/Unitarian/" + sublinks_list[k].split("/")[6], 'wb')
        local_file.write(web_file.read())
        web_file.close()
        local_file.close()
        
        filedirectory = 'C:/Users/brian/Documents/UVA/Capstone/Unitarian/' + sublinks_list[k].split("/")[6] + '.txt'
            
        pdf = PyPDF2.PdfFileReader(open("C:/Users/brian/Documents/UVA/Capstone/Unitarian/" + sublinks_list[k].split("/")[6], "rb"))

        #for page in pdf.pages:
        file = open(filedirectory, 'w', newline='\n', errors='replace')
        
        content = ""
        for i in range(0, pdf.getNumPages()):
            content += pdf.getPage(i).extractText() + "\n"

        file.write(content)
        file.close()


    else:  
        url3 = sublinks_list[k]
        url3 = url3.replace(" ","%20")
     
        response = requests.get(url3)
        soup = BeautifulSoup(response.text)
        text = ""
        for l in range(len(soup.find_all("blockquote"))):
            text = text + str(soup.find_all("blockquote")[l].text)
        
        if(text == ''):
            for l in range(len(soup.find_all("i"))):
                text = text + str(soup.find_all("i")[l].text)    
        
        text = text.replace("\r"," ")        
        filedirectory = 'C:/Users/brian/Documents/UVA/Capstone/Unitarian/' + sublinks_list[k].split("/")[6] + '.txt'
        
        file = open(filedirectory, 'w', newline='\n', errors = 'replace')
        file.write(text)
        file.close()
        
        
##########################################
#
# Lenin
#
##########################################

url = "https://www.marxists.org/archive/lenin/works/cw/index.htm#volume10"
response = requests.get(url)
soup = BeautifulSoup(response.text)

links = soup.find_all("blockquote")[0].find_all('tr')
links = links[:45] # don't need the last 2 elements in this list
links_list = []

for i in range(len(links)):
    links_list.append(soup.find_all("blockquote")[0].find_all('tr')[i].find_all('a',href=True)[1]['href'])
  

for k in range(len(links_list)):
    print(k)

    url2 = "https://www.marxists.org/archive/lenin/works/cw/" + links_list[k]
   
    try:
        web_file = urllib.request.urlopen(url2)
    except:
        continue 
    
    local_file = open("C:/Users/brian/Documents/UVA/Capstone/Lenin/" + links_list[k].split("/")[1], 'wb')
    local_file.write(web_file.read())
    web_file.close()
    local_file.close()
    

##########################################
#
# Steve Shepherd
#
##########################################

links_list = []

for i in range(1,74):

    url = "http://www.sermoncentral.com/Sermons/SearchResults.asp?AudienceAge=&Denomination=&Series=&Page=" + str(i) + "&Sort=date&IsOutline=2&MinRating=&MaxResults=&keyword=&ScriptureBookA2=&ScriptureVerse2=&TopicID=0&series2=&audienceage2=&denomination2=&lang2=&since2=0&Audio=0&BodyFlag=0&ContributorID=1281&MultimediaTypeID="
    response = requests.get(url)
    soup = BeautifulSoup(response.text)

    links = soup.find_all("div", { "class" : "SermonListing-Left" })

    for j in range(len(links)):
        links_list.append(soup.find_all("div", { "class" : "SermonListing-Left" })[j].find_all('a',href=True)[0]['href'])
      
for l in range(577,len(links_list)):
    print(l)
    text = ""
    url2 = "http://www.sermoncentral.com" + links_list[l] + "?Page=1" 
    response = requests.get(url2)
    soup = BeautifulSoup(response.text)
    
    try:
        for j in range(0,(len(soup.find_all("div", { "class" : "PageNumbers" })[0].find_all('li'))-1)):
            url2 = "http://www.sermoncentral.com" + links_list[l] + "?Page=" + str(j+1)
            response = requests.get(url2)
            soup = BeautifulSoup(response.text)
            text += soup.find_all("div", { "id" : "SermonBody" })[0].find_all("div", { "id" : "TheSermonText" })[0].text
    except:
        continue
    
    filedirectory = 'C:/Users/brian/Documents/UVA/Capstone/Shepherd/' + links_list[l].split("/")[2] + '.txt'
    
    file = open(filedirectory, 'w', newline='\n', errors = 'replace')
    file.write(text)
    file.close()
    

##########################################
#
# Mehr Baba
#
##########################################

url = "https://www.ambppct.org/library.php"
response = requests.get(url)
soup = BeautifulSoup(response.text)

links = soup.find_all("table")[59].find_all('tr')
#[0].find_all('td')[0].find_all('a',href=True)
links2 = soup.find_all("table")[60].find_all('tr')[0].find_all('td')[0].find_all('a',href=True)

links_list = []

for i in range(len(soup.find_all("table")[59].find_all('tr'))-1):   
    links_list.append(soup.find_all("table")[59].find_all('tr')[i].find_all('a',href=True)[0]['href'])
     
for i in range(len(soup.find_all("table")[60].find_all('tr')[0].find_all('td')[0].find_all('a',href=True))):
    links_list.append(soup.find_all("table")[60].find_all('tr')[0].find_all('td')[0].find_all('a',href=True)[i]['href'])
    
links_list = [x for x in links_list if x != 'wgEditorial.pdf'] # get rid of observation we don't want

for k in range(len(links_list)):
    print(k)

    url2 = "https://www.ambppct.org/" + links_list[k]
   
    try:
        web_file = urllib.request.urlopen(url2)
    except:
        continue 
    
    local_file = open("C:/Users/brian/Documents/UVA/Capstone/MehrBaba/" + links_list[k].split("/")[1], 'wb')
    local_file.write(web_file.read())
    web_file.close()
    local_file.close()
    
    
##########################################
#
# Nauman Khan
#
##########################################

url = "http://www.nakcollection.com/transcripts.html"
response = requests.get(url)
soup = BeautifulSoup(response.text)


links=soup.find_all("div", { "class" : "paragraph" })

links_list = []

for i in range(len(links)):
    try:
        links_list.append(soup.find_all("div", { "class" : "paragraph" })[i].find_all('a',href=True)[0]['href'])
    except:
        continue 
    
# now we want to get a URL for each of the sermons
links_list = [x for x in links_list if 'mp3' not in x]
links_list = [x for x in links_list if 'facebook' not in x]
links_list = links_list[:88]

# may need to run several times - website has a habit of aborting the connection
for k in range(65,len(links_list)):
    print(k)
    url2 = "http://www.nakcollection.com" + links_list[k]
    try:    
        urllib.request.urlretrieve(url2, "C:/Users/brian/Documents/UVA/Capstone/NaumanKhan/" + links_list[k].split("/")[-1])
    except:
        continue



##########################################
#
# Rabbinic Sermons
#
##########################################

headers = { 'User-Agent': 'Mozilla/5.0 (Windows NT 6.0; WOW64; rv:24.0) Gecko/20100101 Firefox/24.0' }

# now we want to get a URL for each of her sermons
links_list = []
#links_list.append('/resources/sermons/renewing_covenant')
#links_list.append('/resources/sermons/what-comes-next')
#links_list.append('/resources/sermons/sons-fathers')

for i in range(55):
    print(i)
    url = "http://pasyn.org/resources/sermons?page=" + str(i+1)
    response = requests.get(url, headers=headers)
    soup = BeautifulSoup(response.text)
    for j in range(len(soup.find_all("div", {"class" : "clear-block"})[0].find_all("div", {"class" : "views-field-view-node"}))):
        links_list.append(soup.find_all("div", {"class" : "clear-block"})[0].find_all("div", {"class" : "views-field-view-node"})[j].find_all('a')[0]['href'])
        





for k in len(links_list)):
    print(k)
    url2 = "http://pasyn.org" + links_list[k]
    response = requests.get(url2, headers=headers)
    soup = BeautifulSoup(response.text)
    text = soup.find_all("div", { "class" : "content clear-block" })[0].text
    
    filedirectory = 'C:/Users/brian/Documents/UVA/Capstone/Rabbinic/' + links_list[k].split("/")[len(links_list[k].split("/"))-1] + '.txt'
    
    file = open(filedirectory, 'w', newline='\n', encoding='utf8')
    file.write(text)
    file.close()
    
    
    
    