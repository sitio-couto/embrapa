from os import listdir
from datetime import date, timedelta
import argparse
import requests
from requests.auth import HTTPBasicAuth

metadata = list(map(lambda x : x[:-1] if x[-1]=='\n' else x, open('info.txt').readlines()))
late_data = sorted(listdir('late_run'))
final_data = sorted(listdir('final_run'))
parser = argparse.ArgumentParser(description='List the content of a folder')

parser.add_argument('-u','--user', action='store', dest='user', help='Defines user name')
parser.add_argument('-p','--pass', action='store', dest='password', help='Defines users password')
parser.add_argument('-x','--lon', nargs=2, dest='lon', help='Longitude values (min max)', type=int)
parser.add_argument('-y','--lat', nargs=2, dest='lat', help='Latitude values (min max)', type=int)
parser.add_argument('-v','--variables', action='append', dest='var', help='Defines single varible')
parser.add_argument('-t','--time', nargs=2, dest='time', help='Range of dates (YYYY/MM/DD YYYY/MM/DD)')
options = parser.parse_args()

late_source = 0
final_source = 0
version = 0
user = 0
password = 0
lon = 0
lat = 0
var = 0
time = 0

for line in metadata:
    if line.split() == [] : continue
    if len(line.split('|')) < 2 : continue 

    [label,data] = line.split('|')
    if (data == []) : continue
    data = data.strip()

    if "late_source" == label : late_source = data
    if "final_source" == label : final_source = data
    if "version" == label : version = data
    if "user" == label : user = data
    if "pass" == label : password = data
    if "lon" == label : lon = data.split(',')
    if "lat" == label : lat = data.split(',')
    if "var" == label : var = data.split(',')
    if "time" == label : time = data.split(',')

# Overwrite id new parameters are passed through bash
if options.user is not None : user = options.user
if options.password is not None : password = options.password
if options.lon is not None : lon = options.lon
if options.lat is not None : lat = options.lat
if options.var is not None: var = options.var
if options.time is not None : time = options.time

# Cast degrees to range
lon = list(map(lambda x : int(x)*10+1800-1, lon))
lat = list(map(lambda x : int(x)*10+900-1, lat))

# Creating range of dates to be fetched
[year,month,day] = list(map(int, time[0].split('/')))
base = date(year, month, day)
[year,month,day] = list(map(int, time[1].split('/')))
end = date(year, month, day)
delta = end - base  

dates_list = []
for i in range(delta.days + 1):
    dates_list.append(base+timedelta(days=i))


# Generate late product links
late_links = []
for date in dates_list:
    year="{0:0=4d}".format(date.year)
    month="{0:0=2d}".format(date.month)
    day="{0:0=2d}".format(date.day)
    ver= "{0:0=2d}".format(int(version))
    link = '3B-DAY-L.MS.MRG.3IMERG.'+year+month
    link += day+'-S000000-E235959.V'+ver+'.nc4.nc'
    
    # If already exists, ignore
    if link in late_data : continue
    file_name = link

    link = late_source+'.'+ver+'/'+year+'/'+month+'/'+link+'?'
    
    for v in var:
        link += v+"[0:0]"+f'[{lon[0]}:{lon[1]}]'+f'[{lat[0]}:{lat[1]}],'

    link += 'time,'+f'lon[{lon[0]}:{lon[1]}],lat[{lat[0]}:{lat[1]}]'
    late_links.append([link,file_name])

# Generate final product links
final_links = []
for date in dates_list:
    year="{0:0=4d}".format(date.year)
    month="{0:0=2d}".format(date.month)
    day="{0:0=2d}".format(date.day)
    ver= "{0:0=2d}".format(int(version))
    link = '3B-DAY.MS.MRG.3IMERG.'+year+month
    link += day+'-S000000-E235959.V'+ver+'.nc4.nc'
    
    # If already exists, ignore
    if link in final_data : continue
    file_name = link

    link = final_source+'.'+ver+'/'+year+'/'+month+'/'+link+'?'
    
    for v in var:
        link += v+"[0:0]"+f'[{lon[0]}:{lon[1]}]'+f'[{lat[0]}:{lat[1]}]'
    
    link += 'time,'+f'lon[{lon[0]}:{lon[1]}],lat[{lat[0]}:{lat[1]}]'
    final_links.append([link,file_name])


# Download missing links
for [l,n] in late_links : 
    print(f"Downloading Late File:\n {l}")
    r = requests.get(l, auth=HTTPBasicAuth(user, password))
    open('late_run/'+n, 'wb').write(r.content)

for [f,n] in final_links : 
    print(f"Downloading Final File:\n {f}")
    r = requests.get(f, auth=HTTPBasicAuth(user, password))
    open('final_run/'+n, 'wb').write(r.content)
