#!/usr/bin/env python3
import argparse
import datetime as dt
import ftplib
import os, shutil

rootdir=os.path.abspath(os.path.dirname(os.path.realpath(__file__))+'/../')

parser=argparse.ArgumentParser(description='script to dowload L4 CMC 0.2deg SST analysis (CMC0.2deg-CMC-L4-GLOB-v2.0)')
parser.add_argument('start_date')
parser.add_argument('end_date', nargs='?')
parser.add_argument('-basehour',choices=['12'], default='12')
args=parser.parse_args()
if args.end_date == None:
    args.end_date = args.start_date

args.start_date = dt.datetime.strptime(args.start_date, "%Y%m%d").date()
args.end_date = dt.datetime.strptime(args.end_date, "%Y%m%d").date()
args.ftpsite="ftp.nodc.noaa.gov"
args.ftpdir="/pub/data.nodc/ghrsst/GDS2/L4/GLOB/OSPO/Geo_Polar_Blended/v1/"
args.obsdir=rootdir+'/DATA/obs/sst_ospo/raw'
print(args)

#------------------------------------------------------------
ftp=ftplib.FTP(args.ftpsite)
ftp.login()

anaDates=[]
print("Reading available dates")
ftp.cwd(args.ftpdir)
years = ftp.nlst()
years = [y for y in years if  args.start_date.year <= int(y) <= args.end_date.year]
for y in years:        
    ftp.cwd(args.ftpdir+'/{}'.format(y))
    days = ftp.nlst()
    anaDates += [y+d for d in days]
ftp.quit()


# for each date we want to download
cdate = args.start_date
while cdate <= args.end_date:
    print("")
    print("downloading "+str(cdate))
    daynum=(cdate-dt.date(cdate.year, 1,1)).days + 1
    outdir = args.obsdir+cdate.strftime("/%Y/%Y%m%d")
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    yrday="{}{:03d}".format(cdate.year, daynum)
    if yrday not in anaDates:
       continue
    filedir=args.ftpdir+'/{}/{:03d}'.format(cdate.year,daynum)

    try:
        ftp=ftplib.FTP(args.ftpsite)
        ftp.login()
        ftp.cwd(filedir)
        files=ftp.nlst()
        ftp.quit()
    except:
        print ("*****ERROR: unable to open directory ",filedir)
        continue

    for f in sorted(files):
        fname=f[:8]+args.basehour+'.Geo_Polar_Blended-OSPO-L4-GLOB-v1.0.nc'
        print(fname)
        try:
            ftp=ftplib.FTP(args.ftpsite)
            ftp.login()
            ftp.cwd(filedir)
            with open(outdir+'/'+fname, 'wb') as outfile:
                ftp.retrbinary('RETR '+f,outfile.write)
                outfile.close()
            ftp.quit()
        except:
            print('*** ERROR: unable to download ***************************')
            if os.path.exists(outdir+'/'+fname):
                os.remove(outdir+'/'+fname)
    cdate += dt.timedelta(days=1)
