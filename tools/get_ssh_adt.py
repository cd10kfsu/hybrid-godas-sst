#!/usr/bin/env python3
import argparse
import datetime as dt
import ftplib
import os, shutil

rootdir=os.path.abspath(os.path.dirname(os.path.realpath(__file__))+'/../')

parser=argparse.ArgumentParser(description='script to download the ADT files')
parser.add_argument('start_date')
parser.add_argument('end_date', nargs='?')
args=parser.parse_args()
if args.end_date == None:
    args.end_date = args.start_date

args.start_date = dt.datetime.strptime(args.start_date, "%Y%m%d").date()
args.end_date = dt.datetime.strptime(args.end_date, "%Y%m%d").date()
args.ftpsite = 'ftp.star.nesdis.noaa.gov'
args.ftpdir = 'pub/sod/lsa/rads/adt/'
args.obsdir = rootdir+'/DATA/obs/ssh_adt/raw'
print(args)

#-----------------------------------------------------

cdate = args.start_date
while cdate <=args.end_date:
    print("")
    jday=cdate.timetuple().tm_yday
    print("downloading data for the date:{} (day {})".format(cdate,jday)) 

    # create local directory
    outdir=args.obsdir+cdate.strftime("/%Y/%Y%m%d")
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    else:
        print("*****WARN: directory {} exists!".format(outdir))


    # retrieve daily data list
    jday=cdate.timetuple().tm_yday

    filewildcard="{}{}{:03d}{}".format("rads_adt_*_",cdate.year,jday,".nc")
    dailydir=args.ftpdir+'/{}'.format(cdate.year)

    #print(filename)
    #print(dailydir)

    try:    
        ftp = ftplib.FTP(args.ftpsite)
        ftp.login()
        ftp.cwd(dailydir)
        filelist=ftp.nlst(filewildcard)
        ftp.quit()
    except:
        print("*****ERROR: no SSH ADT data for the date: ",cdate)
        cdate+=dt.timedelta(days=1)
        continue

    for fname in sorted(filelist):
        print(fname)
        try:
            ftp = ftplib.FTP(args.ftpsite)
            ftp.login()
            ftp.cwd(dailydir)
            with open(outdir+'/'+fname,'wb') as outfile:
                ftp.retrbinary("RETR "+fname, outfile.write)
                outfile.close()
            ftp.quit()
        except:
            print('*****ERROR: unable to download file: ', fname)
            if os.path.exists(outdir+'/'+fname):
                os.remove(outdir+'/'+fname)

    cdate+=dt.timedelta(days=1)

print("")    
print("*****MESSAGE: Done!")
quit()

