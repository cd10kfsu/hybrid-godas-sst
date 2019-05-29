#!/bin/bash
set -e

# Configurables
START_DATE=20100101
END_DATE=20100101

ROOT_DIR=$(pwd)
GRID_DIR=$ROOT_DIR/DATA/grid
WORK_DIR=$ROOT_DIR/WORK
OBSIN_DIR=$ROOT_DIR/DATA/obs/sst_acspo_avhrr/raw
OBSOUT_DIR=$ROOT_DIR/DATA/obs/sst_acspo_avhrr

#------------------------------------------------------------

# setup working directory
mkdir -p $WORK_DIR
cd $WORK_DIR
cp $ROOT_DIR/run/config.exp_default/da/obsprep.nml .
ln -sf $ROOT_DIR/build/obsprep_sst .
ln -sf $ROOT_DIR/build/obsprep_combine .
mkdir -p INPUT
cd INPUT
ln -sf $GRID_DIR/* .
mv hgrid.nc grid.nc
cd ..

# loop over all the dates of interest
cdate=$START_DATE
while [[ $cdate -le $END_DATE ]]
do
    echo Processing $cdate
    yr=${cdate:0:4}

    # do the superobbing on each input file
    for f in $OBSIN_DIR/$yr/$cdate/*.nc; do
    f2=${f##*/} # get the file basename
    ./obsprep_sst $f $f2
    exit
    done

    # combine all the satellite tracks into a single day
    odir=$OBSOUT_DIR/$yr/$cdate
    mkdir -p $odir
    basedate=$(date "+%Y,%m,%d,0,0,0" -d "$cdate")
    ./obsprep_combine -basedate $basedate ??????????.*.nc $odir/$cdate.nc

    # cleanup temporary files
    rm *.nc
    
    # end of loop, prepare for the next date
    cdate=$(date -d "$cdate + 1 day" "+%Y%m%d")
done

