#!/bin/bash
set -e

# Configurables
START_DATE=20150404
END_DATE=20150410

ROOT_DIR=$(pwd)
GRID_DIR=$ROOT_DIR/DATA/grid
WORK_DIR=$ROOT_DIR/WORK
OBSIN_DIR=$ROOT_DIR/DATA/obs/sst_cmc0d2/raw
OBSOUT_DIR=$ROOT_DIR/DATA/obs/sst_cmc0d2


#------------------------------------------------------------
basehour="12"

# setup working directory
mkdir -p $WORK_DIR
cd $WORK_DIR
cp $ROOT_DIR/run/config.exp_default/da/obsprep.nml obsprep.nml.tmp
sed "s/sst_type=1/sst_type=2/" obsprep.nml.tmp > obsprep.nml
rm -f obsprep.nml.tmp
ln -sf $ROOT_DIR/build/obsprep_sst .
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

    # combine all the satellite tracks into a single day
    odir=$OBSOUT_DIR/$yr/$cdate
    mkdir -p $odir
 
    # do the superobbing on each input file (only 1 file in the dir)
    for f in $OBSIN_DIR/$yr/$cdate/*.nc; do
        echo $f
        ./obsprep_sst $f out.${cdate}${basehour}.nc
    done

    mv out.${cdate}${basehour}.nc $odir/${cdate}${basehour}.nc

    
    # end of loop, prepare for the next date
    cdate=$(date -d "$cdate + 1 day" "+%Y%m%d")
done

