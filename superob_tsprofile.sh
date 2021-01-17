#!/bin/bash
set -e

#"/lustre/cda/CDA/hybrid-godas-sst/DATA/obs/tsprofile/2010"

# Configurables
START_DATE=20100101
END_DATE=20100105

ROOT_DIR=$(pwd)
GRID_DIR=$ROOT_DIR/DATA/grid
WORK_DIR=$ROOT_DIR/WORK_tsprofile
OBSIN_DIR=$ROOT_DIR/DATA/obs/tsprofile/raw
OBSOUT_DIR=$ROOT_DIR/DATA/obs/tsprofile/test2


#------------------------------------------------------------
ulimit -s unlimited

# setup working directory
mkdir -p $WORK_DIR
cd $WORK_DIR
cp $ROOT_DIR/run/config.exp_default/da/obsprep.nml obsprep.nml.tmp
sed "s/\$obsfile_in/fnin_/g; s/\$obsfile_out/fnout.nc/g; s/density_sigma=0.125/density_sigma=-1/g" obsprep.nml.tmp > obsprep.nml
rm -f obsprep.nml.tmp
ln -sf $ROOT_DIR/build/obsprep_insitu_legacy .
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

    # create output dir
    odir=$OBSOUT_DIR/$yr/$cdate
    mkdir -p $odir
 
    # link input tsfiles
    fnsal=${cdate}sal.nc
    fntmp=${cdate}tmp.nc
    ln -sf $OBSIN_DIR/$yr/$fnsal fnin_sal.nc
    ln -sf $OBSIN_DIR/$yr/$fntmp fnin_tmp.nc

    # run command
    ./obsprep_insitu_legacy

    # move output tsfiles
    mv fnout.nc $odir/${cdate}.nc

    read -p "press key to continue"

    rm -f fnin_sal.nc 
    rm -f fnin_tmp.nc
    
    # end of loop, prepare for the next date
    cdate=$(date -d "$cdate + 1 day" "+%Y%m%d")
done

