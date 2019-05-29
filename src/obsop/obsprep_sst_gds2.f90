module obsprep_sst_gds2
  use netcdf
  use datetime_module

  implicit none
  private
  real, parameter :: defaulterr = 1.0
  real, parameter :: badval = -9.99e9


  type, public :: sst_data
     real   :: lon
     real   :: lat
     real   :: val
     real   :: err
     real   :: time
  end type sst_data

  public :: addtag_nc
  integer,parameter,public :: SST_TYPE_GDS2    = 1
  integer,parameter,public :: SST_TYPE_CMC0D2  = 2
  character(*),parameter :: SST_NAME_GDS2   = "SST_GDS2"
  character(*),parameter :: SST_NAME_CMC0D2 = "SST_CMC0D2"

  public :: read_sst_gds2_nc
  !logical, parameter :: gds2_applybias = .true.
  !logical, parameter :: gds2_usenight  = .true.
  !logical, parameter :: gds2_useday    = .false.
  !integer, parameter :: gds2_minqc     = 5
  logical :: gds2_applybias = .true.
  logical :: gds2_usenight  = .true.
  logical :: gds2_useday    = .false.
  integer :: gds2_minqc     = 5
  namelist /sst_gds2_cfg/ gds2_applybias, gds2_usenight, gds2_useday, gds2_minqc


  public :: read_sst_cmc0d2_nc 
  !logical,parameter :: cmc0d2_useice    = .false.
  !logical,parameter :: cmc0d2_usenonsea = .false.
  logical :: cmc0d2_useice    = .false.
  logical :: cmc0d2_usenonsea = .false.
  integer :: cmc0d2_seamsk    = 1
  namelist /sst_cmc0d2_cfg/ cmc0d2_useice, cmc0d2_usenonsea


  
  interface readField
    module procedure readField2
    module procedure readField3
  endinterface readField


contains

  subroutine addtag_nc(outfile,sst_type)
    implicit none

    character(len=*),intent(in) :: outfile
    integer,         intent(in) :: sst_type

    integer :: ncid,ierr

    call check(nf90_open(trim(outfile),NF90_WRITE,ncid))
    call check(nf90_redef(ncid))
    if (sst_type == SST_TYPE_GDS2) then
       call check(nf90_put_att(ncid,NF90_GLOBAL, "tag", trim(SST_NAME_GDS2)))
    elseif (sst_type == SST_TYPE_CMC0D2) then
       call check(nf90_put_att(ncid,NF90_GLOBAL, "tag", trim(SST_NAME_CMC0D2)))
    else
       print*, "Error: unrecognized SST obs type:", sst_type
       stop 213
    endif
    call check(nf90_enddef(ncid))
    call check(nf90_close(ncid))

  endsubroutine addtag_nc
  

  subroutine read_sst_gds2_nc(infile, basedate, obs)
    character(len=*),            intent(in)  :: infile
    type(datetime),              intent(out) :: basedate
    type(sst_data), allocatable, intent(out) :: obs(:)


    logical :: regular_grid

    integer, parameter :: max_dimids = 10
    integer :: ndims, dimids(max_dimids)

    integer :: ncid, vid
    integer :: d_x, d_y
    integer :: nx, ny, i, j, c
    real :: r, r2, r3
    character(len=1024) :: tmp_str
    integer :: stat
    real, allocatable :: tmp2d(:,:)
    type(sst_data), allocatable :: obs2(:,:)
    logical, allocatable :: qc(:,:)
    integer(kind=2), allocatable ::tmp_int(:,:)
    character(len=4) :: proclvl

    ! print out obs qc
    print sst_gds2_cfg 
    
    ! open the file, make sure a valid GHRSST GDS2.0 file
    call check(nf90_open(infile, nf90_nowrite, ncid))
    call check(nf90_get_att(ncid, nf90_global, 'gds_version_id', tmp_str))
    read (tmp_str,*,iostat=stat) r
    if (r /= 2.0) then
       print *, "ERROR: Not a GDS 2.0 formatted file"
       stop 1
    end if
    call check(nf90_get_att(ncid, nf90_global, 'title', tmp_str))
    print *, trim(tmp_str)

    ! which processing level (only handle L3C and L2P for now)
    call check(nf90_get_att(ncid, nf90_global, 'processing_level', tmp_str))
    proclvl=tmp_str(1:3)
    if (proclvl /= "L2P" .and. proclvl /= "L3C") then
       print *, "ERROR: code currently only handles L2P or L3C data. file is ", proclvl
       stop 1
    end if
    print *,"processing level: ", proclvl

    ! what grid type (regular lat/lon or irregular)
    i = 0
    regular_grid = .true.
    call check(nf90_inq_dimids(ncid, ndims, dimids, i))
    do i=1,ndims
       call check(nf90_inquire_dimension(ncid, dimids(i), tmp_str))
       if (trim(tmp_str) == "ni" .or. trim(tmp_str) == "nj") regular_grid = .false.
    end do
    print *, "Regular grid: ",regular_grid

    ! get the grid dimensions, allocate space
    call check(nf90_inq_dimid(ncid, merge("lon","ni ", regular_grid), d_x))
    call check(nf90_inq_dimid(ncid, merge("lat","nj ", regular_grid), d_y))
    call check(nf90_inquire_dimension(ncid, d_x, len=nx))
    call check(nf90_inquire_dimension(ncid, d_y, len=ny))
    print *, "Grid dimension: ",nx,"x",ny
    allocate(tmp2d(nx,ny))
    allocate(obs2(nx,ny))
    allocate(qc(nx,ny))
    allocate(tmp_int(nx,ny))

    ! get the base time
    call check(nf90_inq_varid(ncid, "time", vid))
    call check(nf90_get_att(ncid, vid, 'units', tmp_str))
    if (trim(tmp_str) /= "seconds since 1981-01-01 00:00:00") then
       print *, "ERROR, nonstandard time format"
       stop 1
    end if
    call check(nf90_get_var(ncid, vid, i))
    basedate=datetime(1981,1,1,0,0,0,0)+timedelta(seconds=i)
    print *, "base date/time: ", basedate%isoformat()

    ! observation values
    !------------------------------------------------------------
    !qc
    qc = .true.
    call check(nf90_inq_varid(ncid, "quality_level", vid))
    call check(nf90_get_var(ncid, vid, tmp2d))
    where (tmp2d < gds2_minqc) qc = .false.


    !check day/night flag
    !TODO: i think the night/day flag is a non-standard part of the l2p_flags field
    ! check if this is true.
    if (proclvl == 'L2P') then
       call check(nf90_inq_varid(ncid, "l2p_flags", vid))
       call check(nf90_get_var(ncid, vid, tmp_int))     
       do j=1,ny; do i=1,nx
          tmp2d(i,j) = ibits(tmp_int(i,j),9,1)+1
       end do; end do
       where (.not. qc) tmp2d = 0
       print *, "day obs:  ", count(tmp2d==2)
       print *, "night obs:", count(tmp2d==1)
       
       if ((.not. gds2_usenight) .or. (.not. gds2_useday)) then
          where(tmp2d == merge(2,1,gds2_usenight)) qc = .false.
       end if
    end if


    ! lat
    call check(nf90_inq_varid(ncid, "lat", vid))
    if (regular_grid) then
       call check(nf90_get_var(ncid, vid, tmp2d(1,:)))
       do j=1,ny
          tmp2d(:,j) = tmp2d(1,j)
       end do
    else
       call check(nf90_get_var(ncid, vid, tmp2d))
    end if
    do j=1,ny; do i=1,nx
       obs2(i,j)%lat = tmp2d(i,j)
    end do; end do

    ! lon
    call check(nf90_inq_varid(ncid, "lon", vid))
    if (regular_grid) then
       call check(nf90_get_var(ncid, vid, tmp2d(:,1)))
       do i=1,nx
          tmp2d(i,:) = tmp2d(i,1)
       end do
    else
       call check(nf90_get_var(ncid, vid, tmp2d))
    end if
    do j=1,ny; do i=1,nx
       obs2(i,j)%lon = tmp2d(i,j)
    end do; end do
    
    ! SST
    call readField(ncid, "sea_surface_temperature", tmp2d)
    where (tmp2d == badval) qc = .false.   
    do j=1,ny; do i=1,nx
       obs2(i,j)%val = tmp2d(i,j)
    end do; end do

    ! estimated error
    call readField(ncid, "sses_standard_deviation", tmp2d)
    where(tmp2d == badval) tmp2d = defaulterr
    do j=1,ny; do i=1,nx
       obs2(i,j)%err = tmp2d(i,j)
    end do; end do

    ! bias correction
    if (gds2_applybias) then
       call readField(ncid, "sses_bias", tmp2d)
       where(tmp2d == badval) tmp2d = 0.0
       do j=1,ny; do i=1,nx
          obs2(i,j)%val = obs2(i,j)%val - tmp2d(i,j)
       end do; end do
    end if


    ! time diff

    ! TODO: read in base date/time
    call readField(ncid, "sst_dtime", tmp2d)
    where(tmp2d == badval) tmp2d = 0.0
    do j=1,ny; do i=1,nx
       obs2(i,j)%time = tmp2d(i,j)/3600.0 ! convert seconds to hours
    end do; end do


    ! keep only the good obs
    c = count(qc)
    allocate(obs(c))
    c = 0
    do j=1,ny; do i=1,nx       
       if (.not. qc(i,j) ) cycle
       c = c + 1
       obs(c) = obs2(i,j)
    end do; end do


    ! print some stats
    print *, "Valid observations:"
    print *, " obs cnt: ", c
    r=9e9; r2=0.0; r3=-9e9
    do i=1,c; r=min(r,obs(i)%val); r2 = r2 + (obs(i)%val-r2)/i; r3=max(r3,obs(i)%val); end do
    print *, ' val min/avg/max: ', r,r2,r3

    r=9e9; r2=0.0; r3=-9e9
    do i=1,c; r=min(r,obs(i)%err); r2 = r2 + (obs(i)%err-r2)/i; r3=max(r3,obs(i)%err); end do
    print *, ' err min/avg/max: ', r,r2,r3

    

    ! all done
    call check(nf90_close(ncid))
    deallocate(qc)
    deallocate(obs2)
    deallocate(tmp2d)
    deallocate(tmp_int)

  end subroutine read_sst_gds2_nc



  subroutine read_sst_cmc0d2_nc(infile, basedate, obs)
    character(len=*),            intent(in)  :: infile
    type(datetime),              intent(out) :: basedate
    type(sst_data), allocatable, intent(out) :: obs(:)

    integer :: ncid, vid, d_x, d_y
    type(sst_data),  allocatable :: obs3(:,:,:)
    real,            allocatable :: tmp1d(:), tmp1d2(:), tmp3d(:,:,:)
    logical,         allocatable :: qc(:,:,:)
    integer(kind=2), allocatable :: tmp_int(:,:,:)
    integer :: nx, ny, i, j, c
    real    :: r, r2, r3
    character(len=1024) :: tmp_str

    print sst_cmc0d2_cfg

    ! open the file, make sure a valid CMC 0.2 deg global sea surface temperature analysis
    call check(nf90_open(infile, nf90_nowrite, ncid))
    call check(nf90_get_att(ncid, nf90_global, 'id', tmp_str))
    tmp_str = adjustl(tmp_str)
    if (tmp_str(1:26) /= "CMC0.2deg-CMC-L4-GLOB-v2.0") then
       print *, "ERROR: Not a CMC0.2deg-CMC-L4-GLOB-v2.0 file"
       print *, "       id(inputilfe) = ", trim(tmp_str)
       stop 1
    end if
    call check(nf90_get_att(ncid, nf90_global, 'title', tmp_str))
    print *, trim(tmp_str)

    ! get the grid dimensions, allocate space
    call check(nf90_inq_dimid(ncid, "lon", d_x))
    call check(nf90_inq_dimid(ncid, "lat", d_y))
    call check(nf90_inquire_dimension(ncid, d_x, len=nx))
    call check(nf90_inquire_dimension(ncid, d_y, len=ny))
    print *, "Grid dimension: ",nx,"x",ny, "=", nx*ny
    allocate(tmp1d(nx), tmp1d2(ny))
    allocate(tmp3d(nx,ny,1))
    allocate(obs3(nx,ny,1))
    allocate(qc(nx,ny,1))
    allocate(tmp_int(nx,ny,1))

    ! get the base time
    call check(nf90_inq_varid(ncid, "time", vid))
    call check(nf90_get_att(ncid, vid, 'units', tmp_str))
    tmp_str = adjustl(tmp_str)
    if (tmp_str(1:33) /= "seconds since 1981-01-01 00:00:00") then
       print *, "ERROR, nonstandard time format"
       print *, "       units(inputfile) = ", trim(tmp_str)
       stop 1
    end if
    call check(nf90_get_var(ncid, vid, i))
    basedate=datetime(1981,1,1,0,0,0,0)+timedelta(seconds=i)
    print *, "base date/time: ", basedate%isoformat()

    ! observation values
    !------------------------------------------------------------
    !qc
    qc = .true.

    !check surface mask
    ! 1b,    2b,  4b,                   8b,     16b
    ! water land optional_lake_surface sea_ice optional_river_surface
    if (.not.cmc0d2_usenonsea) then
       call check(nf90_inq_varid(ncid, "mask", vid))
       call check(nf90_get_var(ncid, vid, tmp_int))
       print*, "mask: min,max=", minval(tmp_int), maxval(tmp_int)
       where (tmp_int/=cmc0d2_seamsk) qc = .false.
    endif

    if (.not.cmc0d2_useice) then ! remove grids with sea ice fraction >0.1%
       call readField(ncid, "sea_ice_fraction", tmp3d)
       where (tmp3d==badval) qc = .false.
       where ( qc .and. tmp3d>0.001d0) qc = .false.
    endif

    ! lon
    call check(nf90_inq_varid(ncid, "lon", vid))
    call check(nf90_get_var(ncid, vid, tmp1d))
    do j = 1, ny; do i = 1, nx
       obs3(i,j,1)%lon = tmp1d(i)
    enddo; enddo
    !print *, "lon min/max:", minval(obs3(:,:,:)%lon),maxval(obs3(:,:,:)%lon)

    ! lat
    call check(nf90_inq_varid(ncid, "lat", vid))
    call check(nf90_get_var(ncid, vid, tmp1d2))
    do j = 1, ny; do i = 1, nx
       obs3(i,j,1)%lat = tmp1d2(j)
    enddo; enddo
    !print *, "lat min/max:", minval(obs3(:,:,:)%lat),maxval(obs3(:,:,:)%lat)
    
    ! SST
    call readField(ncid, "analysed_sst", tmp3d)
    where (tmp3d == badval) qc = .false.   
    do j=1,ny; do i=1,nx
       obs3(i,j,1)%val = tmp3d(i,j,1)
    end do; end do

    ! estimated error
    call readField(ncid, "analysis_error", tmp3d)
    where(tmp3d == badval) tmp3d = defaulterr
    do j=1,ny; do i=1,nx
       obs3(i,j,1)%err = tmp3d(i,j,1)
    end do; end do

    ! time diff
    do j=1,ny; do i=1,nx
       obs3(i,j,1)%time = 0.d0/3600.d0 ! convert seconds to hours
    end do; end do


    ! keep only the good obs
    c = count(qc)
    allocate(obs(c))
    c = 0
    do j=1,ny; do i=1,nx       
       if (.not. qc(i,j,1) ) cycle
       c = c + 1
       obs(c) = obs3(i,j,1)
    end do; end do


    ! print some stats
    print *, "Valid observations:"
    print *, " obs cnt retained/total/%: ", c, nx*ny, c*1.d2/nx/ny
    r=9e9; r2=0.0; r3=-9e9
    do i=1,c; r=min(r,obs(i)%val); r2 = r2 + (obs(i)%val-r2)/i; r3=max(r3,obs(i)%val); end do
    print *, ' val min/avg/max: ', r,r2,r3

    r=9e9; r2=0.0; r3=-9e9
    do i=1,c; r=min(r,obs(i)%err); r2 = r2 + (obs(i)%err-r2)/i; r3=max(r3,obs(i)%err); end do
    print *, ' err min/avg/max: ', r,r2,r3

    print *, " lon min/max: ", minval(obs(:)%lon),maxval(obs(:)%lon)
    print *, " lat min/max: ", minval(obs(:)%lat),maxval(obs(:)%lat)

    ! all done
    call check(nf90_close(ncid))
    deallocate(tmp_int,tmp1d,tmp1d2,tmp3d,qc,obs3)

  end subroutine read_sst_cmc0d2_nc





  subroutine readField2(ncid, vname, val)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: vname
    real, intent(inout) :: val(:,:)

    integer :: vid
    real :: v_offset, v_scale, v_min, v_max
    call check(nf90_inq_varid(ncid, vname, vid))
    call check(nf90_get_att(ncid, vid, "add_offset", v_offset))
    call check(nf90_get_att(ncid, vid, "scale_factor", v_scale))
    call check(nf90_get_att(ncid, vid, "valid_min", v_min))
    call check(nf90_get_att(ncid, vid, "valid_max", v_max))
    call check(nf90_inq_varid(ncid, vname, vid))
    call check(nf90_get_var(ncid, vid, val))

    where (val < v_min) val = badval
    where (val > v_max) val = badval   
    where (val /= badval) val = val*v_scale + v_offset
  end subroutine readField2


  subroutine readField3(ncid, vname, val)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: vname
    real, intent(inout) :: val(:,:,:)

    integer :: vid 
    real :: v_offset, v_scale, v_min, v_max
    !integer :: msk(size(val,1),size(val,2),size(val,3))

    call check(nf90_inq_varid(ncid, vname, vid))
    call check(nf90_get_att(ncid, vid, "add_offset", v_offset))
    call check(nf90_get_att(ncid, vid, "scale_factor", v_scale))
    call check(nf90_get_att(ncid, vid, "valid_min", v_min))
    call check(nf90_get_att(ncid, vid, "valid_max", v_max))
    call check(nf90_inq_varid(ncid, vname, vid))
    call check(nf90_get_var(ncid, vid, val))

    !print*, "vname=", vname
    !print*, "add_offset  =", v_offset
    !print*, "scale_factor=", v_scale
    !print*, "valid_min   =", v_min
    !print*, "valid_max   =", v_max
    !print*, "min, max    =", minval(val),maxval(val)

    where (val < v_min) val = badval
    where (val > v_max) val = badval   
    where (val /= badval) val = val*v_scale + v_offset
    !msk = 0 !
    !where (val ==badval) msk = 1 !
    !print*, "bad value=", sum(msk) !
    !pause "readField3" !

  end subroutine readField3



  subroutine check(status)
    integer, intent(in) :: status
    if(status/=nf90_noerr) then
       print *, status
       print *, trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check


   subroutine to_lower(str)
     character(*), intent(in out) :: str
     integer :: i
 
     do i = 1, len(str)
       select case(str(i:i))
         case("A":"Z")
           str(i:i) = achar(iachar(str(i:i))+32)
       end select
     end do  
   end subroutine to_lower

 
end module obsprep_sst_gds2
