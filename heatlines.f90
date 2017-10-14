program heatlines
!--------------------------
!for plotting heatlines in 2d domain
!--------------------------
!started 18May2016
!latest modified 22June2016
!requires dump.csv
IMPLICIT NONE

integer::Npts
real::kth,Cp,rho,Tref,Atn
real,dimension(10000,2)::gradT,vel,coord
real,dimension(10000)::T
real,allocatable,dimension(:,:)::varnew
!double precision,parameter::pi=3.141592653

CALL csvdata
CALL heatline
CALL visualization(39,39)
!pause
contains

subroutine heatline
!-----------------
!gradT(1)=> dT/dx
!gradT(2)=> dT/dy
!vel(1)=> U
!vel(2)=> V
!-----------------
integer::ii
real::rhocp
rho = 1.225
Cp = 1005.0
kth= 0.02568
Tref = 287.5
rhocp = rho*Cp
do ii = 1, Npts !heatfn. component calculation for every node
varnew(ii,1) = rhocp*vel(ii,1)*(T(ii)-Tref)-kth*gradT(ii,1)
varnew(ii,2) = -rhocp*vel(ii,2)*(T(ii)-Tref)+kth*gradT(ii,2)
end do

end subroutine heatline

subroutine tecplot
integer::ii,jj,counter
integer:: Ni,Nj
counter = 0
Ni = 65
Nj = 95

open(unit=4,file='tecplotdat.dat',status='unknown') 
!open(unit=5,file='tecplot2.dat',status='unknown') 
!open(unit=6,file='tecplot3.dat',status='unknown') 
write(4,*) '"TITLE = tecplot data field"' 
write(4,*) "VARIABLES = ",'"X", "Y", "U", "V", "T"'
write(4,*) '"ZONE T= BIG" ',"I=",Ni,", J=",Nj,", DATAPACKING=POINT"
do ii = 1,Ni
do jj = 1,Nj
counter = counter + 1
!WRITE(4,*) coord(counter,1),coord(counter,2),atan2(-varnew(counter,2),varnew(counter,1)),sqrt(varnew(counter,2)*varnew(counter,2)+varnew(counter,1)*varnew(counter,1))
WRITE(4,200) coord(counter,1),coord(counter,2),varnew(counter,1),varnew(counter,2),T(counter)
!WRITE(5,*) coord(counter,1),coord(counter,2),atan2(vel(counter,2),vel(counter,1)),sqrt(vel(counter,2)*vel(counter,2)+vel(counter,1)*vel(counter,1))
!WRITE(6,*) coord(counter,1),coord(counter,2),T(counter)
end do
end do
close(4)
200 format(f10.6,1X,f10.6,1X,f10.6,1X,f10.6,1X,f10.6)
end subroutine tecplot

subroutine csvdata
integer::ii,counter,IOstatus
!-----------------------------
!commands to read the input file
!-----------------------------
ii = 0
Npts = 0
open (2,file='dump.csv',status='old')
do 
ii = ii + 1
!for heatlines
read(2,*,IOSTAT=IOstatus)T(ii),gradT(ii,1),gradT(ii,2),vel(ii,1),vel(ii,2),coord(ii,1),coord(ii,2)

   IF (IOstatus > 0)  THEN
   
      print*,"remove header in dump.pour"
	  stop
      exit
      
   ELSE IF (IOstatus < 0) THEN
   
      print*,"end of file reached"
	  print*," "
      print*,"total no of data points:",Npts
	  print*," "
      exit
   ELSE
   Npts = Npts + 1   
      
   END IF

end do

close (2)
!--------------

!--------------
Allocate(varnew(Npts,2))
end subroutine csvdata

subroutine visualization(Nmax,Mmax)
integer::ii,jj,kk
integer,intent(in)::Nmax,Mmax
!-------------------------
kk = 0
!-------------------------
OPEN(UNIT=4,FILE='tecplotdat.dat', STATUS='UNKNOWN')
WRITE(4,'(A)') 'VARIABLES='
WRITE(4,'(A)') 'X'         
WRITE(4,'(A)') 'Y'
WRITE(4,'(A)') 'U' 
WRITE(4,'(A)') 'V'
WRITE(4,'(A)') 'T'     
WRITE(4,'(A)') 'ZONE'
WRITE(4,'(A,I6)') 'I=',  Nmax
WRITE(4,'(A,I6)') 'J=',  Mmax
WRITE(4,'(A)') 'F=POINT'
    
	   
          DO JJ=1, Mmax
		  
            DO II=1, Nmax
            kk = kk + 1
		  !for heatlines
             WRITE(4,200) coord(kk,1),coord(kk,2),varnew(kk,1),-varnew(kk,2),T(kk)

			END DO

		  END DO
200 format(e15.7,1X,e15.7,1X,e15.7,1X,e15.7,1X,e15.7)
end subroutine visualization
 

END

