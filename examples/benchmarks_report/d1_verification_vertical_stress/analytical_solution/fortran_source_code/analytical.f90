program transient  
use m_general_tools_cheproo 
!c--------------------------------------------------------------
!c Anlytical solution vertical stress
!c based on lemieux at el. (2008)
!c--------------------------------------------------------------
implicit none 

real*8, parameter   :: r1=1.0d0, &
                       facyear=3.1536e7, &
                       pi=3.14159265d0, &
                       g=9.81d0, &
                       r0=0.0d0, &
                       r2=2.0d0, &
                       r4=4.0d0 
real*8              :: &
 beta, &
 hyc, &
 sto, &
 dens, &
 diff, &
 dtime, &
 dsigmadt, &
 time, &
 term1, &
 term2, &
 term3, &
 term4, &
 term5, &
 zloc, &
 delta, &
 sum
integer         :: nx, &
                   ny, &
                   nz
real*8, pointer :: x(:)
real*8, pointer :: y(:)
real*8, pointer :: z(:)
real*8, pointer :: head(:,:)
character(len=100), pointer :: namevar(:)

integer       :: &
 i, &
 j, &
 k, &
 nstep, &
 ncells


dtime=r0
beta=r1
hyc=1.0d-3 !/facyear
sto=1.0d-6 
dens=1.0d3
diff=hyc/sto
nstep=1.0d4
dtime=r1 !*facyear
ncells=20
nx=1
ny=1
nz=20

allocate(x(ncells))
allocate(y(ncells))
allocate(z(ncells))
allocate(head(ncells,1))
allocate(namevar(1))
z=r0
x=r0
y=r0
head=r0
namevar='var'
delta=50.0d0
sum=-delta
do i=1,ncells
 sum=sum+delta
 z(i)=sum
end do


dsigmadt=0.3d0 
time=r0

open(unit=10,status='unknown')
open(unit=20,status='unknown')
open(unit=30,status='unknown')
open(unit=40,status='unknown')
open(unit=50,status='unknown')

head=r0

do i=1,5

   if (i==1) then
     k=10
     time=100.0d0 
   else if(i==2) then
      k=20
     time=500.0d0 
   else if(i==3) then
      k=30
     time=1000.0d0 
   else if(i==4) then
      k=40
     time=5000.0d0 
   else if(i==5) then
      k=50
     time=10000.0d0
   end if 
   
   head=r0
   
   do j=1,ncells
    zloc=z(j) !z(j)-10000.0d0
    term1=time + zloc*zloc/(r2*diff)
    term2=erfc(zloc/(r2*dsqrt(diff*time)))
    term3=dsqrt(time/(pi*diff))
    term4=dexp(-zloc*zloc/(r4*diff*time))
    term5=beta
    if (j==ncells) then
    20 continue
    end if
    !c--------------------------------------------------------------
    !c This equation was modified from equation 24 presented in 
    !c Lemieux 2008 
    !c--------------------------------------------------------------
    head(j,1)=term5*dsigmadt*(time-term1*term2+zloc*term3*term4)
   end do  
    
   call write_resultstecplot_ (time,k,'var ','var ',namevar,head,ncells,1,x,y,z,nx,nz,ny)  
 
 

end do

close(10)
close(20)
close(30)
close(40)
close(50)



10 continue 

end program

subroutine add_ (letters,int)

	implicit none
      ! External variables
	character(len=*)   :: letters
	integer            :: int
      ! Local variables
	integer            :: ipos  
!c-------------------------------------------------------------
    call lastletter_ (ipos,letters)
	write(unit=letters(ipos+1:ipos+5),fmt=5),int
!c-------------------------------------------------------------      
      
      return
    5 format(i4)
end subroutine 

 subroutine lastletter_(last,asinput)
      implicit none
      integer, intent(out)         :: last
	character(len=*), intent(in) :: asInput
	integer i,j,long
!c-------------------------------------------------------------      
	last=0
	last = index(asinput,'     ')-1
!c-------------------------------------------------------------
	return
	end subroutine
	
	 