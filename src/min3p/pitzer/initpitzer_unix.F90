!cprovi-------------------------------------------------------------------
!cprovi This subroutine initialices the aqueous phase obeject.
!cprovi HMW model (Harvie et al., 1984) was implemented according to
!cprovi Bea et al. (2009).
!cprovi 
!cprovi 
!cprovi-------------------------------------------------------------------
subroutine initpitzer  
!cprovi-------------------------------------------------------------------
!cprovi Dependence modules 
!cprovi-------------------------------------------------------------------
use parm
use gen
use chem
use dens 
!cprovi-------------------------------------------------------------------
!cprovi This modul is the species class. It is part of the CHEPROO class 
!cprovi-------------------------------------------------------------------
use m_species
!---------------------------------------------------
implicit none 
!---------------------------------------------------
! Local variables
!---------------------------------------------------
integer :: &
 nsp, &        ! Number of aqueous species
 i, &
 j, &
 isps, &
 l_string, &
 nprop, &
 ndim  
real*8, pointer :: &
 prop(:) => null(), &
 molweight(:) => null (), &
 molvol(:) => null ()
character(len=100), pointer :: &
 unitprop(:) => null (), &
 nameprop(:) => null ()
logical          :: &
 iserror
type(t_species), pointer :: &
 species(:) => null ()
character(len=100)       :: &
 nameactmodel, &
 nameconv, & 
 typephase, & 
 namephase, &
 namedatabase, &
 msg, &
 name
real*8                   :: &
 molw, &
 molv
!integer, parameter       :: &
! outscreen=6
integer :: outscreen

outscreen = ilog

!cprovi------------------------------------------------
!cprovi 
!cprovi------------------------------------------------
if (.not.ispitzer) return 
msg=' ' 
!cprovi------------------------------------------------
!cprovi Assign number of properties 
!cprovi------------------------------------------------
if (ispitzerdens) then
 nprop=3
else
 nprop=1
end if 
!cprovi------------------------------------------------
!cprovi Allocate local pointers 
!cprovi------------------------------------------------
allocate (prop(nprop))
allocate (unitprop(nprop))
allocate (nameprop(nprop))
!cprovi--------------------------------------------------------------------
!cprovi Inicialice local variables 
!cprovi--------------------------------------------------------------------
nameprop(1)='charge'
prop=0.0d0
unitprop=' '
isps=0
!cprovi--------------------------------------------------------------------
!cprovi Compute the number of aqueous species 
!cprovi-------------------------------------------------------------------- 
nsp=nc+nx 
!cprovi--------------------------------------------------------------------
!cprovi Allocate vector of species objects 
!cprovi--------------------------------------------------------------------
allocate (species(nsp))
!cprovi--------------------------------------------------------------------
!cprovi Allocate and create the aqueous phase object 
!cprovi-------------------------------------------------------------------- 
allocate (phase)
call create_ (phase) 

if (ispitzerdens) then
 nameprop(2)='molweight'
 nameprop(3)='molvol'
 unitprop(2)='gr/mol'
 unitprop(3)='cm3/mol'
 allocate(molweight(nsp))
 allocate(molvol(nsp))
 molweight=0.0d0
 molvol=0.0d0
 open(1,file=dbs_dir(:l_dbs_dir)//'/aqueousphase.dat',err=20,status='old')
 read(1,*) ndim
 do i=1,ndim 
   read(1,*) name,molw,molv
   do j=1,nc
     if (name==namec(j)) then
        molweight(j)=molw    
        molvol(j)=molv
        exit    
     end if 
   end do
   do j=1,nx
     if (name==namex(j)) then
        molweight(nc+j)=molw    
        molvol(nc+j)=molv
        exit    
     end if 
   end do
 end do 
 close(1)
end if 
!cprovi--------------------------------------------------------------------
!cprovi Create and set primary species objects  
!cprovi--------------------------------------------------------------------
do i=1,nc
 isps=isps+1
 call create_ (species(isps))
 prop(1)=chargec(i) 
 if (ispitzerdens) then 
   prop(2)=molweight(i)
   prop(3)=molvol(i)
 end if 
!%-------------------------------------------------------------------------
!%
!%-------------------------------------------------------------------------
 call set_name_ (species(isps),namec(i))
 call set_prop_ (species(isps),prop,nameprop,unitprop,nprop) 
end do
!cprovi--------------------------------------------------------------------
!cprovi Create and set aqueous complexes species objects  
!cprovi--------------------------------------------------------------------
do i=1,nx
 isps=isps+1
 prop(1)=chargex(i)
 if (ispitzerdens) then 
   prop(2)=molweight(nc+i)
   prop(3)=molvol(nc+i)
 end if 
 call create_ (species(isps))
 call set_name_ (species(isps),namex(i))
 call set_prop_ (species(isps),prop,nameprop,unitprop,nprop) 
end do 
!cprovi--------------------------------------------------------------------
!cprovi Set the aqueous phase object
!cprovi This service also read the virial coefficients database 
!cprovi--------------------------------------------------------------------
nameactmodel='pitzer'
if (ismacinnes) then
 nameconv='macinnes'
else
 nameconv=' ' 
end if  
typephase='aqueous'
namephase='aqueous phase' 
namedatabase=dbs_dir
l_string = index(namedatabase,'  ')-1
namedatabase=namedatabase(:l_string)//'/pitzer.xml'
if(rank == 0 .and. b_enable_output) then
  write(outscreen,*)'------------------------------------------------------------------------'
  write(outscreen,*) 
  write(outscreen,*)'Pitzer model is used'
end if
call set_ (phase,species,nsp,0,namephase,typephase,nameactmodel,unitprop, &
           unitprop,prop,nameconv,iserror,namedatabase=namedatabase)
if (iserror) then 
   msg='Error when call service set_ in the aqueous phase object'
   goto 10
end if
!cprovi--------------------------------------------------------------------
!cprovi Write on the screen the information contained in the aqueous 
!cprovi in the aqueous phase object 
!cprovi--------------------------------------------------------------------
call write_ (phase,outscreen,iserror)
if (iserror) then 
   msg='Error when call service write_ in the aqueous phase object'
   goto 10
end if
if (rank == 0 .and. b_enable_output) then
  write(6,*)'------------------------------------------------------------------------'
end if
!cprovi-------------------------------------------------------------------
!cprovi Write attributes contained in the phase object in an ascii file
!cprovi igen file 
!cprovi-------------------------------------------------------------------
call write_ (phase,igen,iserror)
if (iserror) then 
   msg='Error when call service write_ in the aqueous phase object'
   goto 10
end if
10 continue 
!cprovi-------------------------------------------------------------------
!cprovi Destroy local species objects 
!cprovi-------------------------------------------------------------------
do i=1,nsp
 call destroy_ (species(i))
end do
!cprovi-------------------------------------------------------------------
!cprovi Deallocate local variables
!cprovi-------------------------------------------------------------------
deallocate (prop) 
deallocate (unitprop)
deallocate (nameprop) 
deallocate (species)
if (ispitzerdens) then
 deallocate(molweight)
 deallocate(molvol)
end if 
if (iserror) then 
  write(ilog,*) msg
  stop
end if 

return
20 continue
msg='Error when open aqueosphase.dat data base (for density based on Pitzer equations calculations)'
write(ilog,*) msg
stop 
end subroutine