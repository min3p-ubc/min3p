      module m_general_tools_cheproo 
c-------------------------------------------------------------      
	private ::
c-------------------------------------------------------------
      public ::
     . add_,
     . lowercase,
     . check_pointer_,
     . lastletter_,
     . firstletter_, 
     . mprove, 
     . find,
     . compute_hash_,
     . write_,
     . add_diagonal_,
     . compute_iamount_,
     . write_visualmeshplot_,
     . write_resultsgid_,
     . write_resultstecplot_, 
     . write_meshgid_,
     . find_repeated_,
     . write_newton_raphson_info_, 
     . findsingrow,
     . findInArray_,
     . temperature_
c-------------------------------------------------------------
      interface add_
       module procedure add_int_  
	 module procedure add_real_
	 module procedure add_char_ 
	 module procedure add_vector_ 
	 module procedure add_array_  
	end interface
c-------------------------------------------------------------
	interface lastletter_
       module procedure lastletter_
	end interface
c-------------------------------------------------------------
	interface firstletter_
       module procedure firstletter_
	end interface
c-------------------------------------------------------------
	interface find_repeated_
       module procedure find_repeated_
	end interface
c-------------------------------------------------------------
      interface check_pointer_
       module procedure check_pointer_vi_
	 module procedure check_pointer_vr_
	 module procedure check_pointer_vch_
	 module procedure check_pointer_ai_ 
	 module procedure check_pointer_ar_
	 module procedure check_pointer_ach_ 
	 module procedure check_pointer_a3ch_ 
	 module procedure check_pointer_a3r_
	 module procedure check_pointer_vb_
       module procedure check_pointer_ab_
      end interface
c-------------------------------------------------------------
      interface compute_hash_
       module procedure compute_hash_
      end interface
c-------------------------------------------------------------
      interface write_
       module procedure write_in_row_txt1_
	 module procedure write_in_row_txt2_ 
       module procedure write_in_row_txt3_ 
       module procedure write_table_txt_ 
      end interface
c-------------------------------------------------------------
      interface add_diagonal_
       module procedure add_diagonal_
      end interface
c-------------------------------------------------------------
      interface compute_iamount_
       module procedure compute_iamount_
      end interface	
c-------------------------------------------------------------
      interface write_visualmeshplot_
       module procedure write_visualmeshplot_
      end interface
c-------------------------------------------------------------
      interface write_resultstecplot_
       module procedure write_resultstecplot_tools 
      end interface
c-------------------------------------------------------------
      interface write_resultsgid_
       module procedure write_resultsgid_tools
      end interface
c-------------------------------------------------------------
      interface write_meshgid_
       module procedure write_meshgid_tools
      end interface
c-------------------------------------------------------------
      interface write_newton_raphson_info_
       module procedure write_newton_raphson_info_
      end interface
c-------------------------------------------------------------
      interface mprove
       module procedure mprove
      end interface      
c-------------------------------------------------------------
      interface findsingrow
       module procedure findsingrow
      end interface            
c-------------------------------------------------------------
      interface findInArray_
       module procedure findInArray_Char
      end interface
c-------------------------------------------------------------
      interface temperature_
       module procedure temperature_
      end interface            
c-------------------------------------------------------------


      contains 
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
      subroutine lowercase(s)


      implicit none
      character(len = *)                :: s
      character(len = len(s))           :: sloc
      integer                           :: i,i1,i2,j


      i1 = 1
      i2 = len(s)

      

      do i=i1,i2        
	   j = iachar(s(i:i))
         select case(j)
         case(65:90)
             sloc(i:i) = achar(j+32)
         
	   case default
	       sloc(i:i) = achar(j)
	   end select
      enddo
  
      s=sloc
      
      return
      end subroutine
**************************************************************
*****************Public function****************************** 
**************************************************************
**************************************************************
**************************************************************
*     Find the last letter         
**************************************************************
      subroutine lastletter_(last,asinput)
      implicit none
      integer, intent(out)         :: last
	character(len=*), intent(in) :: asInput
	integer i,j,long
c-------------------------------------------------------------      
	last=0
	last = index(asinput,'     ')-1
c-------------------------------------------------------------
	return
	end subroutine
**************************************************************
*****************Public function****************************** 
**************************************************************
**************************************************************
**************************************************************
*     Find the last letter         
**************************************************************
      subroutine firstletter_(first,asinput)
      implicit none
      integer   first 
	character*(*) asInput
	integer i,long 
c-------------------------------------------------------------      
	long=len(asinput)
	first=0 
	if (long==0) return  
	do i=1,long
       if (asinput(i:i)/=' ') then
	  first=i 
	  return
	 end if 
	end do
c-------------------------------------------------------------
	return
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
      subroutine find(s,letters,be)


      implicit none
      character(len = *)                :: s, letters
      character(len = len(s))           :: sloc
	integer                           :: ipos
      logical                           :: be

      be=.false. 
      
      ipos=index(s,letters)
      
	if (ipos.ne.0) be=.true. 
      
      return
      end subroutine
**************************************************************
*********************Public subroutine************************
**************************************************************
**************************************************************
**************************************************************
*    Add integer in string          
**************************************************************
	subroutine add_int_ (letters,int)

	implicit none
      ! External variables
	character(len=*)   ::
     . letters
	integer            ::
     . int
      ! Local variables
	integer            ::
     . ipos  
c-------------------------------------------------------------
      call lastletter_ (ipos,letters)
	write(unit=letters(ipos+1:ipos+5),fmt=5),int
c-------------------------------------------------------------      
      
      return
    5 format(i5)
	end subroutine
**************************************************************
*********************Public subroutine************************
**************************************************************
**************************************************************
**************************************************************
*    Add real in string          
**************************************************************
	subroutine add_real_ (letters,r)

	implicit none
      ! External variables
	character(len=*)   ::
     . letters
	real*8             ::
     . r
      ! Local variables
	integer            ::
     . ipos
c-------------------------------------------------------------
      call lastletter_ (ipos,letters)
	write(unit=letters(ipos:ipos+5),fmt=5), r
c-------------------------------------------------------------	
      return
    5	format(e10.3) 
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*     Add string to another string       
**************************************************************
	subroutine add_char_ (letters,add)

	implicit none
      ! External variables
	character(len=*)   ::
     . add
	character(len=*)   ::
     . letters
      ! Local variables
      integer            ::
     . ipos1,
     . ipos2
c-------------------------------------------------------------
      call lastletter_ (ipos1,letters)
	call lastletter_ (ipos2,add)
	letters=letters(1:ipos1)//add(1:ipos2)
c-------------------------------------------------------------      
      return
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine check_pointer_vr_ (pa,ndim,isallocate)

	implicit none
      ! External variables
	integer, intent(in)            ::
     . ndim
	real*8, pointer    ::
     . pa(:)
      logical, intent(in)            ::
     . isallocate
      integer            ::
     . stat

      if (isallocate) then
	 if (associated(pa)) then
         if (size(pa).ne.ndim) then

		   deallocate (pa,stat=stat)
	       if (stat>0) goto 10
		   allocate (pa(ndim),stat=stat) 

	       if (stat>0) goto 10
         end if
	 else
        allocate (pa(ndim),stat=stat)

	  if (stat>0) goto 10
	 end if
       pa=0.0d0   
	else
	  if (associated(pa)) then
	   deallocate (pa,stat=stat)
         if (stat>0) goto 10
	  end if
	  pa => null () 
	  
	end if

c-----------------------------------------------------------
      return
   10 print*,'Error, problems when allocate/deallocate memory'//
     .       ', stat:',stat
      stop
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine check_pointer_vi_ (pa,ndim,isallocate)

	implicit none
      ! External variables
	integer, pointer   ::
     . pa(:)
	integer            ::
     . ndim
      logical            ::
     . isallocate
      integer            ::
     . stat
c----------------------------------------------------------
      if (isallocate) then
	 if (associated(pa)) then
           if (size(pa).ne.ndim) then
	       deallocate (pa,stat=stat)
	       
		   if (stat>0) goto 10
		   allocate (pa(ndim),stat=stat)
	       
	       if (stat>0) goto 10
		 end if 
	 else
        allocate (pa(ndim),stat=stat)
	  
	  if (stat>0) goto 10 
	 end if
       
	 pa=0
	
	
	else
	  if (associated(pa)) then
	   deallocate (pa,stat=stat)
	   if (stat>0) goto 10
	  end if
	  pa => null () 
	  
	end if
c-----------------------------------------------------------
      return
   10 print*,'Error, problems when allocate/deallocate memory'//
     .       ', stat:',stat
      stop 
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine check_pointer_vch_ (pa,ndim,isallocate)

	implicit none
      ! External variables
	character(len=*), pointer   ::
     . pa(:)
	integer            ::
     . ndim
      logical            ::
     . isallocate
      integer            ::
     . stat

      if (isallocate) then
	 if (associated(pa)) then
        if (size(pa).ne.ndim) then
	       deallocate (pa,stat=stat)
	       
	       if (stat>0) goto 10  
		   allocate (pa(ndim),stat=stat)
	       
	       if (stat>0) goto 10
		   pa=' '        
        end if 
	 else
        allocate (pa(ndim),stat=stat) 
	  
	  if (stat>0) goto 10
	 end if
      
	 pa=' ' 
	
	else
	  if (associated(pa)) then
	   deallocate (pa,stat=stat)
	   if (stat>0) goto 10
	  end if
	  pa => null () 
	  
	end if
c-----------------------------------------------------------
      return
   10 print*,'Error, problems when allocate/deallocate memory'//
     .       ', stat:',stat
      stop 
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine check_pointer_a3ch_ (pa,ndim1,ndim2,ndim3,isallocate)

	implicit none
      ! External variables
	character(len=*), pointer   ::
     . pa(:,:,:)
	integer            ::
     . ndim1,
     . ndim2,
     . ndim3
      logical            ::
     . isallocate
      integer            ::
     . stat

      if (isallocate) then
	 if (associated(pa)) then
        if (size(pa,1).ne.ndim1.or.size(pa,2).ne.ndim2.or.
     .      size(pa,3).ne.ndim3) then
	       deallocate (pa,stat=stat)
	       
	       if (stat>0) goto 10  
		   allocate (pa(ndim1,ndim2,ndim3),stat=stat)
	       
	       if (stat>0) goto 10
		   pa=' '        
        end if 
	 else
        allocate (pa(ndim1,ndim2,ndim3),stat=stat)
	  
	  if (stat>0) goto 10
	 end if
       pa=' ' 
	else
	  if (associated(pa)) then
	   deallocate (pa,stat=stat)
	   if (stat>0) goto 10
	  end if
	  pa => null () 
	  
	end if
c-----------------------------------------------------------
      return
   10 print*,'Error, problems when allocate/deallocate memory'
      stop 
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine check_pointer_ar_ (pa,ndim1,ndim2,isallocate)
 
	implicit none

      ! External variables
	real*8, pointer    ::
     . pa(:,:)
	integer            ::
     . ndim1,
     . ndim2
      logical            ::
     . isallocate
      integer            ::
     . stat

      if (isallocate) then
	 if (associated(pa)) then
        if (size(pa,1).ne.ndim1.or.size(pa,2).ne.ndim2) then
		deallocate (pa,stat=stat)
	   
	    if (stat>0) goto 10
		allocate (pa(ndim1,ndim2),stat=stat)
	   
	    if (stat>0) goto 10
        end if 
	 else
        allocate (pa(ndim1,ndim2),stat=stat) 
	  
	  if (stat>0) goto 10
	 end if
       
	  pa=0.0d0
	
	else
	  if (associated(pa)) then
	    deallocate (pa,stat=stat)
	    if (stat>0) goto 10
	  end if
	  pa => null () 
	  
	end if
c-----------------------------------------------------------
      return
   10 print*,'Error, problems when allocate/deallocate memory'//
     .       ', stat:',stat
      stop 
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine check_pointer_a3r_ (pa,ndim1,ndim2,ndim3,isallocate)
 
	implicit none

      ! External variables
	real*8, pointer    ::
     . pa(:,:,:)
	integer            ::
     . ndim1,
     . ndim2,
     . ndim3
      logical            ::
     . isallocate
      integer            ::
     . stat

      if (isallocate) then
	 if (associated(pa)) then
        if (size(pa,1).ne.ndim1.or.size(pa,2).ne.ndim2.or.
     .      size(pa,3).ne.ndim3) then
	    deallocate (pa,stat=stat)
	    
	    if (stat>0) goto 10
		allocate (pa(ndim1,ndim2,ndim3),stat=stat)
	    
	    if (stat>0) goto 10
        end if 
	 else
        allocate (pa(ndim1,ndim2,ndim3),stat=stat) 
	  
	  if (stat>0) goto 10
	 end if
       pa=0.0d0
	else
	  if (associated(pa)) then
	   deallocate (pa,stat=stat)
	   if (stat>0) goto 10
	  end if
	  pa => null () 
	  
	end if
c-----------------------------------------------------------
      return
   10 print*,'Error, problems when allocate/deallocate memory'//
     .       ', stat:',stat
      stop 
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine check_pointer_ai_ (pa,ndim1,ndim2,isallocate)
 
	implicit none

      ! External variables
	integer, pointer   ::
     . pa(:,:)
	integer            ::
     . ndim1,
     . ndim2
      logical            ::
     . isallocate
      integer            ::
     . stat

      if (isallocate) then
	 if (associated(pa)) then
        if (size(pa,1).ne.ndim1.or.size(pa,2).ne.ndim2) then
	       deallocate (pa,stat=stat)
	       
	       if (stat>0) goto 10
		   allocate (pa(ndim1,ndim2),stat=stat) 
	       
	       if (stat>0) goto 10
	  end if  
	 else
        allocate (pa(ndim1,ndim2),stat=stat)
	  
	  if (stat>0) goto 10
	 end if
       
	 pa=0
	
	else
	  if (associated(pa)) then
	   deallocate (pa,stat=stat)
	   if (stat>0) goto 10
	  end if
	  pa => null () 
	  
	end if
c-----------------------------------------------------------
      return
   10 print*,'Error, problems when allocate/deallocate memory'//
     .       ', stat:',stat
      stop 
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine check_pointer_ach_ (pa,ndim1,ndim2,isallocate)
 
	implicit none

      ! External variables
	character(len=100), pointer   ::
     . pa(:,:)
	integer            ::
     . ndim1,
     . ndim2
      logical            ::
     . isallocate
      integer            ::
     . stat

      if (isallocate) then
	 if (associated(pa)) then
        if (size(pa,1).ne.ndim1.or.size(pa,2).ne.ndim2) then
	    deallocate (pa,stat=stat)
	   
	    if (stat>0) goto 10
		allocate (pa(ndim1,ndim2),stat=stat)
	   
	    if (stat>0) goto 10
        end if
	 else
        allocate (pa(ndim1,ndim2),stat=stat)
	  
	  if (stat>0) goto 10
	 end if
       pa=' '
	else
	  if (associated(pa)) then
	    deallocate (pa,stat=stat) 
	    if (stat>0) goto 10
	  end if
	  pa => null () 
	  
	end if

c-----------------------------------------------------------
      return
   10 print*,'Error, problems when allocate/deallocate memory'//
     .       ', stat:',stat
      stop 
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine check_pointer_vb_ (pa,ndim,isallocate)

	implicit none
      ! External variables
	logical, pointer   ::
     . pa(:)
	integer            ::
     . ndim
      logical            ::
     . isallocate
      integer            ::
     . stat

      if (isallocate) then
	 if (associated(pa)) then
        if (size(pa).ne.ndim) then
	       deallocate (pa,stat=stat)
	       
	       if (stat>0) goto 10
		   allocate (pa(ndim),stat=stat)
	       
	       if (stat>0) goto 10
        end if 
	 else 
        allocate (pa(ndim),stat=stat)
	  
	  if (stat>0) goto 10 
	 end if
       pa=.false.
	else
	  if (associated(pa)) then
	   deallocate (pa,stat=stat)
	   if (stat>0) goto 10
	  end if
	  pa => null () 
	  
	end if
c-----------------------------------------------------------
      return
   10 print*,'Error, problems when allocate/deallocate memory'//
     .       ', stat:',stat
      stop 
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine check_pointer_ab_ (pa,ndim1,ndim2,isallocate)

	implicit none
      ! External variables
	logical, pointer   ::
     . pa(:,:)
	integer            ::
     . ndim1,
     . ndim2
      logical            ::
     . isallocate
	integer            ::
     . stat

      if (isallocate) then
	 if (associated(pa)) then
        if (size(pa,1).ne.ndim1.and.size(pa,2).ne.ndim2) then
	       deallocate (pa,stat=stat)
	      
		   allocate (pa(ndim1,ndim2),stat=stat)
	      
	       if (stat>0) goto 10 
        end if 
	 else 
        allocate (pa(ndim1,ndim2),stat=stat)
	  
	  if (stat>0) goto 10
	 end if
       pa=.false.
	else
	  if (associated(pa)) then
	    deallocate (pa,stat=stat)
	    if (stat>0) goto 10
	  end if
	  pa => null () 
	  
	end if

c-----------------------------------------------------------
      return
   10 print*,'Error, problems when allocate/deallocate memory'//
     .       ', stat:',stat
      stop  
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
*             
**************************************************************
	subroutine add_vector_ (vinout,alpha,vin,ndim)
 
	implicit none

      ! External variables
	
	integer, intent(in)            :: ndim
	
	real*8, intent(inout)          :: vinout(ndim)
	
      real*8, intent(in)             :: vin(ndim)
      
	real*8, intent(in)             :: alpha
c-----------------------------------------------------------
	vinout=vinout+alpha*vin
c-----------------------------------------------------------
      return
	end subroutine
**************************************************************
*****************Public subroutine**************************** 
**************************************************************
**************************************************************
**************************************************************
	subroutine add_array_ (ainout,alpha,ain,ndim1,ndim2)
 
	implicit none

      ! External variables
	integer, intent(in)            :: ndim1
      
      integer, intent(in)            :: ndim2
      
	real*8, intent(inout)          :: ainout(ndim1,ndim2)
	
      real*8, intent(in)             :: ain(ndim1,ndim2)
      
	real*8, intent(in)             :: alpha
c-----------------------------------------------------------
	ainout=ainout+alpha*ain
c-----------------------------------------------------------
      return
	end subroutine
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
*
*    Compute univic number that represent one combination of
*    things according the global arrengment. 
*     
*    hash_=sum(idthing)*numthings
*
**************************************************************
      subroutine compute_hash_ (hash,idthing,numthing,typefunction)
      implicit none
      ! External variables
	integer                 ::
     . numthing,
     . hash,
     . typefunction
	integer                 ::
     . idthing(numthing)
      ! Local variables
	integer                 ::
     . i,
     . indx
c--------------------------------------------------------------     
	hash= 0
	do i=1,numthing
	   indx=idthing(i)
         hash=hash+indx
	end do
	hash=hash*numthing
c--------------------------------------------------------------	
	return
	end subroutine
!%************************************************************
!%***************Private function*****************************
!%************************************************************
!%************************************************************
!%************************************************************
      double precision function temperature_ (p0,temp,coeff,ncoeff)
 
      implicit none
!-------------------------------------------------------------------------
!
!   $Description: Compute the parameter dependence with the temperature 
! according to the following expression
!
!   p(T) = p0 + a1*T + a2*T^2 + a3*T^3 + a4*T^4
!
!   This function accepts an either number of coefficients  
!
!   $Arguments:
!
 
      real*8, intent(in)                      :: p0      ! Reference parameter  

	real*8, intent(in)                      :: temp    ! Temperature in celcius 

      real*8, intent(in)                      :: ncoeff  ! Number of coefficents 
	
	real*8, intent(in), dimension(ncoeff)   :: coeff   ! Coefficients    
!-------------------------------------------------------------------------
!
!   $Pre-cond:
!
!   $Post-cond:
!
!   $License:
!
!-------------------------------------------------------------------------
      integer              :: i
!-------------------------------------------------------------------------
!
!   $code
!
      temperature_=p0 
      
	do i=1,ncoeff
       
	 temperature_=temperature_+coeff(i)*temp**i

	end do
!%------------------------------------------------------------
      return
      end function
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
*    
**************************************************************
	subroutine write_in_row_txt1_
     .  (ioutput, 
     .   title, 
     .   name,
     .   value,
     .   ndim,
     .   integerlabel,
     .   writename,
     .   writetitle)
	
      implicit none
	! External variables
	integer                ::
     . ndim,
     . ioutput,
     . integerlabel 
	character(len=*)       ::
     . name(ndim)
	character(len=*)       ::
     . title
	real*8                 ::
     . value(ndim)
	logical                ::
     . writename,
     . writetitle
      ! Local variables
      integer                ::
     . i 
c----------------------------------------------------------
      if (writetitle) then
       write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'
	 write (ioutput,*) title 
	 write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'
	end if
c----------------------------------------------------------
      if (writename) then
       
	 write (ioutput,1) (name(i),i=1,ndim)
	 
	end if
c----------------------------------------------------------
	write (ioutput,2) integerlabel, (value(i),i=1,ndim)
c----------------------------------------------------------	
	return
    1 format(7x,<ndim>a15)	
    2 format(i5,<ndim>e15.7)	
    

      end subroutine
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
	subroutine write_in_row_txt2_
     .  (ioutput, 
     .   title, 
     .   titlecol,
     .   valuecol,
     .   ncol,
     .   reallabel,
     .   writetitlecol,
     .   writetitle)
	
      implicit none
	! External variables
	integer                ::
     . ncol,
     . ioutput
	real*8                 ::
     . reallabel
	character(len=*)       ::
     . titlecol(ncol)
	character(len=*)       ::
     . title
	real*8                 ::
     . valuecol(ncol)
	logical                ::
     . writetitlecol,
     . writetitle
      ! Local variables
      integer                ::
     . i 
c----------------------------------------------------------
      if (writetitle) then
       write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'
	 write (ioutput,*) title 
	 write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'
	end if
c----------------------------------------------------------
      if (writetitlecol) then
       
	 write (ioutput,1) (titlecol(i),i=1,ncol)
	 write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'

	end if
c----------------------------------------------------------
	write (ioutput,2) reallabel, (valuecol(i),i=1,ncol)
c----------------------------------------------------------	
	return
    1 format(15x,<ncol>a15)	
    2 format(e15.5,<ncol>e15.7)	
    

      end subroutine
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
	subroutine write_in_row_txt3_
     .  (ioutput, 
     .   title, 
     .   name,
     .   value,
     .   ndim,
     .   charlabel,
     .   writename,
     .   writetitle)
	
      implicit none
	! External variables
	integer                ::
     . ndim,
     . ioutput
	character(len=*)       ::
     . name(ndim)
	character(len=*)       ::
     . title,
     . charlabel 
	real*8                 ::
     . value(ndim)
	logical                ::
     . writename,
     . writetitle
      ! Local variables
      integer                ::
     . i,
     . lastpos
c----------------------------------------------------------
	call lastletter_(lastpos,charlabel)
c----------------------------------------------------------
      if (writetitle) then
       write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'
	 write (ioutput,*) title 
	 write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'
	end if
c----------------------------------------------------------
      if (writename) then
       
	 write (ioutput,1) (name(i),i=1,ndim)
	 write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'

	end if
c----------------------------------------------------------
	write (ioutput,2) charlabel, (value(i),i=1,ndim)
c----------------------------------------------------------	
	return
    1 format(<lastpos>x,<ndim>a15)	
    2 format(a<lastpos>,<ndim>e15.7)	
    

      end subroutine
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
	subroutine write_table_txt_
     .  (ioutput, 
     .   title, 
     .   titlecol,
     .   titlerow, 
     .   value,
     .   ncol,
     .   nrow)
	
      implicit none
	! External variables
	integer                ::
     . ncol,
     . nrow, 
     . ioutput
	character(len=*)       ::
     . titlecol(ncol),
     . titlerow(nrow)
	character(len=*)       ::
     . title
	real*8                 ::
     . value(nrow,ncol)
      ! Local variables
      integer                ::
     . i,
     . j,
     . k   
	real*8, pointer        ::
     . valueloc(:) => null ()
	character(len=100)     ::
     . label 
c-----------------------------------------------write title
      write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'
	write (ioutput,*) title 
	write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'  
c-------------------------------------write title of column       
	write (ioutput,1) (titlecol(i),i=1,ncol)
	write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'
c----------------------------------------------------------      
	allocate (valueloc(ncol))
c----------------------------------------------------------
      do i=1,nrow
	 do j=1,ncol
         valueloc(j)=value(i,j)
	 end do
	 label=titlerow(i)
	 write (ioutput,2) label,(valueloc(k),k=1,ncol)     
	end do
c----------------------------------------------------------
	write (ioutput,*) '--------------------------------'//
     .                  '--------------------------------'
c----------------------------------------------------------	     
	deallocate(valueloc)
c----------------------------------------------------------	
	return

    1 format(10x,<ncol>a15)	
    2 format(a10,<ncol>e15.7)	
    

      end subroutine
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
*    Add in diagonal some value
**************************************************************
	subroutine add_diagonal_
     .  (array,
     .   value,
     .   ndim)
	
      implicit none
	! External variables
	integer            ::
     . ndim
	real*8             ::
     . array(ndim,ndim)
      real*8             ::
     . value
      ! Local variables
	integer            ::
     . i  	
c----------------------------------------------------------
	do i=1,ndim
       array(i,i)=array(i,i)+value
	end do
c----------------------------------------------------------	
	return

      end subroutine
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
	subroutine compute_iamount_
     .  (iamount, 
     .   amount,
     .   nstep,
     .   istep,
     .   expo)
	
      implicit none
	! External variables
	integer            ::
     . istep,
     . nstep
	real*8             ::
     . iamount,
     . amount,
     . expo 
      ! Local variables
	integer            ::
     . i  	
	real*8             ::
     . factor
c----------------------------------------------------------
	factor=real(istep-nstep)/real(istep)
	iamount=amount*expo**factor
c----------------------------------------------------------	
	return

      end subroutine 
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
*     Find the first repeated component ijn list 
**************************************************************
	subroutine find_repeated_
     .  (list, 
     .   isrepeated,
     .   ndim,
     .   namerepeated)
	
      implicit none
	! External variables
	integer, intent(in)              ::
     . ndim
	character(len=*), intent(in)     ::
     . list(ndim)
	character(len=*), intent(out)    ::
     . namerepeated
      logical                          ::
     . isrepeated
      ! Local variables
	integer                          ::
     . i,
     . j,
     . itime  
	character(len=100)               ::
     . name 
c----------------------------------------------------------
	namerepeated='' 
      isrepeated=.false. 
c----------------------------------------------------------	
	do i=1,ndim 
       itime=0 
	 name=list(i)
	 do j=1,ndim
        if (name==list(j)) then
	   itime=itime+1
	  end if 
	 end do
	 if (itime>1) then
	  isrepeated=.true.  
	  namerepeated=name 
	  exit 
	 end if 
	end do
c----------------------------------------------------------	
	return
      end subroutine 
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
* PURPOSE 
*    
*     Write Visual Meshplot file
*
* DESCRIPTION 
*    
*     Write Visual Meshplot file
*
*
* EXTERNAL VARIABLES: SCALARS
*
*  IWRTOPT                1. Flow
*                         2. Flow and transport.
*  IORTS        Transport regime.
*                 0. Steady state transport.
*                 1. Transient transport with prescribed initial 
*                    conditions.
*                 2. Transient transport with steady-state initial 
*                    conditions.
*
*  ISOLEQ       Array containing the type of head/concentration
*               solution desired for the user at each obs. time
*                 0. Transient
*                 1. Steady state.
*                 2. Read initial conditions
*                 3. Null initial conditions
*                 4. Not solve.
*
* HISTORY: 
*           JHG
**************************************************************
      SUBROUTINE WRITE_VISUALMESHPLOT_ 
     . (IOUTPUT, 
     .  DIMFILE,
     .  IAUX,
     .  ISOLEQ,
     .  ISTATEVAR,
     .  KXX,
     .  LMXNDL,
     .  LNNDEL,
     .  NINT,
     .  NUMEL,
     .  NUMNP,
     .  TIME,
     .  VCALIT,
     .  X,
     .  Y)

       IMPLICIT NONE

       INTEGER*4::IOUTPUT, IROOTLEN,NUMNP,IAUX,NNUD,INODE,L,LENVARUNITS
     &           ,NINT,ISTATEVAR,NUMEL,LMXNDL,ISTEP,I,IWIDTH

	 INTEGER*4::ISOLEQ(NINT,4),LNNDEL(NUMEL),KXX(LMXNDL,NUMEL)

	 REAL*8::VCALIT(NUMNP),TIME(NINT),X(NUMNP),Y(NUMNP)

       CHARACTER::VMSHFILE*20,DIMFILE*20,TYPEL*5,VARUNITS*16,STEPN*5


C------------------------- Rewinds auxiliar files
	REWIND(IAUX)


C------------------------- Opens file. Takes root from DIM.DAT file.

	 IROOTLEN=INDEX(DIMFILE,'DIM',BACK=.TRUE.)-1
	 IF(ISTATEVAR.GT.0) THEN
		VMSHFILE = DIMFILE(1:IROOTLEN) //'VMSHC.INP'
	    VARUNITS = "Mass Fraction, -"
	    LENVARUNITS = 16
	 ELSE
		VMSHFILE = DIMFILE(1:IROOTLEN) //'VMSHH.INP'
	    VARUNITS = "Head, m"
	    LENVARUNITS = 7
	 END IF


	OPEN(UNIT=IOUTPUT,FILE=VMSHFILE,STATUS='UNKNOWN')
	
C------------------------- Writes INP header
C------------------------- Only data is suppose to vary in time	 
      WRITE(IOUTPUT,10) NINT-1
10    FORMAT('# VISUAL MESHPLOT FILE',/,I5,/,'data')

C------------------------- Mesh is included in the first step


C------------------------- Mesh is written

      WRITE(IOUTPUT,20) 'step'//'1',time(1)
20    FORMAT(A9,' ',G15.5)

C------------------------- Mesh Dimensions
      WRITE(IOUTPUT,30) NUMNP,NUMEL
30    FORMAT(I5,' ',I5)

C------------------------- Mesh Nodes
      DO INODE=1,NUMNP
		WRITE(IOUTPUT,40) INODE,X(INODE),Y(INODE)
40        FORMAT(I5,' ',G15.5,' ',G15.5)
	END DO !INODE

C------------------------- Mesh conectivities

      DO L=1,NUMEL
		NNUD=LNNDEL(L)

	    SELECT CASE (NNUD)
			CASE(2)
			    TYPEL='line'
			CASE(3)
				TYPEL='tri'
			CASE(4)
				TYPEL='quad'
		END SELECT

		WRITE(IOUTPUT,50) L,1,TYPEL,KXX(1:NNUD,L)
50        FORMAT(I5, ' ',I5,' ',A5,' ',(I5,' '))

	END DO !L

C------------------------- Steps Loop.

	DO ISTEP=1,NINT-1
C------------------------- Step header (header for step is already 
c--------------------------written)
		IF (ISTEP.GT.1) THEN
			IWIDTH = INT(LOG10(1D0*ISTEP))+1 !1d0*ISTEP para que sea real el 
	                                         !argumento de LOG10
			
			ENCODE(IWIDTH,'(I)',STEPN) ISTEP 
	        
			WRITE(IOUTPUT,60) 'step'//TRIM(stepn),TIME(ISTEP)
60			FORMAT(A,' ',G15.5)


		END IF
C------------------------- Number of nodal and element variables
		WRITE(IOUTPUT,70) 1,0
70		FORMAT(I5,' ',I5)

C------------------------- Number of components and component size
		WRITE(IOUTPUT,80) 1,1
80		FORMAT(I5,' ',I5)

C------------------------- Kind of variable and units
		WRITE(IOUTPUT,90) VARUNITS
90        FORMAT(A)
C------------------------- State Variable for current step

		READ(IAUX) VCALIT
		

		WRITE(IOUTPUT,100) (I, VCALIT(I),I=1,NUMNP)
100		FORMAT(I5,' ',G15.5)

	END DO !ISTEP

	CLOSE (IOUTPUT)

      END SUBROUTINE WRITE_VISUALMESHPLOT_
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
*    Write result for GID
**************************************************************	
	SUBROUTINE WRITE_RESULTSGID_TOOLS
     . (ISTEP,
     .  IOUTPUT,
     .  NAMERES,
     .  RES, 
     .  NVAL,
     .  NVAR,
     .  WRITETITLE)

      IMPLICIT NONE
	
	! EXTERNAL VARIABLES
	INTEGER            ::
     . ISTEP, 
     . IOUTPUT,
     . NVAL,
     . NVAR
	REAL*8             ::
     . RES(NVAL,NVAR) 
      CHARACTER(LEN=*)   ::
     . NAMERES(NVAR)
      LOGICAL            ::
     . WRITETITLE	
	! LOCAL VARIABLES
	INTEGER            ::
     . IVAR,
     . IVAL,
     . ILAST
      CHARACTER(LEN=40)  ::
     . NAMERESLOC  
C-------------------------------------------------------------	
      IF (WRITETITLE) THEN
	 WRITE (IOUTPUT,50) 
	END IF
C-------------------------------------------------------------
	DO IVAR=1,NVAR
	 NAMERESLOC=NAMERES(IVAR)
	 NAMERESLOC=ADJUSTL(NAMERESLOC)  
	 CALL LASTLETTER_(ILAST,NAMERESLOC)	 
C---------------------------------------WRITE SCALAR VARIABLES
	 WRITE (IOUTPUT,100) NAMERESLOC(1:ILAST-1),REAL(ISTEP)
 	 WRITE (IOUTPUT,200) NAMERESLOC(1:ILAST-1)
	 WRITE (IOUTPUT,300) 'Values'
	 DO IVAL = 1, NVAL
	    WRITE (IOUTPUT,400) IVAL,RES(IVAL,IVAR)
	 END DO
       WRITE (IOUTPUT,500)
	END DO
C-------------------------------------------------------------
	RETURN
   50 FORMAT ('GiD Post Results File 1.0') 
  100 FORMAT (/,'Result',1X,'"',A<ILAST-1>,'"',1X,'"Time Analysis"',
     .        2x,f7.1,2x,'Scalar',2x,'OnNodes')
  200 FORMAT ('ComponentNames',1X,'"',A<ILAST-1>,'"')
  300 FORMAT (A6)
  400 FORMAT (5x,i5,5x,e15.6)
  500 FORMAT ('End Values')
	END SUBROUTINE 
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
*    Write results for TECPLOT . (time,
*
* External variables     
* ------------------   
*    ioutput,
*    head,  
*    root, 
*    nameval,
*    val, 
*    nval,
*    nvar 
*    x  => x coordenates 
*    y  => y coordenates 
*    z  => z coordenates 
*
**************************************************************	
	subroutine write_resultstecplot_tools
     . (time,      
     .  ioutput,  
     .  head,  
     .  root, 
     .  nameval,
     .  val, 
     .  nval,
     .  nvar, 
     .  x,
     .  y,
     .  z, 
     .  nx, 
     .  ny, 
     .  nz)

      implicit none
	! External variables 
	integer, intent(in)      ::
     . ioutput,          ! output unit 
     . nvar,             ! number of variables
     . nval,             ! number of values 
     . nx, 
     . ny, 
     . nz 
	real*8, intent(in)       ::
     . time,             ! time 
     . val(nval,nvar),   ! values 
     . x(nval),          ! x coordenates 
     . y(nval),          ! y coordenates
     . z(nval)           ! z coordenates 
      character(len=*), intent(in)   ::
     . nameval(nvar),    ! Name of the variables 
     . root,             ! root
     . head              ! Head 
	! Local variables 
	integer            ::
     . ivar,
     . ival,
     . ilast
      character(len=100) ::
     . head1
      integer, pointer ::
     . ilastletter(:) => null ()
      integer, parameter ::
     . ktec=1 
C-------------------------------------------------------------
C Write head 
C-------------------------------------------------------------	
      call check_pointer_ (ilastletter,nvar,.true.)
      do ivar=1,nvar
         call lastletter_ (ilast,nameval(ivar))
         ilastletter(ivar)=ilast
      end do
      call lastletter_ (ilast,root)
 	write(ioutput,'(3a)') 'title = "dataset ',root(:ilast),'"'
      write(ioutput,'(100a)') 'variables = "x", "y", "z"',
     &                      (',"',nameval(ivar)(:ilastletter(ivar)),'"',
     &                      ivar=1,nvar) 
      call check_pointer_ (ilastletter,1,.false.)
c-------------------------------------------------------------
c Write number of values 
c------------------------------------------------------------- 
	call lastletter_ (ilast,head)
	head1='zone t = "'//head(:ilast)//'"'
	write(ioutput,'(a,3(a,i5),a)')
     &      head1(:ilast+11)//',',
     &       'i =',nx,', j =',nz,', k =',ny,',  f=point'
C-------------------------------------------------------------
c Write values 
c-------------------------------------------------------------
	do ival=1,nval  
	  write(ioutput,'(3e15.7,<nvar>e15.7)') x(ival),y(ival),z(ival),
     &                 (val(ival,ivar),ivar=1,nvar)
	end do
C-------------------------------------------------------------
	return
	end subroutine 
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
*    Write mesh for GID
**************************************************************
      SUBROUTINE WRITE_MESHGID_TOOLS 
     .  (coord,
     .   kxx,
     .   ltype,
     .   lnodes,
     .   lnval,
     .   mtype,
     .   in_nod,
     .   in_elem,
     .   kxx_aux,
     .   numnp_aux,
     .   ndim,
     .   mnnel, 
     .   numel,
     .   numnp,
     .   numnpsmoo, 
     .   numelsmoo,  
     .   root)


       implicit real*8 (a-h,o-z)

*.................................................common block

       
        character(len=*) root

        integer   kxx(mnnel,numel),          ! connectivities 
     .            ltype(numel),              ! element type
     .            mtype(numel),              ! material type
     .            lnodes(numel),             ! element number of nodes 
     .            in_nod(numnpsmoo),
     .            in_elem(numelsmoo),
     .            kxx_aux(mnnel,numelsmoo)
        

        dimension coord(ndim,numnp)          ! node coordinates

        iowdrac=1

        call lastletter_(il,root)
        

        if(ndim.eq.2) then

          open(77,file=root(1:il-1)//'flavia.dat',status='new')   ! grid for meshplot

          if(ltype(1).eq.1 .or. ltype(1).eq.14) then 
              n_elemtype=3
              nnnnn=3
          end if  

          if(ltype(1).eq.2 .or. ltype(1).eq.5
     .       .or. ltype(1).eq.3 .or. ltype(1).eq.4 
     .       .or. ltype(1).eq.6) then 
              n_elemtype=4  
              nnnnn=4
          end if

          if(ltype(1).eq.12) then 
              nnnnn=6    
              n_elemtype=6    
          end if

        else if(ndim.eq.3) then  

          open(77,file=root(1:il-1)//'flavia.msh',status='new')    ! grid for meshplot
 
          n_elemtype=1
          if(ltype(1).eq.3) n_elemtype=1
          if(ltype(1).eq.1) n_elemtype=3
          nnnnn=8

        end if

         write(77,10)

         if (iowdrac.ne.4) then               
            write(77,20)numel,numnp,n_elemtype
         else if (iowdrac.eq.4) then
            write(77,20)numel,numnp_aux,n_elemtype
         end if

*............................................ coordinates 

        write(77,30)

        do nn=1,numnp
           write(77,40)nn,(coord(idim,nn),idim=1,ndim)
        end do
        if (iowdrac.eq.4) then
           do nn=numnp+1,numnp_aux
              n_aux=in_nod(nn)
              write(77,40)nn,(coord(idim,n_aux),idim=1,ndim)
           end do
        end if

  10    format('Title'/////'N_Elems N_Nodos Elem_Type')

  20    format(3i7)

  30    format('Nodo Coord_X  Cord_Y  Coord_Z')

  40    format(i5,' ',3(1x,g11.5))



*......................................................elements


        write(77,50)

        if (iowdrac.ne.4) then

         if (ndim.eq.2 .and. nnnnn.eq.4) then
            do n=1,numel
               if (kxx(3,n).eq.0) kxx(3,n)=kxx(2,n)
               if (kxx(4,n).eq.0) kxx(4,n)=kxx(3,n)
            end do
         end if

         if (ndim.eq.2 .and. nnnnn.eq.3) then
            do n=1,numel
               if (kxx(3,n).eq.0) kxx(3,n)=kxx(2,n)
            end do
         end if

         if (ndim.eq.2 .and. nnnnn.eq.3) then
            do n=1,numel
               if (kxx(3,n).eq.0) kxx(3,n)=kxx(2,n)
            end do
         end if

         if (ndim.eq.2 .and. nnnnn.eq.6) then
            do n=1,numel
               if (kxx(3,n).eq.0) kxx(3,n)=kxx(2,n)
               if (kxx(4,n).eq.0) kxx(4,n)=kxx(3,n)
               if (kxx(5,n).eq.0) kxx(5,n)=kxx(4,n)
               if (kxx(6,n).eq.0) kxx(6,n)=kxx(5,n)
            end do
           do n=1,numel
              write(77,60)n,kxx(1,n),kxx(4,n),
     .                      kxx(2,n),kxx(5,n),
     .                      kxx(3,n),kxx(6,n),mtype(n)
           end do
           close (77) 
           return
         end if




        else if (iowdrac.eq.4) then

         if (ndim.eq.2 .and. nnnnn.eq.4) then
            do n=1,numel
               if (kxx_aux(3,n).eq.0)kxx_aux(3,n)=kxx_aux(2,n)
               if (kxx_aux(4,n).eq.0)kxx_aux(4,n)=kxx_aux(3,n)
            end do
         end if
         if (ndim.eq.2 .and. nnnnn.eq.3) then
            do n=1,numel
               if (kxx_aux(3,n).eq.0)kxx_aux(3,n)=kxx_aux(2,n)
            end do
         end if
         if (ndim.eq.2 .and. nnnnn.eq.6) then
            do n=1,numel
               if (kxx_aux(3,n).eq.0)kxx_aux(3,n)=kxx_aux(2,n)
               if (kxx_aux(4,n).eq.0)kxx_aux(4,n)=kxx_aux(3,n)
               if (kxx_aux(5,n).eq.0)kxx_aux(5,n)=kxx_aux(4,n)
               if (kxx_aux(6,n).eq.0)kxx_aux(6,n)=kxx_aux(5,n)
            end do
         end if
        end if

        if (iowdrac.ne.4) then

          do n=1,numel
c            write(77,60)n,(kxx(i,n),i=1,lnodes(n)),mtype(n)
            write(77,60)n,(kxx(i,n),i=1,nnnnn),mtype(n)
          end do

        else if (iowdrac.eq.4) then

           do n=1,numel
              if (in_elem(n) .ne. -1) then
                 write(77,60)n,(kxx(i,n),i=1,nnnnn),mtype(n)
              else
                 write(77,60)n,(kxx_aux(i,n),i=1,nnnnn),mtype(n)
              end if
           end do

        end if        
           
 50    format('Elem  Nodo_1.......Nodo_N  Mat_Type')

 60    format (15(1x,i6))

      close (77) 

      return
     
      end subroutine 
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************
*
*     write the jacobian and residual of newton raphson infor-
*     mation and concentations of species
*
**************************************************************
      subroutine write_newton_raphson_info_
     .   (jacobian,
     .    residual,
     .    nrowjac,
     .    ncoljac, 
     .    iter, 
     .    ioutput,
     .    nameservice)
      
      implicit none
	
	! External variables
	integer, intent(in)              ::
     . nrowjac,
     . ncoljac, 
     . ioutput,
     . iter
	real*8, intent(in)               ::
     . jacobian(nrowjac,ncoljac),
     . residual(nrowjac)
      character(len=*), intent(in)     ::
     . nameservice 
      ! Local variables
      integer                          ::
     . i,
     . j 
c------------------------------------------------------------
      write (ioutput,*) '*****************************************'//
     .                   '********'
	if (iter.eq.1) then
         write (ioutput,*) '--------------------------'
	   write (ioutput,*) 'Newton Raphson information'
	   write (ioutput,*) 'Service:',nameservice
	   write (ioutput,*) '--------------------------'
	end if

	write (ioutput,*) '--------------------------------------'//
     .                   '-----------'
	write (ioutput,*) 'Iteration=',iter
      write (ioutput,*) '--------------------------------------'//
     .                   '-----------'
	write (ioutput,*) '     jacobian and residual                       '
	write (ioutput,*) '--------------------------------------'//
     .                   '-----------'
	do i=1,nrowjac
	   write (ioutput,2),(jacobian(i,j),j=1,ncoljac),'**',residual(i)
	end do
      
      write (ioutput,*) '*****************************************'//
     .                   '********'
c-----------------------------------------------------------
    2 format(<ncoljac>e10.3,a2,e10.3)
    3 format(a20,a2,e10.3)
c----------------------------------------------------------	
	
	return
	end subroutine
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
************************************************************** 
      SUBROUTINE MPROVE(A,ALUD,N,NP,INDX,B,X)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NMAX=100)
      DIMENSION A(NP,NP),ALUD(NP,NP),INDX(N),B(N),X(N),R(NMAX)
      DO 12 I=1,N
        SDP=-B(I)
        DO 11 J=1,N
          SDP=SDP+DBLE(A(I,J))*DBLE(X(J))
11      CONTINUE
        R(I)=SDP
12    CONTINUE
      CALL LUBKSB(ALUD,N,NP,INDX,R)
      DO 13 I=1,N
        X(I)=X(I)-R(I)
13    CONTINUE
      RETURN
      END SUBROUTINE 
**************************************************************
*****************Public subroutine****************************
**************************************************************
**************************************************************
**************************************************************     
      subroutine findsingrow (a,np,irow,issing)
        
	implicit double precision (a-h,o-z)
	implicit integer(i-n)
	logical  issing
	parameter (nmax=100,tiny=1.0d-20)

        dimension a(np,np),indx(np),vv(nmax)
        n=np 
        d=1.
        irow=0 
        issing=.false. 
        do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.) then
         issing=.true.
         irow=i 
         return 
        end if 
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        if (j.gt.1) then
          do 14 i=1,j-1
            sum=a(i,j)
            if (i.gt.1)then
              do 13 k=1,i-1
                sum=sum-a(i,k)*a(k,j)
13            continue
              a(i,j)=sum
            endif
14        continue
        endif
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          if (j.gt.1)then
            do 15 k=1,j-1
              sum=sum-a(i,k)*a(k,j)
15          continue
            a(i,j)=sum
          endif
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(j.ne.n)then
          if(a(j,j).eq.0.)a(j,j)=tiny
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      
      
      
      do i=1,n
       if (a(i,i)==0.0d0) then
        issing=.true.
        irow=i 

       end if 
      end do 
      
      
      
      if(a(n,n).eq.0.)a(n,n)=tiny
      
      
      
      
      return
      end subroutine  

!%************************************************************
!%***************Public subroutine****************************
!%************************************************************
!%************************************************************
!%************************************************************
      subroutine findInArray_Char(array,tobefound,pos)
      character(len=*),dimension(:),pointer:: array
      character(len=*):: tobefound
      integer::pos
      
!-------------------------------------------------------------------------
!
!   $Pre-cond:
!
!   $Post-cond:
!
!   $License:
!
!-------------------------------------------------------------------------
      integer::i
      
!-------------------------------------------------------------------------
!
!   $code
!

      pos=0
      do i=1,size(array)
        if (array(i)==tobefound) then
            pos=i
            exit
        endif
      enddo
      
      end subroutine          	
**************************************************************
**************************************************************
**************************************************************
**************************************************************
**************************************************************
**************************************************************
**************************************************************
**************************************************************
**************************************************************
	end module m_general_tools_cheproo