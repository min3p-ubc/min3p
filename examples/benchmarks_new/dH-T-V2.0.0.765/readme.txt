The four batch examples are to test the temperature influence on 
ionic complexation  			- aqueous_dh, 
gas exchange 				- gas, 
mineral dissolution/precipitation 	- caldh, and 
redox reactions 			- redox.

Each example is verified against PHREEQC (in a sub-folder phreeqc_n).

Three exapmles are included under each folder, which represent to the cases for three temperature levels, i.e., 5, 25 and 60 degC. 

Comparison of the results are documented in an excel document under each folder.

ToDo:

The pe at tempearature other than 25 degC calcualted by both MIN3P and PHREEQC showed significant difference. 

The code for calculating pe is in file phpe.f90:

!c  compute pH

      if (ph_output) then
        !ph = 0.0d0
        do ic=1,nc-1
          if (namec(ic).eq.'h+1') then
            ph = -dlog10(gammac(ic)*c(ic))
            exit
          end if
        end do
      end if

!c  compute pe and Eh

      if (pe_output) then
        !pe = 0.0d0
        do ic=1,nc-1
          if (namec(ic).eq.'e-1') then           !from e-1
            pe = -dlog10(c(ic))
            eh = ehfac * tempkel * pe
            exit
          end if
        end do
        if (ph_output) then
          do ic=1,nc-1
            if (redox_master.eq.'o2(aq)'.and.                         &
                namec(ic).eq.'o2(aq)') then                            
              pe = 21.5045d0                                          &
                 - dlog10(gammac(nc)**0.5d0)                          &
                 + dlog10((gammac(ic)*c(ic))**0.25d0)                 &
                 - ph                                                  
              eh = ehfac * tempkel * pe                                
            elseif (redox_master.eq.'h2(aq)'.and.                     &
                namec(ic).eq.'h2(aq)') then                            
              pe = -1.562d0                                           &
                 - dlog10((gammac(ic)*c(ic))**0.50d0)                 &
                 - ph
              eh = ehfac * tempkel * pe
            end if
             
          end do
        end if
      end if

      return
      end

It seems that 21.5045d0 is hardwired according to the PhD thesis Table 4.12 by Mayer (1999), which should be temperature dependent (?). This needs to be fixed in the future.

Nevertheless, it is a secondary parameter for output. It has no effect on the pe and Eh calculations.


	  