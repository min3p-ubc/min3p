!c -----------------------------------------------------------------------
!c real*8 function bunsen
!c ---------------------
!c
!c temperature correction for gases using bunsen solubility coefficients
!c
!c written by:      Rich Amos - April 22, 2004
!c
!c last modified:   
!c
!c definition of variables:
!c
!c I --> on input   * arbitrary  - initialized  + entries expected
!c O --> on output  * arbitrary  - unaltered    + altered
!c                                                                    I O
!c
!c passed:   real*8:
!c           -------
!c           a,b,c              = bunsen solubility cooeficients
!c           tempk              = temperature [K]                     + -
!c
!c
!c local:    real*8:
!c           -------
!c           r1                 = constant
!c           enat               = e
!c           r100               = constant
!c           rh2o               = number of moles in 1L H2O
!c           rgasatm            = ideal gas constant (L atm/mol K )
!c           tkels              = standard temperature (K)
!c
!c
!c external: -
!c ----------------------------------------------------------------------
  
      real*8 function bunsen(a,b,c,tempk)
 
      implicit none
      real*8 :: a, b, c, tempk
      real*8 :: r1, r100, rh2o, rgasatm, enat, tkels
 
      parameter (r1 = 1.0d0)
      parameter (enat = 2.71828182845904509d0)
      parameter (r100 = 1.0d2)
      parameter (rh2o = 5.551d1)
      parameter (rgasatm = 0.082054d0)
      parameter (tkels = 2.7315d2)
      
      bunsen = (enat**(a + b*(r100/tempk)+c*dlog(tempk/r100)))/        &
               (rgasatm*tkels)
 
      return
      end
