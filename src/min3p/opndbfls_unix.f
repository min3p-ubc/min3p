c ----------------------------------------------------------------------
c subroutine opndbfls
c -------------------
c
c open database files
c
c written by:      Uli Mayer - February 3, 97
c
c last modified:   -
c
c definition of variables:
c
c I --> on input   * arbitrary  - initialized  + entries expected
c O --> on output  * arbitrary  - unaltered    + altered
c 
c                                                                    I O
c passed:   logical:
c           --------
c           search_database    = .true.  -> search database for      * +
c                                           all secondary species
c                                           possible and list in
c                                           file prefix_o.psp
c           character:
c           ----------
c           redox_master       = 'o2(aq)'                            + -
c                                'h2(aq)'
c                                'e-1'
c
c common:   
c gen.f:    integer*4:
c           ----------
c           icdbs              = unit number, database for           * +
c                                             components
c           igdbs              = unit number, database for gases     * +
c           imdbs              = unit number, database for minerals  * +
c           ipsp               = unit number, list of possible       * +
c                                             secondary species
c           irdbs              = unit number, database for redox     * +
c                                             couples 
c           isdbs              = unit number, database for sorbed    * +
c                                             species
c           ixdbs              = unit number, database for           * +
c                                             secondary species in
c                                             water phase
c           l_dbs_dir          = length of string for database       + -
c           l_prfx             = length of prefix of I/O files       + -
c
c           logical:
c           --------
c           full_path          = .true.  -> path for database        + -
c                                           specified in problem
c                                           specific input file
c
c           character:
c           ----------
c           dbs_dir            = database directory                  + -
c           drive              = drive of program installation       + -
c           prefix             = prefix name for all I/O files       + -
c
c external: -
c ----------------------------------------------------------------------
 
      subroutine opndbfls(redox_master,search_database)
 
      use parm
      use gen

      implicit real*8 (a-h,o-z)

      logical search_database
      character*12 redox_master

c  specify unit numbers and open files
c  database files for geochemistry

      icdbs = 30
      ixdbs = 31
      imdbs = 32
      irdbs = 33
      igdbs = 34
      isdbs = 35

      if (full_path) then

        write(*,*) dbs_dir
        open(icdbs,file=
     &       dbs_dir(:l_dbs_dir)//'/comp.dbs',shared,status='old')
        open(ixdbs,file=
     &       dbs_dir(:l_dbs_dir)//'/complex.dbs',shared,status='old')
        open(imdbs,file=
     &       dbs_dir(:l_dbs_dir)//'/mineral.dbs',shared,status='old')
        if (redox_master.eq.'o2(aq)') then
          open(irdbs,file=
     &         dbs_dir(:l_dbs_dir)//'/redox.dbs',shared,status='old')
        elseif (redox_master.eq.'h2(aq)') then
          open(irdbs,file=
     &         dbs_dir(:l_dbs_dir)//'/redoxh2.dbs',shared,status='old')
        elseif (redox_master.eq.'e-1') then
          open(irdbs,file=
     &         dbs_dir(:l_dbs_dir)//'/redoxe.dbs',shared,status='old')
        end if
        open(igdbs,file=
     &       dbs_dir(:l_dbs_dir)//'/gases.dbs',shared,status='old')
        open(isdbs,file=
     &       dbs_dir(:l_dbs_dir)//'/sorption.dbs',shared,status='old')

      else

cintellinux *** delete old options ***

        if (drive.eq.'u') then

          open(icdbs,file=
     &         'u:/min3p-v1.1/new/database/'//dbs_dir(:l_dbs_dir)//
     &         '\comp.dbs',shared,status='old')
          open(ixdbs,file=
     &         'u:/min3p-v1.1/new/database/'//dbs_dir(:l_dbs_dir)//
     &         '\complex.dbs',shared,status='old')
          open(imdbs,file=
     &         'u:/min3p-v1.1/new\database/'//dbs_dir(:l_dbs_dir)//
     &         '/mineral.dbs',shared,status='old')
          if (redox_master.eq.'o2(aq)') then
            open(irdbs,file=
     &           'u:/min3p-v1.1/new/database/'//dbs_dir(:l_dbs_dir)//
     &           '/redox.dbs',shared,status='old')
          elseif (redox_master.eq.'h2(aq)') then
            open(irdbs,file=
     &           'u:/min3p-v1.1/new/database/'//dbs_dir(:l_dbs_dir)//
     &           '/redoxh2.dbs',shared,status='old')
          elseif (redox_master.eq.'e-1') then
            open(irdbs,file=
     &           'u:/min3p-v1.1/new/database/'//dbs_dir(:l_dbs_dir)//
     &           '/redoxe.dbs',shared,status='old')
          end if
          open(igdbs,file=
     &         'u:/min3p-v1.1/new/database/'//dbs_dir(:l_dbs_dir)//
     &         '/gases.dbs',shared,status='old')
          open(isdbs,file=
     &         'u:/min3p-v1.1/new/database/'//dbs_dir(:l_dbs_dir)//
     &         '/sorption.dbs',shared,status='old')

        else

          open(icdbs,file=
     &         drive//':/min3p/database/'//dbs_dir(:l_dbs_dir)//
     &         '/comp.dbs',shared,status='old')
          open(ixdbs,file=
     &         drive//':/min3p/database/'//dbs_dir(:l_dbs_dir)//
     &         '/complex.dbs',shared,status='old')
          open(imdbs,file=
     &         drive//':/min3p/database/'//dbs_dir(:l_dbs_dir)//
     &         '/mineral.dbs',shared,status='old')
          if (redox_master.eq.'o2(aq)') then
            open(irdbs,file=
     &           drive//':/min3p/database/'//dbs_dir(:l_dbs_dir)//
     &           '/redox.dbs',shared,status='old')
          elseif (redox_master.eq.'h2(aq)') then
            open(irdbs,file=
     &           drive//':/min3p/database/'//dbs_dir(:l_dbs_dir)//
     &           '/redoxh2.dbs',shared,status='old')
          end if
          open(igdbs,file=
     &         drive//':/min3p/database/'//dbs_dir(:l_dbs_dir)//
     &         '/gases.dbs',shared,status='old')
          open(isdbs,file=
     &         drive//':/min3p/database/'//dbs_dir(:l_dbs_dir)//
     &         '/sorption.dbs',shared,status='old')

        end if

      end if

      if (search_database) then
        ipsp = 36
        open(ipsp,file=prefix(:l_prfx)//'_o.psp',status='unknown',
     &            form='formatted')
      end if

      return
      end
