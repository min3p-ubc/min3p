!*****Revision Informations Automatically Generated by VisualSVN*****!
!---------------------------------------------------------------------
!> $ID:$
!> $Revision: 869 $
!> $Author: dsu $
!> $Date: 2023-08-18 09:44:21 -0700 (Fri, 18 Aug 2023) $
!> $URL: https://min3psvn.ubc.ca/svn/min3p_thcm/branches/dsu_new_add_2024Jan/src/usg/usg_ice_sheet.F90 $
!---------------------------------------------------------------------
!********************************************************************!

!c ----------------------------------------------------------------------
!c module usg_ice_sheet
!c ----------------------
!c
!c ice sheet model for unstructured mesh
!c
!c written by:      Danyang Su - March. 24, 2020
!c
!c last modified:
!c
!c ----------------------------------------------------------------------
#ifdef USG 
module usg_ice_sheet

  use usg_mesh_surface

  implicit none

  !>
  !> definition of icesheet curve fitting parameters
  !>
  type :: ice_fitting_type
    character*32 :: btypezn               !type of ice sheeting fitting model
    integer :: nparms                     !number of parameters in ice fitting model
    !c 'gradient-fitting model', same as 'gradient' boundary type.
    !c ice thickness = thick_grad + thick_slope_grad*pos_grad(xg, yg or zg).
    integer :: itype_grad                 !  0 or 1 or 2
    character(len=1) :: dir_grad          ! [m] 'x' or 'y' or 'z'
    real*8 :: thick_grad                  ! [m]
    real*8 :: thick_slope_grad            ! [m/m]
    !c 'curve-fitting model'
    !c tz = 100 [m/T], temperature change over elevation, 1 C drop per 100 m elevation gain
    !c ice thickness = max(min(sea_level-sea_level_threshold,0)*ice_thick_sea_level_coeff-
    !c                (z-elevation_threshold)/tz*sea_level_temp_coeff*ice_thick_sea_level_coeff,0)
    real*8 :: sea_level                   ! [m]
    real*8 :: sea_level_temp_coeff        ! [m/T] sea level change per temperature change
    real*8 :: ice_thick_sea_level_coeff   ! [m/m] ice thickness change per sea level change
    real*8 :: sea_level_threshold         ! [m], threshold of sea level change to form ice sheet
    real*8 :: elevation_threshold         ! [m], threshold of elevation to form ice sheet
    !c 'ellipsoid-fitting model'
    !c ellipsoid function: (x-x0)^2/a^2 + (y-y0)^2/b^2 + (z-z0)^2/c^2 = 1
    !c horizontal rotation angle: alpha
    !c ice thickness = 0    if (x-x0)^2/a^2 + (y-y0)^2/b^2 > 1
    !c                 z    if (x-x0)^2/a^2 + (y-y0)^2/b^2 <= 1
    real*8 :: ellip_x0                    ! center of ellipsoid, X0, unit is same as input coordinates
    real*8 :: ellip_y0                    ! center of ellipsoid, Y0, unit is same as input coordinates
    real*8 :: ellip_z0                    ! center of ellipsoid, Z0, unit is same as input coordinates
    real*8 :: ellip_a                     ! principal semiaxes in X axis, unit is same as input coordinates
    real*8 :: ellip_b                     ! principal semiaxes in Y axis, unit is same as input coordinates
    real*8 :: ellip_c                     ! principal semiaxes in Z axis, unit is same as input coordinates
    real*8 :: ellip_alpha                 ! counterclockwise rotation angle (along Z axis) in degree
    !c 'other model', e.g., first type, assign value directly
    real*8 :: ice_thickness    
  end type ice_fitting_type

  !>
  !> file unit of ice sheet 
  !>
  integer :: iunitice_out

  !>
  !> density of ice sheet, [kg/m^3]
  !>
  real*8 :: ice_density

  !>
  !> ice sheet basal temperature type
  !> 0 - not set
  !> 1 - use specified basal temperature
  !> 2 - use 1D steady state heat equation for double layer medium to estimate temperature
  !>
  integer :: ice_basal_temperature_type

  !>
  !> basal temperature of ice sheet during ice growth, [C]
  !>
  real*8 :: ice_basal_temperature_grow

  !>
  !> basal temperature of ice sheet during ice decay, [C]
  !>
  real*8 :: ice_basal_temperature_decay

  !>
  !> 1D thermal conductivity of ice sheet, upper layer
  !> unit: W/mK
  !>
  real*8 :: heatcond_upper_layer

  !>
  !> 1D thermal conductivity of porous media (sedimentary rock), lower layer
  !> unit: W/mK
  !>
  real*8 :: heatcond_lower_layer

  !>
  !> temperature change per meter as elevation increases (e.g., -0.006 oC per meter)
  !>
  real*8 :: delta_temp_per_meter

  !>
  !> scaling factor of ice sheet thickness
  real*8 :: ice_thickness_scalfac
  !>
  !> scaling factor of pressure head and pore stress, including
  real*8 :: ice_scalfac_pw_grow
  !>
  !> scaling factor of pressure head due to ice sheet growth
  !>
  real*8 :: ice_scalfac_pice_grow
  !>
  !> scaling factor of pressure head due to ice sheet decay
  !>
  real*8 :: ice_scalfac_pw_decay
  !>
  !> scaling factor of pore stress due to ice sheet decay
  !>
  real*8 :: ice_scalfac_pice_decay
  !>
  !> scaling factor of boundary pore pressure due to ice sheet grow
  !>
  real*8 :: ice_scalfac_pp_grow
  !>
  !> scaling factor of boundary pore pressure due to ice sheet decay
  !>
  real*8 :: ice_scalfac_pp_decay

  !>
  !> linear interpolation of ice sheet factors
  !> 0: no linear interpolation of ice sheet factors
  !> 1: linear interpolation of ice sheet factor for pw and pice
  !> 2: linear interpolation of ice sheet factor for pw and pice, and boundary pore pressure
  !>
  integer :: ice_scalfac_linearType

  !>
  !> Transformation parameters of ice thickness based on log function.
  !> The gradient based or curve fitting based ice thickness is linear distribution.
  !> These parameters are used to modifiy the ice thickness based on log function
  !> h'=a*(log(h*b+1))^c where h is the original ice thickness, a, b, and c are
  !> the specified parameters
  !>
  logical :: b_ice_transformation  
  real*8 :: ice_transformation_a
  real*8 :: ice_transformation_b
  real*8 :: ice_transformation_c

  !>
  !> Add positive water head as extra thickness when icesheet advances.
  !>
  logical :: b_phw2ice


  !>
  !> flag to specify different reactive transport boundary condition below ice sheet
  !>
  logical, allocatable :: ice_istotbc(:)

  !>
  !> reactive transport boundary condition below ice sheet
  !>
  real*8, allocatable :: ice_totbc(:,:)

  !>
  !> type of reactive transport boundary condition below ice sheet
  !>
  character(len=12), allocatable :: ice_typetotbc(:,:)  

  !>
  !> temperature at the bottom of layered simualtion domain
  !> global variable required for MPI parallel version
  !> only required when '1D steady state heat equation for 
  !> double layer medium to estimate temperature' is used.
  !> Alternatively, use mpi message to send and receive data directly.
  !>
  real*8, allocatable :: layer_nodes_bottom_temp(:)
  real*8, allocatable :: layer_nodes_bottom_temp_gbl(:)

  !>
  !> ice thickness data
  !> size: num_nodes_sn
  !> note: for mpi parallel version, each processor has a copy, 
  !>       but only the subdomains with surface nodes assign the data, 
  !>       the others subdomains keep the initial data
  !> if ice_thickness_new(ivol) >= ice_thickness_old(ivol), ice sheet is growing, 
  !> otherwise, ice sheet is melting
  !>
  real*8, allocatable :: ice_thickness_old(:)
  real*8, allocatable :: ice_thickness_new(:)
  real*8, allocatable :: ice_thickness_new_prev(:)
  real*8, allocatable :: ice_thickness_new_next(:)
  real*8, allocatable :: ice_thickness_phw2ice_old(:)
  real*8, allocatable :: ice_thickness_phw2ice_new(:)

  !>
  !> global ice thickness, used for pore stress calculation
  !> size: num_nodes_per_layer
  !> note: index is in surface node natural ordering
  !>
  real*8, allocatable :: ice_thickness_gbl_old(:)
  real*8, allocatable :: ice_thickness_gbl_new(:)
  real*8, allocatable :: ice_thickness_gbl_tmp(:)
  real*8, allocatable :: ice_thickness_phw2ice_gbl_old(:)
  real*8, allocatable :: ice_thickness_phw2ice_gbl_new(:)

  !>
  !> zone parameters for ice sheet curve fitting
  !>
  type(ice_fitting_type), allocatable :: ice_fitting_parms(:,:)

  contains

  !>
  !> Calculate temperature at specified elevation
  !> based on temperature at zero elevation. 
  !> temperature decreases 1 C for each 100m elevation gain
  !>
  function usg_ice_cal_temperature(temp_z0, z) result(temp_z)

    implicit none

    real*8, intent(in) :: temp_z0, z
    real*8 :: temp_z

    !c local variables
    real*8, parameter :: r100 = 1.0d2

    temp_z = temp_z0 - z/r100

  end function usg_ice_cal_temperature

  !>
  !> Calculate ice thickness based on curve fitting
  !>
  subroutine usg_ice_cal_thickness_curve(ivol,ice_fitting_parm,thickness)

    use usg_mesh_data, only : nodes

    implicit none

    integer, intent(in) :: ivol
    type(ice_fitting_type), intent(in) :: ice_fitting_parm
    real*8, intent(out) :: thickness

    !c local variables
    real*8 :: sea_level                   ! [m]
    real*8 :: sea_level_temp_coeff        ! [m/T] sea level change per temperature change
    real*8 :: ice_thick_sea_level_coeff   ! [m/m] ice thickness change per sea level change
    real*8 :: sea_level_threshold         ! [m], threshold of sea level change to form ice sheet
    real*8 :: elevation_threshold         ! [m], threshold of elevation to form ice sheet
    real*8 :: elevation_ivol              ! [m], elevation of specified control volume
    real*8, parameter :: r0 = 0.0d0, r100 = 100.0d0

    sea_level = ice_fitting_parm%sea_level                 
    sea_level_temp_coeff = ice_fitting_parm%sea_level_temp_coeff      
    ice_thick_sea_level_coeff = ice_fitting_parm%ice_thick_sea_level_coeff 
    sea_level_threshold = ice_fitting_parm%sea_level_threshold       
    elevation_threshold = ice_fitting_parm%elevation_threshold
    elevation_ivol = nodes(ivol)%z

    thickness = min(sea_level-sea_level_threshold,r0)*ice_thick_sea_level_coeff
    thickness = thickness-(elevation_ivol-elevation_threshold)/r100*   &
                sea_level_temp_coeff*ice_thick_sea_level_coeff
    thickness = max(thickness,r0)

  end subroutine usg_ice_cal_thickness_curve

  !>
  !> Calculate ice thickness based on curve fitting
  !>
  subroutine usg_ice_cal_thickness_ellip(ivol,ice_fitting_parm,thickness)

    use usg_mesh_data, only : nodes, b_anchor_coord, anchor_coord_old, &
                              anchor_coord_new

    implicit none

    integer, intent(in) :: ivol
    type(ice_fitting_type), intent(in) :: ice_fitting_parm
    real*8, intent(out) :: thickness

    !c local variables
    real*8 :: x0, y0, z0                  ! center of ellipsoid, unit is same as input coordinates
    real*8 :: a, b, c                     ! principal semiaxes in X, Y and Z axis, unit is same as input coordinates
    real*8 :: alpha                       ! counterclockwise rotation angle (along Z axis) in degree for ellipsoid
    real*8 :: x, y, z                     ! x, y coordinates after rotation
    real*8 :: x1, y1, x2, y2, xa, ya, xb, yb, dx, dy, ratio
    real*8, parameter :: r0 = 0.0d0, r1 = 1.0d0, r180 = 180.0d0,       &
                         pi=3.14159265359d0

    x0 = ice_fitting_parm%ellip_x0
    y0 = ice_fitting_parm%ellip_y0
    z0 = ice_fitting_parm%ellip_z0
    a = ice_fitting_parm%ellip_a
    b = ice_fitting_parm%ellip_b
    c = ice_fitting_parm%ellip_c
    !c convert to radian
    alpha = ice_fitting_parm%ellip_alpha*(pi/r180)

    !c instead of rotation the ellipsoid, we rotate the x, y coordinates in clockwise direction
    if (b_anchor_coord) then
      x1 = anchor_coord_old(1)%x
      y1 = anchor_coord_old(1)%y
      x2 = anchor_coord_old(2)%x
      y2 = anchor_coord_old(2)%y
      xa = anchor_coord_new(1)%x
      ya = anchor_coord_new(1)%y
      xb = anchor_coord_new(2)%x
      yb = anchor_coord_new(2)%y
      ratio = sqrt(((nodes(ivol)%x-x1)**2+(nodes(ivol)%y-y1)**2)/      &
                   ((x2-x1)**2+(y2-y1)**2))
      dx = xa + ratio*(xb-xa) - x0
      dy = ya + ratio*(yb-ya) - y0
    else
      dx = nodes(ivol)%x - x0
      dy = nodes(ivol)%y - y0
    end if
    
    x = dx*cos(-alpha) - dy*sin(-alpha)
    y = dx*sin(-alpha) + dy*cos(-alpha)

    z = (r1 - x**2/a**2 - y**2/b**2)*c**2
    if (z < r0) then
      thickness = r0
    else
      thickness = sqrt(z)
    end if

  end subroutine usg_ice_cal_thickness_ellip

  !>
  !> Calculate ice thickness based on gradient specified parameter
  !>
  subroutine usg_ice_cal_thickness_grad(ivol,ice_fitting_parm,thickness)

    use usg_mesh_data, only : nodes, node_to_layer_node,               &
                              layer_nodes_bottom, layer_nodes_top

    implicit none

    integer, intent(in) :: ivol
    type(ice_fitting_type), intent(in) :: ice_fitting_parm
    real*8, intent(out) :: thickness    

    !c local variables
    integer :: itype_grad
    character*1   :: dir_grad
    real*8 :: pos_grad, temp_grad, thick_grad, temp_slope_grad,        &
              thick_slope_grad

    itype_grad = ice_fitting_parm%itype_grad
    dir_grad = ice_fitting_parm%dir_grad
    thick_grad = ice_fitting_parm%thick_grad
    thick_slope_grad = ice_fitting_parm%thick_slope_grad 
    
    if (itype_grad == 1)then
      select case(dir_grad)
      case ('x')
        pos_grad=layer_nodes_bottom(node_to_layer_node(ivol))%x
      case ('y')
        pos_grad=layer_nodes_bottom(node_to_layer_node(ivol))%y
      case ('z')
        pos_grad=layer_nodes_bottom(node_to_layer_node(ivol))%z
      end select
    else if (itype_grad == 2)then
      select case(dir_grad)
      case ('x')
        pos_grad=layer_nodes_top(node_to_layer_node(ivol))%x
      case ('y')
        pos_grad=layer_nodes_top(node_to_layer_node(ivol))%y
      case ('z')
        pos_grad=layer_nodes_top(node_to_layer_node(ivol))%z
      end select
    else 
      select case(dir_grad)
      case ('x')
        pos_grad=nodes(ivol)%x
      case ('y')
        pos_grad=nodes(ivol)%y
      case ('z')
        pos_grad=nodes(ivol)%z
      end select   
    end if

    thickness = thick_grad + thick_slope_grad*pos_grad    

  end subroutine usg_ice_cal_thickness_grad

  !>
  !> Calculate ice thickness based on other specified model
  !>
  subroutine usg_ice_cal_thickness_other(ivol,ice_fitting_parm,thickness)

    implicit none

    integer, intent(in) :: ivol
    type(ice_fitting_type), intent(in) :: ice_fitting_parm
    real*8, intent(out) :: thickness 

    thickness = ice_fitting_parm%ice_thickness

  end subroutine usg_ice_cal_thickness_other

  !>
  !> Calculate ice thickness for the specified surface node
  !>
  subroutine usg_ice_cal_thickness(i_sn,ice_fitting_parm_opt,ice_thickness_opt)

    implicit none

    integer, intent(in) :: i_sn
    type(ice_fitting_type), intent(in), optional :: ice_fitting_parm_opt
    real*8, optional :: ice_thickness_opt

    !c local variable
    integer :: ivol, ibz
    character*32  :: btypezn
    type(ice_fitting_type) :: ice_fitting_parm
    real*8 :: ice_thickness, phw2ice_thickness

    real*8, parameter :: r0 = 0.0d0

    ibz = node_idx_sn2bz(i_sn)
    ivol = node_idx_sn2lg(i_sn)

    if (present(ice_fitting_parm_opt)) then
      ice_fitting_parm = ice_fitting_parm_opt
    else
      ice_fitting_parm = ice_fitting_parms(ibz,1)
    end if

    btypezn = ice_fitting_parm%btypezn
    if (btypezn.eq.'gradient')then
      call usg_ice_cal_thickness_grad(ivol,ice_fitting_parm,           &
                                      ice_thickness)
    else if (btypezn.eq.'curve-fitting')then
      call usg_ice_cal_thickness_curve(ivol,ice_fitting_parm,          &
                                       ice_thickness)
    else if (btypezn.eq.'ellipsoid-fitting')then
      call usg_ice_cal_thickness_ellip(ivol,ice_fitting_parm,          &
                                       ice_thickness)
    else
      call usg_ice_cal_thickness_other(ivol,ice_fitting_parm,          &
                                       ice_thickness)
    end if

    !c apply icesheet thickness scaling factor
    ice_thickness = ice_thickness*ice_thickness_scalfac

    if (b_ice_transformation) then
      ice_thickness = ice_transformation_a*                            &
                (log10(ice_thickness*ice_transformation_b+1.0d0))**    &
                ice_transformation_c
    end if

    if (present(ice_thickness_opt)) then
      ice_thickness_opt = ice_thickness
    else
      ice_thickness_new(i_sn) = ice_thickness
    end if

  end subroutine usg_ice_cal_thickness

    !>
  !> Calculate ice thickness for the specified surface node
  !>
  subroutine usg_ice_cal_thickness_phw2ice(i_sn)

    use gen, only : btypevs, ivol2bvs, gacc, bcondvs0, time_io

    implicit none

    integer, intent(in) :: i_sn

    !c local variable
    integer :: ivol, ibvs
    real*8 :: phw2ice_thickness

    real*8, parameter :: r0 = 0.0d0, rsmall = 1.0d-10

    ivol = node_idx_sn2lg(i_sn)

    !c Add positive water head as extra thickness when icesheet advances.
    !c Do NOT apply this extra thickness when icesheet retreats as original
    !c flow boundary condition is applied.
    ice_thickness_phw2ice_old(i_sn) = ice_thickness_phw2ice_new(i_sn)

    ice_thickness_phw2ice_new(i_sn) = r0    
    if (ice_thickness_new(i_sn) >= ice_thickness_old(i_sn) .and.     &
        ice_thickness_new(i_sn) >= rsmall) then
      ibvs = ivol2bvs(ivol)
      if (ibvs > 0) then
        if (btypevs(ibvs)=='first') then
          phw2ice_thickness = bcondvs0(ibvs)/gacc/ice_density
          ice_thickness_phw2ice_new(i_sn) = max(r0,phw2ice_thickness)
        end if
      end if
    end if

  end subroutine usg_ice_cal_thickness_phw2ice

  !>
  !> Get components from ice sheet model.
  !>
  subroutine usg_ice_get_components(ibz, ncomp, totbc, typetotbc, iserror) 

    implicit none

    integer, intent(in) :: ibz, ncomp
    real*8, intent(out) :: totbc(ncomp)
    character(len=*), intent(out) :: typetotbc(ncomp)
    logical, intent(out) :: iserror

    if (ncomp /= size(ice_totbc,1)) then
      iserror = .true.
    else
      iserror = .false.
    end if

    totbc(1:ncomp) = ice_totbc(1:ncomp,ibz)
    typetotbc(1:ncomp) = ice_typetotbc(1:ncomp,ibz)

  end subroutine usg_ice_get_components

  !>
  !> Set components to ice sheet model.
  !>
  subroutine usg_ice_set_components(ibz, ncomp, totbc, typetotbc, iserror) 

    implicit none

    integer, intent(in) :: ibz, ncomp
    real*8, intent(in) :: totbc(ncomp)
    character(len=*), intent(in) :: typetotbc(ncomp)
    logical, intent(out) :: iserror

    if (ncomp /= size(ice_totbc,1)) then
      iserror = .true.
    else
      iserror = .false.
    end if

    ice_totbc(1:ncomp,ibz) = totbc(1:ncomp)
    ice_typetotbc(1:ncomp,ibz) = typetotbc(1:ncomp)

  end subroutine usg_ice_set_components

  !>
  !>  Get ice sheet boundary condition
  !>
  subroutine usg_ice_get_new_bc(nvolbc,ivolbc,b_ivolbc_ice,totbc)

    use gen, only : uvsnew, tkel

    implicit none

    integer, intent(in) :: nvolbc

    integer, intent(in) :: ivolbc(nvolbc)

    logical, intent(inout) :: b_ivolbc_ice(nvolbc)      !indicator for those volumes affected

    real*8, allocatable, optional :: totbc(:,:)

    real*8 :: pressure_melt_k
    external :: pressure_melt_k

    !c local variables
    integer :: i, i_sn, ibz_ice, ivol, ncomp
    logical :: hastotbc
    real*8, parameter :: r0 = 0.0d0, rsmall = 1.0d-10


    b_ivolbc_ice = .true.

    if (nvolbc == 0 .or. num_nodes_sn == 0) then
      return
    end if

    b_ivolbc_ice = .false.

    hastotbc = present(totbc)

    ncomp = size(ice_totbc,1)

    do i = 1, nvolbc
      !c boundary condition can be duplicated, use negative value for
      !c those overwritten boundary condition, by DSU, 2018-02-02 
      ivol = abs(ivolbc(i))
      i_sn = node_idx_lg2sn(ivol)
      if (i_sn > 0) then                 !top surface node

        if (ice_basal_temperature_type <= 1) then
          if ((ice_thickness_new(i_sn) < ice_thickness_old(i_sn) .and. &
               ice_thickness_old(i_sn) >= rsmall) .or.                 &
              (ice_thickness_new(i_sn) == ice_thickness_old(i_sn) .and.&
               ice_thickness_old(i_sn) <= rsmall)) then     !ice decay
            b_ivolbc_ice(i) = .true.
          end if
        else if (ice_basal_temperature_type == 2) then
          if (ice_thickness_old(i_sn) >= rsmall .and.                  &
              tkel(ivol) > pressure_melt_k(ivol,r0)) then
            b_ivolbc_ice(i) = .true.
          end if
        end if

        if (b_ivolbc_ice(i)) then
          if (hastotbc) then
            ibz_ice = node_idx_sn2bz(i_sn)
            if (ice_istotbc(ibz_ice) .and. ice_thickness_new(i_sn) > r0) then
              totbc(1:ncomp,i) = ice_totbc(1:ncomp,ibz_ice)
            end if
          end if
        end if
      else                               !internal node
        b_ivolbc_ice(i) = .true.
      end if
    end do

  end subroutine usg_ice_get_new_bc

  !>
  !> modify for permafrost temperature 
  !> to be further mofified using freezing/thawing model
  !>
  subroutine usg_ice_modify_permafrost_temp(ivol,tempbc)

    use usg_mesh_data, only : nodes, node_to_layer_node,               &
                              layer_nodes_bottom

    implicit none

    integer, intent(in) :: ivol

    real*8, intent(inout) :: tempbc            !in: ground surface temperature under ice free condition
                                               !out: estimated ground surface temperature under ice free or ice covered condition

    !c local variables
    integer :: i_sn, ivol_bottom
    real*8 :: temp_low, temp_up, z_cal, z_low, z_up
    real*8, parameter :: r0 = 0.0d0, rsmall = 1.0d-10

    if (ice_basal_temperature_type == 0) then
      return
    else if (ice_basal_temperature_type == 1) then
      !c use specified basal temperature
      i_sn = node_idx_lg2sn(ivol)
      if (i_sn > 0) then                 !top surface node
        if (ice_thickness_new(i_sn) >= rsmall) then
          if (ice_thickness_new(i_sn) >= ice_thickness_old(i_sn)) then     !ice grows (advances)
            tempbc = ice_basal_temperature_grow
          else if (ice_thickness_new(i_sn) < ice_thickness_old(i_sn)) then     !ice decays (retreat)
            tempbc = ice_basal_temperature_decay
          end if
        end if
      end if
    else if (ice_basal_temperature_type == 2) then
      !c use 1D steady state heat equation for double layer medium to estimate basal temperature
      !c ref https://primer-computational-mathematics.github.io/book/d_geosciences/1D_Heat_Conduction.html
      i_sn = node_idx_lg2sn(ivol)
      if (i_sn > 0) then                 !top surface node
        ivol_bottom = node_to_layer_node(ivol)

        z_low = 0.0d0
        z_cal = nodes(ivol)%z - layer_nodes_bottom(ivol_bottom)%z
        z_up = z_cal + ice_thickness_new(i_sn)

        temp_low = layer_nodes_bottom_temp(ivol_bottom)
        temp_up = tempbc + (z_up-z_cal)*delta_temp_per_meter

        tempbc = temp_low + (temp_up-temp_low)*heatcond_lower_layer*z_cal/     &
                 (z_cal*heatcond_lower_layer+(z_up-z_cal)*heatcond_upper_layer)
      end if

    end if

  end subroutine usg_ice_modify_permafrost_temp

  !>
  !> compute pressure head change due to ice sheet
  !>
  subroutine usg_ice_compute_pw(ivol,rho_w,bcond_start,bcond_end,      &
                                delta_pw,delta_pp,gacc)

    use gen, only : uvsnew, tkel

    implicit none

    integer, intent(in) :: ivol
    real*8, intent(in) :: rho_w
    real*8, intent(in) :: bcond_start
    real*8, intent(in) :: bcond_end
    real*8, intent(inout) :: delta_pw
    real*8, intent(inout) :: delta_pp
    real*8, intent(in),optional :: gacc

    real*8 :: pressure_melt_k
    external :: pressure_melt_k

    !c local variables
    integer :: i_sn
    real*8, parameter :: r0 = 0.0d0, r1 = 1.0d0, rsmall = 1.0d-10

    delta_pw = r0
    delta_pp = bcond_end
    i_sn = node_idx_lg2sn(ivol)

    if (i_sn > 0) then                 !top surface node
      if (ice_thickness_new(i_sn) >= rsmall) then
        delta_pw = ice_thickness_new(i_sn)*ice_density/rho_w
        
        if (ice_basal_temperature_type <= 1) then
          if (ice_thickness_new(i_sn) >= ice_thickness_old(i_sn)) then     !ice grows (advances)
            delta_pw = delta_pw*ice_scalfac_pw_grow
            delta_pp = bcond_start*ice_scalfac_pp_grow +               &
                       bcond_end*(r1-ice_scalfac_pp_grow)

          else if (ice_thickness_new(i_sn) < ice_thickness_old(i_sn)) then     !ice decays (retreat)
            delta_pw = delta_pw*ice_scalfac_pw_decay
            delta_pp = bcond_start*ice_scalfac_pp_decay +              &
                       bcond_end*(r1-ice_scalfac_pp_decay)

          end if
        else if (ice_basal_temperature_type == 2) then
          if (tkel(ivol) <= pressure_melt_k(ivol,r0)) then
            delta_pw = delta_pw*ice_scalfac_pw_grow
            delta_pp = bcond_start*ice_scalfac_pp_grow +               &
                       bcond_end*(r1-ice_scalfac_pp_grow)
          else
            delta_pw = delta_pw*ice_scalfac_pw_decay
            delta_pp = bcond_start*ice_scalfac_pp_decay +              &
                       bcond_end*(r1-ice_scalfac_pp_decay)
          end if
        end if

        if (present(gacc)) then
          delta_pw = delta_pw*rho_w*gacc
        end if
      end if
    end if

  end subroutine usg_ice_compute_pw

  !>
  !> compute porosity change due to pore pressure change caused by 
  !> vertical loading (e.g., ice sheet)
  !>
  subroutine usg_ice_compute_dpicedt(ivol,storcoeff,skeptoncoeff,      &
                                     loadingfactor,dpicedt,gacc)
    use usg_mesh_data, only : node_to_layer_node
    use gen, only : uvsnew, tkel, node_idx_lg2g, idbg, rank

    implicit none 

    integer, intent(in) :: ivol
    real*8, intent(in) :: storcoeff, skeptoncoeff, loadingfactor
    real*8, intent(inout) :: dpicedt
    real*8, intent(in),optional :: gacc

    real*8 :: pressure_melt_k
    external :: pressure_melt_k

    !c local variables
    integer :: ivol_ln, ivol_sn
    real*8, parameter :: r0 = 0.0d0

    !c note, pore pressure is applied to all internal nodes below ice sheet.
    !c use global ice thickness instead of ice thickness owned by lcoal surface
    ivol_ln = node_to_layer_node(ivol)
    !c do NOT use ivol_sn = node_idx_lg2sn(ivol) here since this is only for boundary nodes
    ivol_sn = node_idx_ln2sn(ivol_ln)

    dpicedt = r0

    !c Important Note: do not skip calculation when ivol_sn is zero. ivol_sn 0 means
    !c the subdomain does not contain any surface node, but the mechanical loading should
    !c still be calculated. Without domain decomposition, this ivol_sn is always positive.
    !if (ivol_sn <= 0) then
    !  return      
    !end if 

#ifdef PETSC    
    dpicedt = -(ice_thickness_gbl_new(ivol_ln)-                        &
               ice_thickness_gbl_old(ivol_ln)+                         &
               ice_thickness_phw2ice_gbl_new(ivol_ln)-                 &
               ice_thickness_phw2ice_gbl_old(ivol_ln))*                &
               storcoeff*skeptoncoeff*loadingfactor
#else
    dpicedt = -(ice_thickness_new(ivol_sn)-                            &
               ice_thickness_old(ivol_sn)+                             &
               ice_thickness_phw2ice_new(ivol_sn)-                     &
               ice_thickness_phw2ice_old(ivol_sn))*                    &
               storcoeff*skeptoncoeff*loadingfactor
#endif

    if (ice_basal_temperature_type <= 1) then
      if (dpicedt < r0) then                             !ice grows (advances)
        dpicedt = dpicedt*ice_scalfac_pice_grow
      else if (dpicedt > r0) then                        !ice decays (retreat)
        dpicedt = dpicedt*ice_scalfac_pice_decay
      end if
    else if (ice_basal_temperature_type == 2) then
      if (tkel(ivol) <= pressure_melt_k(ivol,r0)) then      !ice grows (advances)
        dpicedt = dpicedt*ice_scalfac_pice_grow
      else                                               !ice decays (retreat)
        dpicedt = dpicedt*ice_scalfac_pice_decay
      end if
    end if

    if (present(gacc)) then
      dpicedt = dpicedt*ice_density*gacc
    end if

  end subroutine usg_ice_compute_dpicedt


  !>
  !> Output ice sheet basal mesh (surface mesh) using ascii format.
  !> Please note for MPI parallel output, ghost cells are not included.
  !>
  subroutine usg_ice_output_mesh(ifile,strtitle)

    use gen, only : ilog, memory_monitor, ascii_fmt

    use usg_mesh_data, only : b_mesh_output_scale, mesh_output_scale,  &
                              cell_type, cell_type_tri, cell_type_quad,&
                              cell_type_prism, cell_type_hexa, cells,  &
                              num_nodes_per_cell, node_to_layer_node,  &
                              layer_nodes_top

    implicit none

    integer, intent(in) :: ifile
    character(len=*), intent(in) :: strtitle

    !c local variables
    integer :: i, j, k, icell, i_sn, ierr, num_cells_sn_loc2
#ifdef PETSC
    logical, allocatable :: iflags(:)
#endif
    !c external functions
    external :: checkerr

    !c write version information
    write(ifile,'(a)') "# vtk DataFile Version 2.0"

    !c write title
    write(ifile,'(a)') trim(adjustl(strtitle))

    !c indicate ascii or binary data
    write(ifile,'(a)') "ASCII"

    !c write dataset head
    write(ifile,'(a)') "DATASET UNSTRUCTURED_GRID"

    !c write nodes
    write(ifile,'(a,1x,i12,1x,a)') "POINTS",num_nodes_sn_loc,"double"
    if (b_mesh_output_scale) then
      do i_sn = 1, num_nodes_sn_loc
        i = node_to_layer_node(node_idx_sn2lg(i_sn))
        write(ifile,ascii_fmt)                                         &
              layer_nodes_top(i)%x*mesh_output_scale%x,                &
              layer_nodes_top(i)%y*mesh_output_scale%y,                &
              layer_nodes_top(i)%z*mesh_output_scale%z
      end do
    else
      do i_sn = 1, num_nodes_sn_loc
        i = node_to_layer_node(node_idx_sn2lg(i_sn))
        write(ifile,ascii_fmt) layer_nodes_top(i)%x,                   &
              layer_nodes_top(i)%y, layer_nodes_top(i)%z
      end do
    end if

    !number of cells without ghost nodes
#ifdef PETSC
    allocate(iflags(num_cells_sn_loc), stat = ierr)
    iflags = .false.
    call checkerr(ierr,'usg_mesh_surface-iflags',ilog)
    call memory_monitor(sizeof(iflags),'usg_mesh_surface-iflags',.false.)

    num_cells_sn_loc2 = 0

    do i_sn = 1, num_cells_sn_loc
      icell = cell_idx_sn2lg(i_sn)
      k = 0
      do i = 1, num_nodes_per_cell
        j = node_idx_lg2sn(cells(i,icell))
        if (j > 0 .and. j <= num_nodes_sn_loc) then
          k = k + 1
        end if
      end do
      if (k == num_nodes_sn_per_cell) then
        iflags(i_sn) = .true.
        num_cells_sn_loc2 = num_cells_sn_loc2 + 1
      end if
    end do    
#else
    num_cells_sn_loc2 = num_cells_sn_loc
#endif
    !c write cells
    write(ifile,'(a,2(1x,i12))') "CELLS",num_cells_sn_loc2,            &
          num_cells_sn_loc2*(num_nodes_sn_per_cell+1)

    do i_sn = 1, num_cells_sn_loc
#ifdef PETSC
      if (.not.iflags(i_sn)) then
        cycle
      end if
#endif
      icell = cell_idx_sn2lg(i_sn)
      write(ifile,'(i3)',advance='no') num_nodes_sn_per_cell
      do i = 1, num_nodes_per_cell
        j = node_idx_lg2sn(cells(i,icell))
        if (j > 0) then
          write(ifile,'(3(1x,i12))',advance='no') j-1
        end if
      end do
      write(ifile,'(/,a)') ' '
      backspace(ifile)
    end do

    !c free memory space
#ifdef PETSC
    call memory_monitor(-sizeof(iflags),'usg_mesh_surface-iflags',.false.)
    deallocate(iflags, stat = ierr)    
#endif

    !c write cell types
    write(ifile,'(a,1x,i12)') "CELL_TYPES",num_cells_sn_loc2
    do i_sn = 1, num_cells_sn_loc2
      write(ifile,'(i2)') cell_type_sn
    end do

    !c write ice sheet temperature and thickness
    write(ifile,'(a,1x,i12)') "POINT_DATA",num_nodes_sn_loc

    !c ice sheet thickness
    write(ifile,'(a)') "SCALARS thickness double"
    write(ifile,'(a)') "LOOKUP_TABLE default"
    do i_sn = 1, num_nodes_sn_loc
      write(ifile,ascii_fmt) ice_thickness_new(i_sn)
    end do

    !c ice sheet thickness due to positive water head
    write(ifile,'(a)') "SCALARS thickness_phw2ice double"
    write(ifile,'(a)') "LOOKUP_TABLE default"
    do i_sn = 1, num_nodes_sn_loc
      write(ifile,ascii_fmt) ice_thickness_phw2ice_new(i_sn)
    end do 
    
  end subroutine usg_ice_output_mesh


end module usg_ice_sheet
#endif
