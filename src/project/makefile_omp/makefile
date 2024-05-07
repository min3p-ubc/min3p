#
# Makefile for MIN3P-HPC OpenMP parallel version
# written by:      Danyang Su
#
# last modified:   Danyang Su - Nov. 13, 2019
#

# Conditional parameters, please leave the variable empty (DO NOT PUT FALSE instead) if the given condition is FALSE.
# e.g., 'with-debug = true' or 'with-debug = false' or 'with-debug = xxx' means debug is turned on.
#       'with-debug = ' means debug is turned off. The provided value does not mean anything here.
with-debug =

with-openmp = true

with-gcc = true
with-intel =
with-intel-after2018 =
with-pgi = 

with-lis =
with-metis =
with-cgal = 
with-gpl = 
with-archisimple = 
with-usg = true
with-mem-monitor = true

with-static-link = true

####################################################
######     DO NOT CHANGE ANYTHING BELOW       ######
###### IF YOU ARE NOT SURE WHAT YOU ARE DOING ######
####################################################

################## Debug options ###################
ifdef with-debug

ifdef with-intel
FFLAGS := -g -check all -check bounds -traceback
CXXFLAGS := -g -check all  -check bounds -traceback
else
FFLAGS := -g -fcheck=all -fbacktrace -Wall
CXXFLAGS := -g -fcheck=all -fbacktrace -Wall
endif

FPPFLAGS := -DLINUX -DDEBUG -DDEBUG_X64 -DSTANDARD_FORTRAN
CPPFLAGS := -DLINUX -DDEBUG -DDEBUG_X64

EXENAME := min3p-hpc-omp-d
else
FFLAGS := -O2
CXXFLAGS := -O2

FPPFLAGS := -DLINUX -DRELEASE -DRELEASE_X64 -DSTANDARD_FORTRAN
CPPFLAGS := -DLINUX -DRELEASE -DRELEASE_X64

EXENAME := min3p-hpc-omp
endif

################## OpenMP options ##################
ifdef with-openmp
FPPFLAGS := $(FPPFLAGS) -DOPENMP -DSCHEDULE_STATIC
endif

################## Compiler options ################
# Additional flags that may be required by the compiler ...
# For intel compiler, please add -qoverride-limits to the compiling flags.
# For some complex code likes jacrt.F90, intel compiler may run out of memory during parallelization
# of this region when USG code is included that it aborts during the parallelization, creating bad code without reporting error.
# The bug has been reported to Intel with Request#:03681401. Probably this will be fixed after 2019 version.
# Remove -I$(LIS_INC) in FFLAGS and CXXFLAGS when LIS is not used, otherwise, it will cause link error when CGAL is used.
ifdef with-gcc
FC = gfortran -lstdc++
CXX = g++ -lstdc++
FPPFLAGS := $(FPPFLAGS) -DGCC
OPTFLAGS := -frounding-math

ifdef with-openmp
FFLAGS := $(FFLAGS) -fopenmp
endif

else
OPTFLAGS :=
endif

ifdef with-pgi
FC = pgfortran -lstdc++
CXX = pgc++ -lstdc++
FPPFLAGS := $(FPPFLAGS) -DPGI

ifdef with-openmp
FFLAGS := $(FFLAGS) -mp
endif

endif

ifdef with-intel
FC = ifort -lstdc++
CXX = icpc -lstdc++
FPPFLAGS := $(FPPFLAGS) -DINTEL

FFLAGS := $(FFLAGS) -qoverride-limits
CXXFLAGS := $(CXXFLAGS) -qoverride-limits

ifdef with-openmp
ifdef with-intel-after2018
FFLAGS := $(FFLAGS) -qopenmp
else
FFLAGS := $(FFLAGS) -openmp
endif
endif

endif

################## LIS options #####################
# To enable LIS solver, set LIS installation path (LIS_DIR) and architecture (LIS_ARCH) 
# before compiling and include LIS_INC and LIS_LIB below, and use -DLIS in FPPFLAGS
ifdef with-lis
LIS_INC := ${LIS_DIR}/${LIS_ARCH}/include
LIS_LIB := -L${LIS_DIR}/${LIS_ARCH}/lib -llis

FFLAGS := $(FFLAGS) -I$(LIS_INC)
CXXFLAGS := $(CXXFLAGS) -I$(LIS_INC)
FPPFLAGS := $(FPPFLAGS) -DLIS
else
LIS_INC :=
LIS_LIB :=
endif

################## metis options ###################
ifdef with-metis
FPPFLAGS := $(FPPFLAGS) -DMETIS
DLIB := -lstdc++ -lm -lmetis
else
DLIB := -lstdc++ -lm
endif

################## CGAL options ####################
# To enable CGAL library, add the following DLIB with CGAL related libraries and use -DCGAL in FPPFLAGS and -DUSECGAL in CPPFLAGS
ifdef with-cgal
DLIB := $(DLIB) -L/usr/local/lib -rdynamic /usr/local/lib/libmpfr.so /usr/local/lib/libgmp.so /usr/local/lib/libCGAL_ImageIO.so.11.0.1 /usr/local/lib/libCGAL.so.11.0.1 /usr/local/lib/libboost_thread.so /usr/local/lib/libboost_system.so -lpthread -lGLU -lGL -lX11 -lXext -lz /usr/local/lib/libCGAL_ImageIO.so.11.0.1 /usr/local/lib/libCGAL.so.11.0.1 /usr/local/lib/libboost_thread.so /usr/local/lib/libboost_system.so -lpthread -lGLU -lGL -lX11 -lXext -lz /usr/local/lib/libmpfr.so /usr/local/lib/libgmp.so /usr/local/lib/libboost_thread.so /usr/local/lib/libboost_system.so -lpthread -Wl,-rpath,/usr/local/lib

FPPFLAGS := $(FPPFLAGS) -DCGAL
CPPFLAGS := $(CPPFLAGS) -DUSECGAL
endif

################## GPL options #####################
ifdef with-gpl
FPPFLAGS := $(FPPFLAGS) -DGPL
endif

############## ARCHISIMPLE options #################
ifdef with-archisimple
FPPFLAGS := $(FPPFLAGS) -DARCHISIMPLE
CPPFLAGS := $(CPPFLAGS) -DARCHISIMPLE
endif


################## USG options #####################
ifdef with-usg
FPPFLAGS := $(FPPFLAGS) -DUSG
endif


################## Static libs #####################
# by default, intel compiler use static link on    #
# windows, PGI compiler does not support static    #
# linking of user binary, GCC compiler may still   #
# link some system dynamic libs                    #
####################################################
ifdef with-static-link
ifdef with-gcc
SLIB := $(SLIB) -static-libgfortran -static-libgcc
endif
ifdef with-intel
SLIB := $(SLIB) -static-intel
endif
endif

################## other options ###################
ifdef with-mem-monitor
FPPFLAGS := $(FPPFLAGS) -DMEMORY_MONITOR
endif

#Source code folder
SRC =./../../

SOURCES = $(SRC)min3p/parm.o\
	$(SRC)usg/geometry_definition.o\
	$(SRC)usg/math_common.o\
	$(SRC)usg/geometry.o\
	$(SRC)solver/lisf_init.o\
	$(SRC)gas_advection/wgpropd.o\
	$(SRC)min3p/int3b_ovendry.o\
	$(SRC)min3p/h_ovendry.o\
	$(SRC)min3p/root_modu.o\
	$(SRC)gas_advection/giups_brt.o\
	$(SRC)min3p/concsort.o\
	$(SRC)min3p/stordd.o\
	$(SRC)min3p/soilprdd.o\
	$(SRC)min3p/relpfsat.o\
	$(SRC)min3p/xh_ovendry.o\
	$(SRC)min3p/satindex.o\
	$(SRC)gas_advection/gasm.o\
	$(SRC)dgm/dgefam.o\
	$(SRC)min3p/cos_function.o\
	$(SRC)min3p/boundary_func.o\
	$(SRC)min3p/zero_r8.o\
	$(SRC)dgm/msub.o\
	$(SRC)min3p/zero_i4.o\
	$(SRC)min3p/relperm_ovendry.o\
	$(SRC)min3p/fluxv.o\
	$(SRC)dgm/dgeslm.o\
	$(SRC)min3p/soilparm.o\
	$(SRC)min3p/porosity_flow.o\
	$(SRC)min3p/g_ovendry.o\
	$(SRC)min3p/diffcoff.o\
	$(SRC)dgm/diffcoff_g.o\
	$(SRC)min3p/fluxd.o\
	$(SRC)min3p/fluxfs.o\
	$(SRC)min3p/dtotcong.o\
	$(SRC)min3p/satfpres_app.o\
	$(SRC)min3p/rhsrt.o\
	$(SRC)min3p/icnvrt.o\
	$(SRC)min3p/findstrg.o\
	$(SRC)min3p/zoutput.o\
	$(SRC)min3p/vhoff.o\
	$(SRC)min3p/satfpres.o\
	$(SRC)dgm/mmul.o\
	$(SRC)min3p/gs_ovendry.o\
	$(SRC)min3p/rhonew.o\
	$(SRC)min3p/alkcalc.o\
	$(SRC)min3p/totsorb.o\
	$(SRC)min3p/storheatevap.o\
	$(SRC)min3p/rsort.o\
	$(SRC)gas_advection/dfluxvg.o\
	$(SRC)min3p/storheat.o\
	$(SRC)min3p/ionstr.o\
	$(SRC)min3p/diff_vapour.o\
	$(SRC)min3p/acoff.o\
	$(SRC)min3p/w_ovendry.o\
	$(SRC)min3p/int_h_ovendry.o\
	$(SRC)min3p/fluxvs.o\
	$(SRC)min3p/fluxvs_free_drainage.o\
	$(SRC)min3p/fluxheat.o\
	$(SRC)min3p/fluxdd.o\
	$(SRC)min3p/crc.o\
	$(SRC)min3p/bunsen.o\
	$(SRC)min3p/simq.o\
	$(SRC)min3p/findkey.o\
	$(SRC)min3p/storddfs.o\
	$(SRC)gas_advection/relpfsat_g.o\
	$(SRC)dgm/madd.o\
	$(SRC)phase_class/tools/lubksb.o\
	$(SRC)min3p/findzone.o\
	$(SRC)min3p/dhconst.o\
	$(SRC)gas_advection/wgprop.o\
	$(SRC)min3p/secspec.o\
	$(SRC)min3p/rhsvs.o\
	$(SRC)min3p/int3a_ovendry.o\
	$(SRC)min3p/bulkconc.o\
	$(SRC)phase_class/constants/m_constants_cheproo.o\
	$(SRC)solver/solver_linear.o\
	$(SRC)version/Version.o\
	$(SRC)solver/solver_lis_common.o\
	$(SRC)phase_class/tools/xmlreader/m_charset.o\
	$(SRC)phase_class/tools/xmlreader/m_debug.o\
	$(SRC)min3p/dual.o\
	$(SRC)min3p/phys.o\
	$(SRC)solver/solver_snes_common.o\
	$(SRC)version/writeversion.o\
	$(SRC)dgm/dgml.o\
	$(SRC)min3p/bbls.o\
	$(SRC)solver/petsc_mpi_common.o\
	$(SRC)phase_class/tools/xmlreader/m_io.o\
	$(SRC)min3p/dens.o\
	$(SRC)icesheet/m_ice_sheet.o\
	$(SRC)solver/solver_snes_function.o\
	$(SRC)phase_class/tools/xmlreader/m_converters.o\
	$(SRC)phase_class/tools/xmlreader/m_buffer.o\
	$(SRC)phase_class/tools/xmlreader/m_dictionary.o\
	$(SRC)min3p/multidiff.o\
	$(SRC)phase_class/tools/xmlreader/m_reader.o\
	$(SRC)energy_balance/m_heat_transport.o\
	$(SRC)phase_class/tools/xmlreader/m_elstack.o\
	$(SRC)phase_class/tools/xmlreader/m_entities.o\
	$(SRC)phase_class/tools/xmlreader/m_fsm.o\
	$(SRC)phase_class/tools/xmlreader/m_xml_error.o\
	$(SRC)min3p/gen.o\
	$(SRC)min3p/pressure_melt_k.o\
	$(SRC)min3p/biol.o\
	$(SRC)usg/fluxvs_usg.o\
	$(SRC)usg/fluxdd_usg.o\
	$(SRC)usg/fluxfs_usg.o\
	$(SRC)min3p/darcy_energybal.o\
	$(SRC)usg/darcy_energybal_usg.o\
	$(SRC)min3p/satcorr_curve.o\
	$(SRC)min3p/relcond_freezing.o\
	$(SRC)min3p/relheat_freezing.o\
	$(SRC)phase_class/tools/xmlreader/m_xml_parser.o\
	$(SRC)solver/solver_lis.o\
	$(SRC)parallel_io/m_binary_mpiio.o\
	$(SRC)phase_class/tools/xmlreader/flib_sax.o\
	$(SRC)usg/diff_vapour_usg.o\
	$(SRC)usg/metis_iface.o\
	$(SRC)min3p/file_unit.o\
	$(SRC)min3p/file_utility.o\
	$(SRC)phase_class/tools/xpath/m_path.o\
	$(SRC)phase_class/tools/xmlreader/m_handlers.o\
	$(SRC)solver/solver_results.o\
	$(SRC)phase_class/tools/xpath/flib_xpath.o\
	$(SRC)phase_class/tools/m_general_tools_cheproo.o\
	$(SRC)solver/solver_runtime.o\
	$(SRC)solver/matrix_utility.o\
	$(SRC)usg/gms_mesh_domain.o\
	$(SRC)solver/solver_pardiso.o\
	$(SRC)phase_class/species/m_species.o\
	$(SRC)usg/usg_mesh_generation.o\
	$(SRC)phase_class/phase/m_parentphase.o\
	$(SRC)phase_class/phase/m_parentaqueousphase.o\
	$(SRC)phase_class/phase/m_aqueousphasebdot.o\
	$(SRC)phase_class/phase/m_aqueousphasedavis.o\
	$(SRC)phase_class/phase/m_aqueousphaseideal.o\
	$(SRC)phase_class/phase/m_aqueousphasepitzer.o\
	$(SRC)phase_class/phase/m_aqueousphase.o\
	$(SRC)phase_class/phase/m_phase.o\
	$(SRC)min3p/chem.o\
	$(SRC)noble_gas/nobleGasIngrowth.o\
	$(SRC)min3p/serveParams.o\
	$(SRC)solver/solver.o\
	$(SRC)usg/usg_mesh_data.o\
	$(SRC)usg/usg_mesh_surface.o\
	$(SRC)usg/usg_ice_sheet.o\
	$(SRC)usg/hdf5_usg.o\
	$(SRC)usg/read_zone_usg.o\
	$(SRC)solver/solver_ddmethod.o\
	$(SRC)usg/gradient_usg.o\
	$(SRC)usg/usg_face_utility.o\
	$(SRC)min3p/freezing_adjacent_bd.o\
	$(SRC)min3p/zero_r8_parallel.o\
	$(SRC)min3p/vapor_prop.o\
	$(SRC)min3p/trans_evap_info.o\
	$(SRC)min3p/tranbcrt.o\
	$(SRC)min3p/stddmass.o\
	$(SRC)min3p/seepface.o\
	$(SRC)min3p/readsorb.o\
	$(SRC)min3p/ratemin_new.o\
	$(SRC)mip_mt/mip_math.o\
	$(SRC)mip_mt/mip_bubble.o\
	$(SRC)mip_mt/mip_output.o\
	$(SRC)min3p/root_output.o\
	$(SRC)min3p/opnpgfls.o\
	$(SRC)min3p/opnmbfls.o\
	$(SRC)min3p/mem_nm.o\
	$(SRC)min3p/mem_irm.o\
	$(SRC)min3p/mem_heat.o\
	$(SRC)min3p/initsatw.o\
	$(SRC)min3p/initcpevaporation.o\
	$(SRC)usg/iajart_usg.o\
	$(SRC)min3p/dratemin_new.o\
	$(SRC)min3p/ddfsflow.o\
	$(SRC)min3p/chrgcorr.o\
	$(SRC)min3p/assnpair.o\
	$(SRC)min3p/zero_i4_parallel.o\
	$(SRC)min3p/updatevs.o\
	$(SRC)min3p/tprfvs.o\
	$(SRC)min3p/rstatgs.o\
	$(SRC)usg/outputvs_usg.o\
	$(SRC)min3p/outputvs.o\
	$(SRC)usg/outputmech_usg.o\
	$(SRC)min3p/outputmech.o\
	$(SRC)usg/outputaca_usg.o\
	$(SRC)min3p/msysdd.o\
	$(SRC)min3p/mem_nc.o\
	$(SRC)min3p/mem_naq.o\
	$(SRC)min3p/initprob.o\
	$(SRC)min3p/initpprt.o\
	$(SRC)min3p/infheat_d.o\
	$(SRC)min3p/infheat_c.o\
	$(SRC)min3p/i2upfind.o\
	$(SRC)min3p/drategas.o\
	$(SRC)min3p/cvolume.o\
	$(SRC)icesheet/comp_bc_ice_sheet.o\
	$(SRC)min3p/updtsurf.o\
	$(SRC)min3p/totmin.o\
	$(SRC)min3p/totconcg.o\
	$(SRC)min3p/stedflow.o\
	$(SRC)min3p/readredx_new.o\
	$(SRC)min3p/radiation.o\
	$(SRC)usg/outputdd_usg.o\
	$(SRC)min3p/mem_mat.o\
	$(SRC)min3p/mem_bbls.o\
	$(SRC)min3p/jacvs.o\
	$(SRC)min3p/jacddvs.o\
	$(SRC)min3p/initcplc.o\
	$(SRC)usg/iajavs_usg.o\
	$(SRC)min3p/iajavs.o\
	$(SRC)min3p/gasconc.o\
	$(SRC)min3p/findpath.o\
	$(SRC)min3p/dratemin_salinity.o\
	$(SRC)min3p/velocity_a.o\
	$(SRC)min3p/updatedd_energybalance.o\
	$(SRC)min3p/totdyvisc.o\
	$(SRC)min3p/initfaceflux.o\
	$(SRC)min3p/tprfvs_faceflux.o\
	$(SRC)min3p/tprfrtlc_faceflux.o\
	$(SRC)min3p/timeloop.o\
	$(SRC)min3p/storvs.o\
	$(SRC)min3p/restart_w.o\
	$(SRC)min3p/readgses.o\
	$(SRC)min3p/reactran_2.o\
	$(SRC)dgm/rateh2o.o\
	$(SRC)min3p/opnplfls.o\
	$(SRC)min3p/mem_dd.o\
	$(SRC)min3p/jacbvs.o\
	$(SRC)min3p/initppvs.o\
	$(SRC)min3p/initcpenergybal.o\
	$(SRC)usg/infcrtdd_usg.o\
	$(SRC)gas_advection/giups.o\
	$(SRC)min3p/draterdx.o\
	$(SRC)dgm/dgm_dfluxdg.o\
	$(SRC)min3p/ddbdflux.o\
	$(SRC)min3p/assngaspc.o\
	$(SRC)min3p/tsteprt.o\
	$(SRC)min3p/tstepfs.o\
	$(SRC)min3p/tortuosity_vap.o\
	$(SRC)min3p/readsspc.o\
	$(SRC)min3p/ratemin.o\
	$(SRC)min3p/msysrt.o\
	$(SRC)min3p/mem_nx.o\
	$(SRC)min3p/binmatevap.o\
	$(SRC)min3p/binmattransp.o\
	$(SRC)min3p/initgeom.o\
	$(SRC)min3p/initcpvs.o\
	$(SRC)min3p/infcvs.o\
	$(SRC)min3p/iajavs_dp.o\
	$(SRC)dgm/gasdiff.o\
	$(SRC)min3p/findname.o\
	$(SRC)parallel_io/dumpstring.o\
	$(SRC)min3p/diff_grad.o\
	$(SRC)solver/ws209.o\
	$(SRC)min3p/updtsvmp.o\
	$(SRC)min3p/tprfdd.o\
	$(SRC)min3p/totcona.o\
	$(SRC)min3p/readminx.o\
	$(SRC)min3p/nexttime.o\
	$(SRC)min3p/mem_nsb.o\
	$(SRC)min3p/mbaldd.o\
	$(SRC)min3p/jaclc.o\
	$(SRC)min3p/jacddfs.o\
	$(SRC)usg/infcrt_g_usg.o\
	$(SRC)min3p/icbcrt.o\
	$(SRC)min3p/hydrostatic_pitzerdens.o\
	$(SRC)usg/fluxd_usg.o\
	$(SRC)min3p/drateint_new.o\
	$(SRC)min3p/datstr_1.o\
	$(SRC)min3p/bdryflux.o\
	$(SRC)usg/velodd_usg.o\
	$(SRC)usg/velocity_g_usg.o\
	$(SRC)min3p/updtbcatm.o\
	$(SRC)min3p/updtbcenergybal.o\
	$(SRC)min3p/updatelc.o\
	$(SRC)min3p/totconc.o\
	$(SRC)min3p/storfs.o\
	$(SRC)min3p/readzone.o\
	$(SRC)min3p/readmin.o\
	$(SRC)min3p/reactran.o\
	$(SRC)min3p/outputlc.o\
	$(SRC)dgm/ms_fluxdg_s.o\
	$(SRC)min3p/mem_evap.o\
	$(SRC)min3p/jacbrt.o\
	$(SRC)min3p/initopgs.o\
	$(SRC)min3p/initcpdd.o\
	$(SRC)min3p/initbcrt.o\
	$(SRC)min3p/guess.o\
	$(SRC)min3p/evap.o\
	$(SRC)min3p/ddbdflux_energybal.o\
	$(SRC)min3p/aratemin.o\
	$(SRC)min3p/xyzcoord.o\
	$(SRC)min3p/tranflow.o\
	$(SRC)min3p/totint.o\
	$(SRC)min3p/solvsg.o\
	$(SRC)min3p/readtemp.o\
	$(SRC)min3p/readint.o\
	$(SRC)min3p/rateint_new.o\
	$(SRC)min3p/phpe.o\
	$(SRC)min3p/mem_njavs.o\
	$(SRC)min3p/initpppm.o\
	$(SRC)min3p/pitzer/initpitzer_pc.o\
	$(SRC)usg/infevap_usg.o\
	$(SRC)min3p/infcrt_mcd.o\
	$(SRC)min3p/gcconst.o\
	$(SRC)gas_advection/gasd_m.o\
	$(SRC)min3p/energysys.o\
	$(SRC)dgm/dgm_fluxdg_s.o\
	$(SRC)min3p/clstpfls.o\
	$(SRC)min3p/weed.o\
	$(SRC)min3p/velocity.o\
	$(SRC)min3p/updtbcvs.o\
	$(SRC)min3p/updtdisprt.o\
	$(SRC)dgm/totint_w.o\
	$(SRC)min3p/readint_new.o\
	$(SRC)min3p/pitzer/pitzer.o\
	$(SRC)min3p/outputdd.o\
	$(SRC)min3p/molconc.o\
	$(SRC)min3p/mem_nr.o\
	$(SRC)min3p/inittsgs.o\
	$(SRC)min3p/initppdd.o\
	$(SRC)gas_advection/infcvs_cp.o\
	$(SRC)gas_advection/gasp.o\
	$(SRC)min3p/gasbub.o\
	$(SRC)min3p/dtotconc.o\
	$(SRC)min3p/draoult.o\
	$(SRC)dgm/dgm_fluxdg.o\
	$(SRC)min3p/ddvsflow.o\
	$(SRC)min3p/tstepheat.o\
	$(SRC)min3p/updateheat.o\
	$(SRC)min3p/jacheat.o\
	$(SRC)min3p/jacbheat.o\
	$(SRC)min3p/heattran.o\
	$(SRC)min3p/checkerr.o\
	$(SRC)min3p/velodd.o\
	$(SRC)min3p/updatedd.o\
	$(SRC)min3p/tstepvs.o\
	$(SRC)min3p/tcorr.o\
	$(SRC)min3p/surf_tens_prop.o\
	$(SRC)min3p/readminx_new.o\
	$(SRC)min3p/rateredx.o\
	$(SRC)min3p/rate_gasmin.o\
	$(SRC)dgm/ms_fluxdg.o\
	$(SRC)dgm/lennard_jones.o\
	$(SRC)min3p/jacbevap.o\
	$(SRC)min3p/initppenergybal.o\
	$(SRC)min3p/initbcvs.o\
	$(SRC)min3p/initbcice.o\
	$(SRC)min3p/updtbcice.o\
	$(SRC)icesheet/updtice_scalfac.o\
	$(SRC)usg/infcrt_mcd_usg.o\
	$(SRC)usg/hydrostatic_pitzerdens_usg.o\
	$(SRC)dgm/gasdiff_s.o\
	$(SRC)min3p/energy_bal.o\
	$(SRC)min3p/datstr_n.o\
	$(SRC)min3p/welcome_pc.o\
	$(SRC)usg/velocity_usg.o\
	$(SRC)min3p/updtsvgp.o\
	$(SRC)min3p/readredx.o\
	$(SRC)usg/outputrt_usg.o\
	$(SRC)min3p/mem_ntmp.o\
	$(SRC)min3p/mem_nmx.o\
	$(SRC)min3p/jacsurf.o\
	$(SRC)min3p/initcsys.o\
	$(SRC)min3p/initcpice.o\
	$(SRC)min3p/initppsn.o\
	$(SRC)min3p/infcrt_g.o\
	$(SRC)min3p/icrtlczn.o\
	$(SRC)min3p/gcreact.o\
	$(SRC)min3p/driver_pc.o\
	$(SRC)min3p/drate_gasmin.o\
	$(SRC)min3p/cbalance.o\
	$(SRC)min3p/vsflow.o\
	$(SRC)min3p/visconew.o\
	$(SRC)min3p/updatert.o\
	$(SRC)min3p/tsteplc.o\
	$(SRC)min3p/sortcomp.o\
	$(SRC)min3p/seepfdd.o\
	$(SRC)min3p/readtime.o\
	$(SRC)min3p/readcomp.o\
	$(SRC)min3p/phcorr.o\
	$(SRC)min3p/opnmbfls_mcd.o\
	$(SRC)min3p/modrate.o\
	$(SRC)min3p/mbalrt.o\
	$(SRC)min3p/jacbvs_energybal.o\
	$(SRC)min3p/initweed.o\
	$(SRC)min3p/initcpgs.o\
	$(SRC)min3p/readsit.o\
	$(SRC)usg/infcrt_a_usg.o\
	$(SRC)min3p/iajart.o\
	$(SRC)dgm/gasdiff2_s.o\
	$(SRC)gas_advection/fluxvg.o\
	$(SRC)min3p/dratemin.o\
	$(SRC)min3p/diffcoff_mcd.o\
	$(SRC)min3p/ddtds_energybal.o\
	$(SRC)min3p/cliqdisp.o\
	$(SRC)min3p/batreac.o\
	$(SRC)gas_advection/velocity_g.o\
	$(SRC)min3p/tranunit.o\
	$(SRC)min3p/sorbspc.o\
	$(SRC)min3p/rtrvpprm.o\
	$(SRC)min3p/restart_r.o\
	$(SRC)min3p/rategas.o\
	$(SRC)min3p/opndbfls_pc.o\
	$(SRC)min3p/mem_vs.o\
	$(SRC)min3p/mem_rt.o\
	$(SRC)min3p/initicvs.o\
	$(SRC)usg/outputice_usg.o\
	$(SRC)min3p/initiice.o\
	$(SRC)usg/infheat_d_usg.o\
	$(SRC)min3p/iajavs_energybal.o\
	$(SRC)min3p/iajabl.o\
	$(SRC)gas_advection/gasv.o\
	$(SRC)gas_advection/gasp_m.o\
	$(SRC)dgm/drt_hubb.o\
	$(SRC)min3p/compute_sensible.o\
	$(SRC)min3p/tprfrtlc.o\
	$(SRC)min3p/totredx.o\
	$(SRC)min3p/setsize.o\
	$(SRC)min3p/rootwat.o\
	$(SRC)min3p/readmin_new.o\
	$(SRC)min3p/rateint.o\
	$(SRC)min3p/outputrt.o\
	$(SRC)min3p/mem_hmcd.o\
	$(SRC)min3p/mbal_mcd.o\
	$(SRC)min3p/jacrt.o\
	$(SRC)min3p/jacfs.o\
	$(SRC)min3p/initicrt.o\
	$(SRC)min3p/initbcdd.o\
	$(SRC)min3p/indextec.o\
	$(SRC)min3p/drateint.o\
	$(SRC)min3p/constnts.o\
	$(SRC)min3p/clsgfls.o\
	$(SRC)min3p/velovapour.o\
	$(SRC)usg/velocity_a_usg.o\
	$(SRC)min3p/updtbcdd.o\
	$(SRC)min3p/trddflow.o\
	$(SRC)min3p/surfcomp.o\
	$(SRC)dgm/read_tau.o\
	$(SRC)min3p/opntemp.o\
	$(SRC)min3p/minmaxwd.o\
	$(SRC)min3p/mbalvs.o\
	$(SRC)min3p/inittemp.o\
	$(SRC)min3p/initcprt.o\
	$(SRC)min3p/initcpgb.o\
	$(SRC)usg/infcvs_usg.o\
	$(SRC)min3p/infcrt_a.o\
	$(SRC)min3p/fsflow.o\
	$(SRC)min3p/fluxv_vl.o\
	$(SRC)min3p/flux_calc.o\
	$(SRC)min3p/distmp.o\
	$(SRC)min3p/ddtds.o\
	$(SRC)min3p/checksat.o\
	$(SRC)min3p/atotconc.o\
	$(SRC)dgm/totmin_w.o\
	$(SRC)min3p/sptldisc.o\
	$(SRC)min3p/rstatlc.o\
	$(SRC)min3p/raoult.o\
	$(SRC)min3p/opnenergybal.o\
	$(SRC)min3p/msysvs.o\
	$(SRC)dgm/ms_dfluxdg.o\
	$(SRC)min3p/mem_ng.o\
	$(SRC)min3p/intpolt.o\
	$(SRC)min3p/initicdd.o\
	$(SRC)min3p/initbcenergybal.o\
	$(SRC)min3p/infevap.o\
	$(SRC)min3p/i2upfind_heat.o\
	$(SRC)dgm/gasdiff2.o\
	$(SRC)min3p/elecmigration.o\
	$(SRC)min3p/comptotc.o\
	$(SRC)usg/velovapour_usg.o\
	$(SRC)min3p/updtsvap.o\
	$(SRC)dgm/totredx_w.o\
	$(SRC)min3p/totconfac.o\
	$(SRC)min3p/readbloc.o\
	$(SRC)min3p/rategasd.o\
	$(SRC)min3p/opngfls.o\
	$(SRC)min3p/mem_njart.o\
	$(SRC)min3p/jacdd_energybal.o\
	$(SRC)min3p/initicenergybal.o\
	$(SRC)usg/infheat_c_usg.o\
	$(SRC)min3p/infcrtdd.o\
	$(SRC)min3p/drategdd.o\
	$(SRC)min3p/cputime_pc.o\
	$(SRC)min3p/pitzer/closepitzer.o\
	$(SRC)min3p/mem_etr.o\
	$(SRC)min3p/initplant.o\
	$(SRC)min3p/rew.o\
	$(SRC)min3p/evapo.o\
	$(SRC)min3p/updtetp.o\
	$(SRC)min3p/updtrootdensity.o

min3p: $(SOURCES)
	$(FC) $(FFLAGS) $(FPPFLAGS) -o $(EXENAME) $(SOURCES) $(LIS_LIB) $(LIB) $(SLIB)
%.o:%.F90
	$(FC) $(FFLAGS) $(FPPFLAGS) -c $(OPTFLAGS) $< -o $@
%.o:%.cpp
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $(OPTFLAGS) $< -o $@
clean:
	${RM} $(SRC)*.o $(SRC)energy_balance/*.o $(SRC)icesheet/*.o $(SRC)min3p/*.o 
	${RM} $(SRC)min3p/pitzer/*.o $(SRC)phase_class/constants/*.o $(SRC)noble_gas/*.o 
	${RM} $(SRC)phase_class/phase/*.o $(SRC)phase_class/species/*.o 
	${RM} $(SRC)phase_class/tools/*.o $(SRC)phase_class/tools/xmlreader/*.o 
	${RM} $(SRC)phase_class/tools/xpath/*.o $(SRC)version/*.o
	${RM} $(SRC)solver/*.o $(SRC)gas_advection/*.o 
	${RM} $(SRC)parallel_io/*.o $(SRC)dgm/*.o $(SRC)usg/*.o $(SRC)mip_mt/*.o *.o
	${RM} $(SRC)*.mod $(SRC)energy_balance/*.mod $(SRC)icesheet/*.mod $(SRC)min3p/*.mod 
	${RM} $(SRC)min3p/pitzer/*.mod $(SRC)phase_class/constants/*.mod $(SRC)noble_gas/*.mod
	${RM} $(SRC)phase_class/phase/*.mod $(SRC)phase_class/species/*.mod 
	${RM} $(SRC)phase_class/tools/*.mod $(SRC)phase_class/tools/xmlreader/*.mod 
	${RM} $(SRC)phase_class/tools/xpath/*.mod $(SRC)version/*.mod
	${RM} $(SRC)solver/*.mod $(SRC)gas_advection/*.mod 
	${RM} $(SRC)parallel_io/*.mod $(SRC)dgm/*.mod $(SRC)usg/*.mod $(SRC)mip_mt/*.mod *.mod
