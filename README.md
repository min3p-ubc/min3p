MIN3P: Multicomponent Reactive Transport Code


    ------------------------------------------------
          M     M  I  N     N  333333  PPPPPP       
          MM   MM  I  NN    N       3  P    P       
          M M M M  I  N N   N       3  P    P       
          M  M  M  I  N  N  N    3333  PPPPPP       
          M     M  I  N   N N       3  P            
          M     M  I  N    NN       3  P            
          M     M  I  N     N  333333  P            

          A Numerical Model for the Analysis
                       of the
                Geochemical Evolution
                         of
                   Variable-Density
              Mineral-Water-Gas-Systems
    ------------------------------------------------

Overview
------------------------------------------------------------------------------------------

MIN3P is a general-purpose reactive transport code for simulating variably saturated flow and multicomponent reactive transport in porous media. Originally developed at the University of Waterloo and the University of British Columbia, MIN3P has been continuously enhanced to address a wide range of subsurface environmental problems including contaminant migration, geochemical weathering, carbon sequestration, and acid mine drainage.

MIN3P tightly couples fluid flow, solute transport, and geochemical reactions, enabling robust simulation of complex, non-linear processes in natural and engineered subsurface systems.

Key Features
------------------------------------------------------------------------------------------

Variably saturated flow: Solves Richards' equation for unsaturated–saturated flow conditions
Multicomponent reactive transport: Handles aqueous complexation, mineral dissolution/precipitation, ion exchange, surface complexation, and gas transfer
Tight coupling: Flow, transport, and reaction equations solved in a fully coupled framework
Multi-phase systems: Gas, aqueous, and solid phases considered simultaneously

Contacts & Link
------------------------------------------------------------------------------------------

**Git Repository**: https://github.com/min3p-ubc/min3p

**MIN3P Website**: https://www.min3p.com/

Dr. Uli Mayer (code owner, maintainer, core developer)

https://www.eoas.ubc.ca/people/ulrichmayer

https://www.linkedin.com/in/uli-mayer-b10a5418/

Dr. Danyang Su (code maintainer, core developer)

https://www.linkedin.com/in/danyang-su-7916a177/

Latest Developments
------------------------------------------------------------------------------------------

Parallel Computing for Large-Scale Simulation: One of the most significant recent enhancements to MIN3P is the implementation of high-performance parallel computing capabilities (MPI and OpenMP), enabling simulations at previously intractable spatial and temporal scales.

Unstructured Grid Capability for Complex Geometry: MIN3P now supports fully unstructured computational grids, a major advancement over its original structured (orthogonal) grid framework. This enables accurate representation of geometrically complex domains.

Development History
------------------------------------------------------------------------------------------

1999

Initial development of MIN3P at the University of Waterloo by U. Mayer as part of his Ph.D. thesis (Mayer, 1999; Mayer et al, 2002). At that time, the code was able to simulate multicomponent reactive transport in variably-saturated porous media. The applications of the code included the generation and fate of acid mine drainage in unsaturated porous media, the in-situ remediation of groundwater contaminated by inorganic (e.g. hexavalent chromium) and organic contaminants (e.g. chlorinated organic compounds).

2006 

1) Dual porosity model (MIN3P-DUAL) added by L. Cheng, (PhD work at University of Sheffield, UK) (Cheng, 2006). This model was used in the assessment of the fate and transport of MTBE in a Chalk aquifer. 

2) Gas exsolution, entrapment and release model (MIN3P-BUBBLE) incorporated by R. Amos (PhD work at the University of British Columbia; Amos and Mayer, 2006). The problems investigated with this version of the code were primarily related to bubble growth and contraction due to in-situ gas production or consumption, bubble entrapment due to water table rise and subsequent re-equilibration of the bubble with ambient groundwater, and permeability changes due to  gas formation and entrapment.

2007

Multicomponent gas phase diffusion and advection model (MIN3P-DUSTY) added by S. Molins at the University of British Columbia (Molins and Mayer, 2007). The model was used to simulate gas attenuation in partially saturated landfill soil covers, methane production, and oxidation in aquifers contaminated by organic compounds (e.g. an oil spill site) and pyrite oxidation in mine tailings.

2009

Density coupling between flow and reactive transport included by T. Henderson as part of his Ph.D. thesis at the University of British Columbia. This code version was named MIN3P-D (Henderson et al., 2009) and was used to simulate permanganate-based remediation under free convection conditions, considering contaminant treatment, and geochemical reactions including the oxidation of naturally occurring organic matter (e.g. DNAPL) using the oxidant (KMnO4), mineral dissolution and precipitation, and ion exchange reactions.

2012

Pitzer equations for activity corrections, energy balance, and a formulation for 1D vertical stress were implemented by Bea et al. (2011, 2012). The resulting code was renamed to MIN3P-THCm.

2014

Multicomponent diffusion to account for the species dependent diffusion coefficient and to maintain local charge balance and multisite ion exchange models were implemented by Rasouli and Xie (Xie et al., 2015; Rasouli, 2016). The code is also parallelized using thread acceleration and domain decomposition methods (Su et al., 2015).

2016

Salinity dependent SRB model was added. Development of MIN3P unstructured capability was initiated.

2017

LIS iterative solver was implemented to the current version. First order gas decay model was added to the current version. SIT model and pore clogging function was added to the code.

2018

MIN3P-HPC code was released. The unstructured capabilities support different mesh types including triangular mesh, quadrilateral mesh in 2D and prism mesh, hexahedral mesh and tetrahedral mesh in 3D. The new code has been optimized for OpenMP parallel version based on thread acceleration, MPI parallel version based on domain decomposition and hybrid MPI-OpenMP parallel version.

2019

ArchiSimple, a dynamic root architecture model was added to the MIN3P-HPC code. 

Publications
------------------------------------------------------------------------------------------

Mining
------------------------------------------------------------------------------------------

Yi, X., Su, D., Bussière, B., Mayer, K. U. (2021). Thermal-hydrological-chemical modeling of a covered waste rock pile in a permafrost region, Minerals, 11(6):565. https://doi.org/10.3390/min11060565

Raymond, K. E., Seigneur, N., Su, D., Mayer, K. U. (2021). Investigating the influence of structure and heterogeneity in waste rock piles on mass loading rates – A reactive transport modeling study, Frontiers in Water 3:618418, https://doi.org/10.3389/frwa.2021.618418  

Yi, X., Su, D., Seigneur, N., Mayer K. U. (2021). Modeling of thermal-hydrological-chemical (THC) processes during waste rock weathering under permafrost conditions, Frontiers in Water, 3:645675, https://doi.org/10.3389/frwa.2021.645675

Seigneur, N., Vriens, B., Mayer, K. U., Beckie, R. D. (2021). Reactive transport modelling to investigate multi-scale waste rock weathering, Journal of Contaminant Hydrology, 236:103752, https://doi.org/10.1016/j.jconhyd.2020.103752  

Vriens, B., Seigneur, N., Mayer, K. U., Beckie, R. D. (2020). Scale dependence of effective geochemical rates in weathering mine waste rock, Journal of Contaminant Hydrology, 234:103699, https://doi.org/10.1016/j.jconhyd.2020.103699

Raymond, K. E., Seigneur, N., Su, D., Poaty, B., Plante, B., Bussière, B., Mayer, K. U. (2020). Numerical Modeling of a Laboratory-Scale Waste Rock Pile Featuring an Engineered Cover System. Minerals, 10, 652. https://doi.org/10.3390/min10080652

Javadi, M. (2019). Reactive transport modeling of unsaturated hydrology and geochemistry of neutral and acid rock drainage in highly heterogeneous mine waste rock at the Antamina Mine, Peru (T). University of British Columbia. 

Wilson, D., Amos, R. T., Blowes, D. W., Langman, J. B., Ptacek, C. J., Smith, L., & Sego, D. C. (2018). Diavik waste rock project: A conceptual model for temperature and sulfide-content dependent geochemical evolution of waste rock–Laboratory scale. Applied Geochemistry, 89, 160-172.

Wilson, D., Amos, R. T., Blowes, D. W., Langman, J. B., Smith, L., & Sego, D. C. (2018). Diavik Waste Rock Project: Scale-up of a reactive transport model for temperature and sulfide-content dependent geochemical evolution of waste rock. Applied Geochemistry, 96, 177-190.​

Pabst, T., Molson, J., Aubertin, M., & Bussière, B. (2017). Reactive transport modelling of the hydro-geochemical behaviour of partially oxidized acid-generating mine tailings with a monolayer cover. Applied Geochemistry, 78, 219-233.

Pedretti, D., Mayer, K. U., & Beckie, R. D. (2017). Stochastic multicomponent reactive transport analysis of low quality drainage release from waste rock piles: Controls of the spatial distribution of acid generating and neutralizing minerals. Journal of contaminant hydrology, 201, 30-38.

Mayer, K. U., Alt-Epping, P., Jacques, D., Arora, B., & Steefel, C. I. (2015). Benchmark problems for reactive transport modeling of the generation and attenuation of acid rock drainage. Computational geosciences, 19(3), 599-611.

Demers, I., Molson, J., Bussière, B., & Laflamme, D. (2013). Numerical modeling of contaminated neutral drainage from a waste-rock field test cell. Applied geochemistry, 33, 346-356.

Ouangrawa, M., Molson, J., Aubertin, M., Bussière, B., Zagury, G.J., (2009). Reactive transport modelling of mine tailings columns with capillarity-induced high water saturation for preventing sulfide oxidation. Appl. Geochemistry. https://doi.org/10.1016/j.apgeochem.2009.04.005

D’Affonseca, F.M., Blum, P., Finkel, M., Melzer, R., Grathwohl, P., (2008). Field scale characterization and modeling of contaminant release from a coal tar source zone. J. Contam. Hydrol. https://doi.org/10.1016/j.jconhyd.2008.03.011

D’Affonseca, F.M., Blum, P., Finkel, M., Melzer, R., Grathwohl, P., (2008). Modelling the source zone depletion and plume development of a coal-tar contaminated site. IAHS-AISH publication, 197-202.

Molson, J., Aubertin, M., Bussière, B., Benzaazoua, M., (2008). Geochemical transport modelling of drainage from experimental mine tailings cells covered by capillary barriers. Appl. Geochemistry. https://doi.org/10.1016/j.apgeochem.2007.08.004

Mayer, K. U., Benner, S. G., & Blowes, D. W. (2006). Process-based reactive transport modeling of a permeable reactive barrier for the treatment of mine drainage. Journal of contaminant hydrology, 85(3-4), 195-211.

Ouangrawa, M., Molson, J., Aubertin, M., Zagury, G., Bussière, B., (2006). The effect of water table elevation on acid mine drainage from reactive tailings: A laboratory and numerical modeling study. In Proceedings of the 7th International Conference on Acid Rock Drainage (ICARD), St. Louis, Mo (Vol. 26, p. 30).

Amos, R. T., Mayer, K. U., Blowes, D. W., & Ptacek, C. J. (2004). Reactive transport modeling of column experiments for the remediation of acid mine drainage. Environmental science & technology, 38(11), 3131-3138.​

Jurjovec, J., D. W. Blowes, C. J. Ptacek, and K. U. Mayer (2004). Multicomponent reactive transport modeling of acid neutralization reactions in mine tailings, Water Resources Research, 40, W11202, doi:10.1029/2003WR002233.

Bain, J. G., Mayer, K. U., Blowes, D. W., Frind, E. O., Molson, J. W. H., Kahnt, R., & Jenk, U. (2001). Modelling the closure-related geochemical evolution of groundwater at a former uranium mine. Journal of Contaminant Hydrology, 52(1-4), 109-135.

Deep Geologic Repositories
------------------------------------------------------------------------------------------

Su, D., M., Xie, Mayer, K.U. and MacQuarrie, K.T.B. (2022). The Impact of Ice Sheet Geometry on Meltwater Ingress and Reactive Solute Transport in Sedimentary Basins, Water Resources Research, 2022, 58, e2022WR032353.

Xie, M., D. Su, Mayer, K.U. and MacQuarrie, K.T.B. (2022). Reactive transport investigations of the long-term geochemical evolution of a multibarrier system including bentonite, low-alkali concrete and host rock, Applied Geochemistry, 105385 

Xie, M., Su, D., K.U. Mayer and K.T.B. MacQuarrie (2018). Reactive Transport Modelling Investigation of Elevated Dissolved Sulphide Concentrations in Sedimentary Basin Rocks, NWMO Technical Report, NWMO-TR-2018-07.

Su, D., K.U. Mayer and K.T.B. MacQuarrie (2015). Parallelization of the Reactive Transport Code MIN3P-THCm, NWMO Technical Report, NWMO-TR-2015-23.

Xie, M., P. Rasouli, K.U. Mayer and K.T.B. MacQuarrie (2015). MIN3P-THCm Code Enhancements for Reactive Transport Modelling in Low Permeability Media, NWMO Technical Report, NWMO-TR-2015-12 October 2015.

Xie, M., P. Rasouli, K.U. Mayer and K.T.B. MacQuarrie (2014). Reactive Transport Modelling in Low Permeability Media – MIN3P-THCm Simulations of EBS TF-C Compacted Bentonite Diffusion Experiments, NWMO Technical Report, NWMO-TR-2014-23, December 2014.

Xie, M., P. Rasouli, K.U. Mayer and K.T.B. MacQuarrie (2014). Reactive Transport Modelling of In-situ Diffusion Experiments for the Mont Terri Project - MIN3P-THCm Code Enhancement and Numerical Simulations, NWMO Technical Report, NWMO-TR-2014-25, December 2014.

Bea, S.A., K.U. Mayer, K.T.B. MacQuarrie (2011). Modelling Reactive Transport in Sedimentary Rock Environments - Phase II MIN3P-NWMO code enhancements and illustrative simulations for a glaciation scenario. Technical report: NWMO TR-2011-13.

Spiessl, S.M., K.U. Mayer and K.T.B. MacQuarrie (2009). Reactive Transport Modelling in Fractured Rock – Redox Stability Study, Technical report: NWMO TR-2009-04.

Oil & Gas
------------------------------------------------------------------------------------------

Forde, O.N., Mayer, K.U., Cahill, A.G., Mayer, B., Cherry, J., Parker, B. (2018). Vadose zone gas migration and surface effluxes following a controlled natural gas release into an unconfined shallow aquifer. Vadose Zone J. 17:180033.

Hers, I., Jourabchi, P., Lahvis, M.A., Dahlen, P., Luo, E.H., Johnson, P., Devaull, G.E., Mayer, K.U., (2014). Evaluation of seasonal factors on petroleum hydrocarbon vapor biodegradation and intrusion potential in a cold climate. Groundw. Monit. Remediat. https://doi.org/10.1111/gwmr.12085

Hers, I., Jourabchi, P., Menatti, J. A., & Suuberg, E. M. (2012). 3-D modeling of aerobic biodegradation of petroleum vapors: Effect of building area size on oxygen concentration below the slab. In Proceedings of Air Waste Management Association (AWMA) Vapor Intrusion 2010 Conference.

Hers, I., Lingle, J., Dombrowski, F., Murphy, E., Rees, T., Jourabchi, P., Mayer, K.U., (2010). EPRI Soil Vapor Intrusion Field Research Program - Evaluation of soil vapor attenuation above residual MGP impacts at a site in Wisconsin.

Broholm, M.M., Christophersen, M., Maier, U., Stenby, E.H., Höhener, P., Kjeldsen, P., (2005). Compositional evolution of the emplaced fuel source in the vadose zone field experiment at airbase Værløse, Denmark. Environ. Sci. Technol. https://doi.org/10.1021/es048557s

Maier, U., Mayer, U., Grathwohl, P., (2005). Natural attenuation of volatile hydrocarbons in the unsaturated zone - Modelling for the Værløse field site. IAHS-AISH Publ.

Grathwohl, P., & Maier, U. (2002). Natural Attenuation of Volatile Hydrocarbons in Unsaturated Soil Zone. Journal of Agricultural and Marine Sciences [JAMS], 7(2), 9-15.

Carbon Sequestration
------------------------------------------------------------------------------------------
Jia, M., Lapen, D. R., Su, D., Mayer, K. U., (2025), Multi-Domain Reactive Transport Modeling of GHG Emissions From Macroporous Agricultural Soils With a Focus on N2O Hotspots and Hot Moments, Water Resources Research, 61(11), e2025WR040588, https://doi.org/10.1029/2025WR040588

Harrison, A.L., Dipple, G.M., Power, I.M., Mayer, K.U., (2015). Influence of surface passivation and water content on mineral reactions in unsaturated porous media: Implications for brucite carbonation and CO2 sequestration. Geochim. Cosmochim. Acta. https://doi.org/10.1016/j.gca.2014.10.020

Bea, S., Wilson, S., Mayer, K., Dipple, G., Power, I., Gamazo, P. (2012). Reactive transport modeling of natural carbon sequestration in ultramafic mine tailings. Vadose Zone Journal, 11(2), 1-17.

Soil & Plants
------------------------------------------------------------------------------------------

Gatz-Miller, H. S., Gérard, F., Verrecchia, E. P., Su, D. and Mayer, K. U. (2022). Reactive transport modelling the oxalate-carbonate pathway of the Iroko tree; Investigation of calcium and carbon sinks and sources, Geoderma, 2022, 410, 115665.

Braghiere R K, Gérard F, Evers J B, Pradal C and Pagès L (2020). Simulating the effects of water limitation on plant biomass using a 3D functional–structural plant model of shoot and root driven by soil hydraulics Ann. Bot. 126 713–28. https://academic.oup.com/aob/advance-article/doi/10.1093/aob/mcaa059/5816312

Jia, M., Jacques, D., Gérard, F., Su, D., Mayer, K. U., & Šimůnek, J. (2019). A benchmark for soil organic matter degradation under variably saturated flow conditions. Computational Geosciences, 1-19.

Gérard, F., Blitz-Frayret, C., Hinsinger, P., Pagès, L., (2017). Modelling the interactions between root system architecture, root functions and reactive transport processes in soil. Plant Soil. https://doi.org/10.1007/s11104-016-3092-x

Bao, Z., Haberer, C., Maier, U., Beckingham, B., Amos, R.T., Grathwohl, P., (2015). Modeling long-term uptake and re-volatilization of semi-volatile organic compounds (SVOCs) across the soil-atmosphere interface. Sci. Total Environ. https://doi.org/10.1016/j.scitotenv.2015.08.104

Maier, U., Flegr, M., Rügner, H., Grathwohl, P., (2013). Long-term solute transport and geochemical equilibria in seepage water and groundwater in a catchment cross section. Environ. Earth Sci. https://doi.org/10.1007/s12665-013-2393-0

De Biase, C., Maier, U., Oswald, S., Thullner, M., (2012). Reactive transport simulation of volatile organic compound removal in vertical flow soil filters. IAHS-AISH publication, 355, 169-174.

Moradi, A.B., Oswald, S.E., Nordmeyer-Massner, J.A., Pruessmann, K.P., Robinson, B.H., Schulin, R., (2010). Analysis of nickel concentration profiles around the roots of the hyperaccumulator plant Berkheya coddii using MRI and numerical simulations. Plant Soil. https://doi.org/10.1007/s11104-009-0109-8

Gérard, F., Mayer, K.U., Hodson, M.J., Ranger, J., (2008). Modelling the biogeochemical cycle of silicon in soils: Application to a temperate forest ecosystem. Geochim. Cosmochim. Acta. https://doi.org/10.1016/j.gca.2007.11.010

Miller, G.R., Rubin, Y., Mayer, K.U., Benito, P.H., (2008). Modeling vadose zone processes during land application of food-processing waste water in California’s Central Valley. J. Environ. Qual. https://doi.org/10.2134/jeq2007.0320

Nowack, B., Mayer, K.U., Oswald, S.E., Van Beinum, W., Appelo, C.A.J., Jacques, D., Seuntjens, P., Gérard, F., Jaillard, B., Schnepf, A., Roose, T., (2006). Verification and intercomparison of reactive transport codes to describe root-uptake. Plant Soil. https://doi.org/10.1007/s11104-006-9017-3

Gérard, F., Tinsley, M., & Mayer, K. U. (2004). Preferential flow revealed by hydrologic modeling based on predicted hydraulic properties. Soil Science Society of America Journal, 68(5), 1526-1538.

Permeable Reactive Barriers
------------------------------------------------------------------------------------------

Bilardi, S., Amos, R.T., Blowes, D.W., Calabrò, P.S., Moraci, N., (2013). Reactive Transport Modeling of ZVI Column Experiments for Nickel Remediation. Groundw. Monit. Remediat. https://doi.org/10.1111/j.1745-6592.2012.01417.x

Weber, A., Ruhl, A.S., Amos, R.T., (2013). Investigating dominant processes in ZVI permeable reactive barriers using reactive transport modeling. J. Contam. Hydrol. https://doi.org/10.1016/j.jconhyd.2013.05.001

Jeen, S.-W., Amos, R.T., Blowes, D.W., (2012). Modeling gas formation and mineral precipitation in a granular iron column. Environ. Sci. Technol. https://doi.org/10.1021/es300299r

Pettenati, M., Croiset, N., Picot-Colbeaux, G., Casanova, J., Azaroual, M., Besnard, K., Rampnoux, N., 2012. Optimisation of wastewater treatments through combined geomaterials and natural soil filter: Modelling tools. J. Water Reuse Desalin. https://doi.org/10.2166/wrd.2012.023

Mayer, K. U., Benner, S. G., & Blowes, D. W. (2006). Process-based reactive transport modeling of a permeable reactive barrier for the treatment of mine drainage. Journal of contaminant hydrology, 85(3-4), 195-211.

Amos, R. T., Mayer, K. U., Blowes, D. W., & Ptacek, C. J. (2004). Reactive transport modeling of column experiments for the remediation of acid mine drainage. Environmental science & technology, 38(11), 3131-3138.​

Mayer, K. U., Blowes, D. W., Frind, E. O., (2001). Reactive transport modeling for the treatment of an in situ reactive barrier for the treatment of hexavalent chromium and trichloroethylene in groundwater. Water Resources Research, 37:3091-3103. doi:10:1029/2001WR000862.

Jeen, S.-W., Mayer, K.U., Gillham, R.W., Blowes, D.W., 2007. Reactive transport modeling of trichloroethene treatment with declining reactivity of iron. Environ. Sci. Technol. https://doi.org/10.1021/es062490m

MIN3P Code & Benchmarks
------------------------------------------------------------------------------------------
Su, D., MacQuarrie, K.T.B., Mayer, K.U., Quadrilateral mesh-based reactive transport modeling in non-orthogonal random fracture-matrix systems, Applied Computing and Geosciences, 2026, 30, 100353 

Su, D., Mayer, K.U., MacQuarrie, K.T.B. (2022). Implementation of 2D/3D-Unstructured Grid Capabilities into MIN3P-THCm, NWMO Technical Report, NWMO-TR-2022-10.

Su, D.; Xie, M., Mayer, K. U. and MacQuarrie, K. T. B. (2022), Simulation of diffusive solute transport in heterogeneous porous media with dipping anisotropy, Frontiers in Water, 4.

Su, D., Mayer, K.U., MacQuarrie, K.T.B. (2020). MIN3P-HPC: a high performance unstructured grid code for subsurface flow and reactive transport simulation, Mathematical Geosciences, 53-517-550, https://doi.org/10.1007/s11004-020-09898-7

Su, D., Mayer, K.U., MacQuarrie, K.T.B. (2020). Numerical investigation of flow instabilities using fully unstructured discretization for variably saturated flow problems, Advances in Water Resources, 143:103673, https://doi.org/10.1016/j.advwatres.2020.103673

Maher, K., Mayer K. U (2019). The art of reactive transport model building, Elements, 15: 117-118, https://doi:10.2138/gselements.15.2.117

Poonoosamy, J., Wanner, C., Alt Epping, P., Águila, J.F., Samper, J., Montenegro, L., Xie, M., Su, D., Mayer, K.U., Mäder, U., Van Loon, L.R., Kosakowski, G., 2018. Benchmarking of reactive transport codes for 2D simulations with mineral dissolution–precipitation reactions and feedback on transport parameters. Comput. Geosci. https://doi.org/10.1007/s10596-018-9793-x

Su, D. Mayer, K.U., MacQuarrie, K.T.B. Parallelization of MIN3P-THCm: a high performance computational framework for subsurface flow and reactive transport simulation, Environmental Modelling & Software, 95, 2017, 271-289.

Bea, S.A., Mayer K. U., MacQuarrie K. T. B. (2016). Reactive transport and thermo-hydro-mechanical coupling in deep sedimentary basins affected by glaciation cycles: model development, verification, and illustrative example, Geofluids, 16, 279–300.

Rasouli, P. (2016). The role of multicomponent diffusion and electrochemical migration for reactive transport in porous media, PhD-thesis, University of British Columbia, Vancouver, British Columbia, Canada.

Alt-Epping, P., Tournassat, C., Rasouli, P., Steefel, C. I., Mayer, K. U., Jenni, A., ... & Fernández, R. (2015). Benchmark reactive transport simulations of a column experiment in compacted bentonite with multispecies diffusion and explicit treatment of electrostatic effects. Computational geosciences, 19(3), 535-550.

Marty, N.C.M., Bildstein, O., Blanc, P., Claret, F., Cochepin, B., Gaucher, E.C., Jacques, D., Lartigue, J.-E., Liu, S., Mayer, K.U., Meeussen, J.C.L., Munier, I., Pointeau, I., Su, D., Steefel, C.I., (2015). Benchmarks for multicomponent reactive transport across a cement/clay interface. Comput. Geosci. https://doi.org/10.1007/s10596-014-9463-6

Mayer, K. U., Alt-Epping, P., Jacques, D., Arora, B., Steefel, C. I. (2015). Benchmark problems for reactive transport modeling of the generation and attenuation of acid rock drainage. Computational geosciences, 19(3), 599-611.

Molins, S., Greskowiak, J., Wanner, C., Mayer, K.U., (2015). A benchmark for microbially mediated chromium reduction under denitrifying conditions in a biostimulation column experiment. Comput. Geosci. https://doi.org/10.1007/s10596-014-9432-0

Perko, J., Mayer, K.U., Kosakowski, G., De Windt, L., Govaerts, J., Jacques, D., Su, D., Meeussen, J.C.L., (2015). Decalcification of cracked cement structures. Comput. Geosci. https://doi.org/10.1007/s10596-014-9467-2

Rasouli, P., Steefel, C. I., Mayer, K. U., & Rolle, M. (2015). Benchmarks for multicomponent diffusion and electrochemical migration. Computational Geosciences, 19(3), 523-533.

Şengör, S.S., Mayer, K.U., Greskowiak, J., Wanner, C., Su, D., Prommer, H., (2015). A reactive transport benchmark on modeling biogenic uraninite re-oxidation by Fe(III)-(hydr)oxides. Comput. Geosci. https://doi.org/10.1007/s10596-015-9480-0

Steefel, C.I., Appelo, C.A.J., Arora, B., Jacques, D., Kalbacher, T., Kolditz, O., Lagneau, V., Lichtner, P.C., Mayer, K.U., Meeussen, J.C.L., Molins, S., Moulton, D., Shao, H., Šimůnek, J., Spycher, N., Yabusaki, S.B., Yeh, G.T. (2015). Reactive transport codes for subsurface environmental simulation. Computational Geosciences, 19(3), 445-478.

Wanner, C., Druhan, J.L., Amos, R.T., Alt-Epping, P., Steefel, C.I., (2015). Benchmarking the simulation of Cr isotope fractionation. Comput. Geosci. https://doi.org/10.1007/s10596-014-9436-9

Mayer, K. U., Amos, R., Molins, S., Gerard, F. (2012). Reactive transport modeling in variably saturated media with MIN3P: Basic model formulation and model enhancements. Vol. 26, pp. 186-211. Sharjah, UAE: Bentham Science Publishers.

Mayer, K. U., MacQuarrie K. T. B. (2010). Solution of the MoMaS reactive transport benchmark with MIN3P - model formulation and simulation results, Computational Geosciences 14:405-419.

Molins, S., Mayer, K. U. (2007). Coupling between geochemical reactions and multicomponent gas and solute transport in unsaturated media: A reactive transport modeling study. Water Resources Research, 43(5).

Amos, R.T., Mayer, K.U., (2006). Investigating ebullition in a sand column using dissolved gas analysis and reactive transport modeling. Environ. Sci. Technol. https://doi.org/10.1021/es0602501

Amos, R.T., Ulrich Mayer, K., (2006). Investigating the role of gas bubble formation and entrapment in contaminated aquifers: Reactive transport modelling. J. Contam. Hydrol. https://doi.org/10.1016/j.jconhyd.2006.04.008

Mayer, K.U., Frind, E.O., Blowes, D.W., (2002). Multicomponent reactive transport modeling in variably-saturated porous media using a generalized formulation for kinetically controlled reactions. Water Resources Research, Vol. 38, No. 9.

Mayer, K.U. (1999). A numerical model for multicomponent reactive transport in variably-saturated porous media, Ph.D. – thesis, Department of Earth Sciences, University of Waterloo, Waterloo, Ontario, Canada.

Other Applications
------------------------------------------------------------------------------------------

Bao, Z., Haberer, C.M., Maier, U., Amos, R. T., Blowes, D. W., Grathwohl, P. (2017). Modeling controls on the chemical weathering of marine mudrocks from the Middle Jurassic in Southern Germany. Chem. Geol. https://doi.org/10.1016/j.chemgeo.2017.03.021

Altenkirch, N., Zlatanovic, S., Woodward, K. B., Trauth, N., Mutz, M., Mokenthin, F. (2016). Untangling Hyporheic Residence time Distributions and Whole Stream Metabolism Using a Hydrological Process Model. https://doi.org/10.1016/j.proeng.2016.07.598

Jamieson-Hanes, J.H., Amos, R.T., Blowes, D.W., (2012). Reactive transport modeling of chromium isotope fractionation during Cr(VI) reduction. Environ. Sci. Technol. https://doi.org/10.1021/es3046235

Gibson, B. D., Amos, R. T., Blowes, D. W. (2011). 34S/32S Fractionation during sulfate reduction in groundwater treatment systems: Reactive transport modeling. Environmental science & technology, 45(7), 2863-2870.

Marica, F., Jofré, S.A.B., Mayer, K.U., Balcom, B.J., Al, T.A., (2011). Determination of spatially-resolved porosity, tracer distributions and diffusion coefficients in porous media using MRI measurements and numerical simulations. J. Contam. Hydrol. https://doi.org/10.1016/j.jconhyd.2011.04.008

Stewart, B.D., Amos, R.T., Fendorf, S., (2011). Effect of uranium(VI) speciation on simultaneous microbial reduction of uranium(VI) and iron(III). J. Environ. Qual. https://doi.org/10.2134/jeq2010.0304

Stewart, B.D., Amos, R.T., Nico, P.S., Fendorf, S., (2011). Influence of uranyl speciation and iron oxides on uranium biogeochemical redox reactions. Geomicrobiol. J. https://doi.org/10.1080/01490451.2010.507646

Henderson, T. (2009). Numerical modeling of density-driven chemical oxidation of chlorinated solvents, Ph.D. – thesis, Department of Earth and Ocean Sciences, the University of British Columbia, Vancouver, British Columbia, Canada.

D'Affonseca, F. M., Park, S., Finkel, M. & P. Blum (2008): Quantification of Natural and Technically Enhanced NAPL Source Depletion: Analytical Models vs. Numerical Models. Groundwater Quality: Securing Groundwater Quality in Urban and Industrial Environments.- IAHS Publ. no. 324, 380-387.

Henderson, T., Mayer, K. U., Parker, B., Al, T. (2009). Three-dimensional density-dependent flow and multicomponent reactive transport modeling of chlorinated solvent oxidation by potassium permanganate. Journal of Contaminant Hydrology, 106, 195-211.

Maier, U., Beyer, C., Susset, B., Grathwohl, P., (2008). Modelling the dilution of solutes due to mass transfer across the capillary fringe. IAHS Publ. 324, 2008, 86–93.

Henderson, T.H., Mayer, K.U., Parker, B.L., Tom, A.A.L., (2005). Numerical simulation of density-driven permanganate oxidation of trichloroethylene DNAPL in a sandy aquifer. IAHS-AISH Publ.

Mayer, K. U., Benner, S. G., Frind, E. O., Thornton, S. F., Lerner, D. N., (2001). Reactive transport modeling of processes controlling the distribution and natural attenuation of phenolic compounds in a deep sandstone aquifer. Journal of Contaminant Hydrology 53 (2001) 341-363.



