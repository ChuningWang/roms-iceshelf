# git $Id$
# svn $Id: makefile 968 2019-06-17 16:02:48Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::: Hernan G. Arango :::
# Copyright (c) 2002-2021 The ROMS/TOMS Group             Kate Hedstrom :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                                                                       :::
#  ROMS/TOMS Framework Master Makefile                                  :::
#                                                                       :::
#  This makefile is designed to work only with GNU Make version 3.80 or :::
#  higher. It can be used in any architecture provided that there is a  :::
#  machine/compiler rules file in the  "Compilers"  subdirectory.  You  :::
#  may need to modify the rules file to specify the  correct path  for  :::
#  the NetCDF and ARPACK libraries. The ARPACK library is only used in  :::
#  the Generalized Stability Theory analysis and Laczos algorithm.      :::
#                                                                       :::
#  If appropriate,  the USER needs to modify the  macro definitions in  :::
#  in user-defined section below.  To activate an option set the macro  :::
#  to "on". For example, if you want to compile with debugging options  :::
#  set:                                                                 :::
#                                                                       :::
#      USE_DEBUG := on                                                  :::
#                                                                       :::
#  Otherwise, leave macro definition blank.                             :::
#                                                                       :::
#  The USER needs to provide a value for the  macro FORT.  Choose  the  :::
#  appropriate value from the list below.                               :::
#                                                                       :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

ifneq (3.80,$(firstword $(sort $(MAKE_VERSION) 3.80)))
 $(error This makefile requires GNU make version 3.80 or higher. \
		Your current version is: $(MAKE_VERSION))
endif

#--------------------------------------------------------------------------
#  Initialize some things.
#--------------------------------------------------------------------------

  sources    :=

#==========================================================================
#  Start of user-defined options. In some macro definitions below: "on" or
#  any other string means TRUE while blank (or spaces) is FALSE.
#==========================================================================
#
#  The CPP option defining a particular application is specified below.
#  See header file "ROMS/Include/cppdefs.h" for all available idealized
#  and realistic applications CPP flags. For example, to activate the
#  upwelling test case (UPWELLING) set:
#
#    ROMS_APPLICATION ?= UPWELLING
#
#  Notice that this makefile will include the associated application header
#  file, which is located either in the "ROMS/Include" or MY_HEADER_DIR
#  directory.  This makefile is designed to search in both directories.
#  The only constrain is that the application CPP option must be unique
#  and header file name is the lowercase value of ROMS_APPLICATION with
#  the .h extension. For example, the upwelling application includes the
#  "upwelling.h" header file.

ROMS_APPLICATION ?= UPWELLING

#  If application header files is not located in "ROMS/Include",
#  provide an alternate directory FULL PATH.

MY_HEADER_DIR ?=

#  If your application requires analytical expressions and they are
#  not located in "ROMS/Functionals", provide an alternate directory.
#  Notice that a set analytical expressions templates can be found in
#  "User/Functionals".
#
#  If applicable, also used this directory to place your customized
#  biology model header file (like fennel.h, nemuro.h, ecosim.h, etc).

MY_ANALYTICAL_DIR ?=

#  Sometimes it is desirable to activate one or more CPP options to
#  run different variants of the same application without modifying
#  its header file. If this is the case, specify such options here
#  using the -D syntax.  For example, to write time-averaged fields
#  set:
#
#    MY_CPP_FLAGS ?= -DAVERAGES
#

MY_CPP_FLAGS ?=

#  Activate debugging compiler options:

   USE_DEBUG ?=

#  If parallel applications, use at most one of these definitions
#  (leave both definitions blank in serial applications):

     USE_MPI ?=
  USE_OpenMP ?=

#  If distributed-memory, turn on compilation via the script "mpif90".
#  This is needed in some Linux operating systems. In some systems with
#  native MPI libraries the compilation does not require MPICH type
#  scripts. This macro is also convient when there are several fortran
#  compiliers (ifort, pgf90, pathf90) in the system that use mpif90.
#  In this, case the user need to select the desired compiler below and
#  turn on both USE_MPI and USE_MPIF90 macros.

  USE_MPIF90 ?=

#  If applicable, activate 64-bit compilation:

   USE_LARGE ?= on

#  If applicable, link with NetCDF-4 library. Notice that the NetCDF-4
#  library needs both the HDF5 and MPI libraries.

 USE_NETCDF4 ?=

#--------------------------------------------------------------------------
#  We are going to include a file with all the settings that depend on
#  the system and the compiler. We are going to build up the name of the
#  include file using information on both. Set your compiler here from
#  the following list:
#
#  Operating System        Compiler(s)
#
#     AIX:                    xlf
#     ALPHA:                  f90
#     CYGWIN:                 g95, df, ifort
#     Darwin:                 f90, xlf
#     IRIX:                   f90
#     Linux:                  ftn, ifc, ifort, pgi, path, g95, gfortran
#     SunOS:                  f95
#     UNICOS-mp:              ftn
#     SunOS/Linux:            ftn (Cray cross-compiler)
#
#  Feel free to send us additional rule files to include! Also, be sure
#  to check the appropriate file to make sure it has the right paths to
#  NetCDF and so on.
#--------------------------------------------------------------------------

        FORT ?= pgi

#--------------------------------------------------------------------------
#  Set directory for executable.
#--------------------------------------------------------------------------

      BINDIR ?= .

#==========================================================================
#  End of user-defined options. See also the machine-dependent include
#  file being used above.
#==========================================================================

#--------------------------------------------------------------------------
#  Set directory for temporary objects.
#--------------------------------------------------------------------------

SCRATCH_DIR ?= Build_roms
 clean_list := core *.ipo $(SCRATCH_DIR)

ifeq "$(strip $(SCRATCH_DIR))" "."
  clean_list := core *.o *.oo *.mod *.f90 lib*.a *.bak
  clean_list += $(CURDIR)/*.ipo
endif
ifeq "$(strip $(SCRATCH_DIR))" "./"
  clean_list := core *.o *.oo *.ipo *.mod *.f90 lib*.a *.bak
  clean_list += $(CURDIR)/*.ipo
endif

#--------------------------------------------------------------------------
#  Notice that the token "libraries" is initialized with the ROMS/Utility
#  library to account for calls to objects in other ROMS libraries or
#  cycling dependencies. These types of dependencies are problematic in
#  some compilers during linking. Such libraries appear twice at linking
#  step (beginning and almost the end of ROMS library list).
#--------------------------------------------------------------------------

   libraries := $(SCRATCH_DIR)/libNLM.a $(SCRATCH_DIR)/libDRIVER.a \
		$(SCRATCH_DIR)/libUTIL.a

#--------------------------------------------------------------------------
#  Set Pattern rules.
#--------------------------------------------------------------------------

%.o: %.F

%.o: %.f90
	cd $(SCRATCH_DIR); $(FC) -c $(FFLAGS) $(notdir $<)

%.f90: %.F
	$(CPP) $(CPPFLAGS) $(MY_CPP_FLAGS) $< > $*.f90
	$(CLEAN) $*.f90

CLEAN := ROMS/Bin/cpp_clean

#--------------------------------------------------------------------------
#  Set C-preprocessing flags associated with ROMS application. They are
#  used in "ROMS/Include/cppdefs.h" to include the appropriate application
#  header file.
#--------------------------------------------------------------------------

ifdef ROMS_APPLICATION
        HEADER := $(addsuffix .h, \
			$(shell echo ${ROMS_APPLICATION} | tr [A-Z] [a-z]))
 ROMS_CPPFLAGS := -D$(ROMS_APPLICATION)
 ROMS_CPPFLAGS += -D'HEADER="$(HEADER)"'
 ifdef MY_HEADER_DIR
  ROMS_CPPFLAGS += -D'ROMS_HEADER="$(MY_HEADER_DIR)/$(HEADER)"'
 else
  ROMS_CPPFLAGS += -D'ROMS_HEADER="$(HEADER)"'
 endif
 ifdef MY_CPP_FLAGS
  ROMS_CPPFLAGS += $(MY_CPP_FLAGS)
 endif
endif

#--------------------------------------------------------------------------
#  Internal macro definitions used to select the code to compile and
#  additional libraries to link. It uses the CPP activated in the
#  header file ROMS/Include/cppdefs.h to determine macro definitions.
#--------------------------------------------------------------------------

  COMPILERS ?= $(CURDIR)/Compilers

MAKE_MACROS := $(shell echo ${HOME} | sed 's| |\\ |g')/make_macros.mk

ifneq ($(MAKECMDGOALS),clean)
  MACROS := $(shell cpp -P $(ROMS_CPPFLAGS) Compilers/make_macros.h > \
              $(MAKE_MACROS); $(CLEAN) $(MAKE_MACROS))

  GET_MACROS := $(wildcard $(SCRATCH_DIR)/make_macros.*)

  ifdef GET_MACROS
    include $(SCRATCH_DIR)/make_macros.mk
  else
    include $(MAKE_MACROS)
  endif
endif

clean_list += $(MAKE_MACROS)

#--------------------------------------------------------------------------
#  Make functions for putting the temporary files in $(SCRATCH_DIR)
#  DO NOT modify this section; spaces and blank lines are needed.
#--------------------------------------------------------------------------

# $(call source-dir-to-binary-dir, directory-list)
source-dir-to-binary-dir = $(addprefix $(SCRATCH_DIR)/, $(notdir $1))

# $(call source-to-object, source-file-list)
source-to-object = $(call source-dir-to-binary-dir,   \
                   $(subst .F,.o,$1))

# $(call make-library, library-name, source-file-list)
define make-library
   libraries += $(SCRATCH_DIR)/$1
   sources   += $2

   $(SCRATCH_DIR)/$1: $(call source-dir-to-binary-dir,    \
                      $(subst .F,.o,$2))
	$(AR) $(ARFLAGS) $$@ $$^
	$(RANLIB) $$@
endef

# $(call f90-source, source-file-list)
f90-source = $(call source-dir-to-binary-dir,     \
                   $(subst .F,.f90,$1))

# $(compile-rules)
define compile-rules
  $(foreach f, $(local_src),       \
    $(call one-compile-rule,$(call source-to-object,$f), \
    $(call f90-source,$f),$f))
endef

# $(call one-compile-rule, binary-file, f90-file, source-files)
define one-compile-rule
  $1: $2 $3
	cd $$(SCRATCH_DIR); $$(FC) -c $$(FFLAGS) $(notdir $2)

  $2: $3
	$$(CPP) $$(CPPFLAGS) $$(MY_CPP_FLAGS) $$< > $$@
	$$(CLEAN) $$@

endef

#--------------------------------------------------------------------------
#  Set ROMS/TOMS executable file name.
#--------------------------------------------------------------------------

ifdef USE_DEBUG
  BIN ?= $(BINDIR)/romsG
else
 ifdef USE_MPI
   BIN ?= $(BINDIR)/romsM
 else
  ifdef USE_OpenMP
    BIN ?= $(BINDIR)/romsO
  else
    BIN ?= $(BINDIR)/romsS
  endif
 endif
endif

#--------------------------------------------------------------------------
#  Set name of module files for netCDF F90 interface. On some platforms
#  these will need to be overridden in the machine-dependent include file.
#--------------------------------------------------------------------------

   NETCDF_MODFILE := netcdf.mod
TYPESIZES_MODFILE := typesizes.mod

#--------------------------------------------------------------------------
#  "uname -s" should return the OS or kernel name and "uname -m" should
#  return the CPU or hardware name. In practice the results can be pretty
#  flaky. Run the results through sed to convert "/" and " " to "-",
#  then apply platform-specific conversions.
#--------------------------------------------------------------------------

OS := $(shell uname -s | sed 's/[\/ ]/-/g')
OS := $(patsubst CYGWIN_%,CYGWIN,$(OS))
OS := $(patsubst MINGW%,MINGW,$(OS))
OS := $(patsubst sn%,UNICOS-sn,$(OS))

CPU := $(shell uname -m | sed 's/[\/ ]/-/g')

GITURL := $(shell git config remote.origin.url)
GITREV := $(shell git log -n 1 --format=%H)

SVNURL := $(shell grep HeadURL ./ROMS/Version | \
            sed 's/.* \(https.*\)\/ROMS\/Version.*/\1/')
SVNREV := $(shell grep Revision ./ROMS/Version | \
            sed 's/.* \([0-9]*\) .*/\1/')

ROOTDIR := $(shell pwd)

ifndef FORT
  $(error Variable FORT not set)
endif

ifneq ($(MAKECMDGOALS),clean)
  MKFILE := $(COMPILERS)/$(OS)-$(strip $(FORT)).mk
  include $(MKFILE)
endif

ifdef USE_MPI
 ifdef USE_OpenMP
  $(error You cannot activate USE_MPI and USE_OpenMP at the same time!)
 endif
endif

#--------------------------------------------------------------------------
#  Pass the platform variables to the preprocessor as macros. Convert to
#  valid, upper-case identifiers. Attach ROMS application  CPP options.
#--------------------------------------------------------------------------

CPPFLAGS += -D$(shell echo ${OS} | tr "-" "_" | tr [a-z] [A-Z])
CPPFLAGS += -D$(shell echo ${CPU} | tr "-" "_" | tr [a-z] [A-Z])
CPPFLAGS += -D$(shell echo ${FORT} | tr "-" "_" | tr [a-z] [A-Z])

CPPFLAGS += -D'ROOT_DIR="$(ROOTDIR)"'
ifdef ROMS_APPLICATION
  CPPFLAGS  += $(ROMS_CPPFLAGS)
  CPPFLAGS  += -DNestedGrids=$(NestedGrids)
  MDEPFLAGS += -DROMS_HEADER="$(HEADER)"
endif

ifndef MY_ANALYTICAL_DIR
  MY_ANALYTICAL_DIR := $(ROOTDIR)/ROMS/Functionals
endif
ifeq (,$(findstring ROMS/Functionals,$(MY_ANALYTICAL_DIR)))
  MY_ANALYTICAL := on
endif
CPPFLAGS += -D'ANALYTICAL_DIR="$(MY_ANALYTICAL_DIR)"'

ifdef MY_ANALYTICAL
  CPPFLAGS += -D'MY_ANALYTICAL="$(MY_ANALYTICAL)"'
endif

CPPFLAGS += -D'GIT_URL="$(GITURL)"'
CPPFLAGS += -D'GIT_REV="$(GITREV)"'
CPPFLAGS += -D'SVN_URL="$(SVNURL)"'
CPPFLAGS += -D'SVN_REV="$(SVNREV)"'

#--------------------------------------------------------------------------
#  Build target directories.
#--------------------------------------------------------------------------

.PHONY: all

all: $(SCRATCH_DIR) $(SCRATCH_DIR)/MakeDepend $(BIN) rm_macros

 modules  :=
ifdef USE_ADJOINT
 modules  +=	ROMS/Adjoint \
		ROMS/Adjoint/Biology
endif
ifdef USE_REPRESENTER
 modules  +=	ROMS/Representer \
		ROMS/Representer/Biology
endif
ifdef USE_TANGENT
 modules  +=	ROMS/Tangent \
		ROMS/Tangent/Biology
endif
 modules  +=	ROMS/Nonlinear \
		ROMS/Nonlinear/Biology \
		ROMS/Nonlinear/Sediment \
		ROMS/Functionals \
		ROMS/Utility \
		ROMS/Drivers \
		ROMS/Modules

 modules  +=	ROMS/IceShelf
ifdef USE_SEAICE
 modules  +=	ROMS/SeaIce
endif

 includes :=	ROMS/Include
ifdef MY_ANALYTICAL
 includes +=	$(MY_ANALYTICAL_DIR)
endif
ifdef USE_ADJOINT
 includes +=	ROMS/Adjoint \
		ROMS/Adjoint/Biology
endif
ifdef USE_REPRESENTER
 includes +=	ROMS/Representer \
		ROMS/Representer/Biology
endif
ifdef USE_TANGENT
 includes +=	ROMS/Tangent \
		ROMS/Tangent/Biology
endif
 includes +=	ROMS/Nonlinear \
		ROMS/Nonlinear/Biology \
		ROMS/Nonlinear/Sediment \
		ROMS/Utility \
		ROMS/Drivers \
		ROMS/Functionals

 includes +=	ROMS/IceShelf
ifdef USE_SEAICE
 includes +=	ROMS/SeaIce
endif

ifdef MY_HEADER_DIR
 includes +=	$(MY_HEADER_DIR)
endif

ifdef USE_PIO
 includes +=	$(PIO_INCDIR)
endif

ifdef USE_COAMPS
 includes +=	$(COAMPS_LIB_DIR)
endif

ifdef USE_SWAN
 modules  +=	Waves/SWAN/Src
 includes +=	Waves/SWAN/Src
endif

ifdef USE_WRF
 ifeq "$(strip $(WRF_LIB_DIR))" "$(WRF_SRC_DIR)"
  includes +=	$(addprefix $(WRF_LIB_DIR)/,$(WRF_MOD_DIRS))
 else
  includes +=	$(WRF_LIB_DIR)
 endif
endif

 modules  +=	Master
 includes +=	Master Compilers

vpath %.F $(modules)
vpath %.h $(includes)
vpath %.f90 $(SCRATCH_DIR)
vpath %.o $(SCRATCH_DIR)

include $(addsuffix /Module.mk,$(modules))

MDEPFLAGS += $(patsubst %,-I %,$(includes)) --silent --moddir $(SCRATCH_DIR)

CPPFLAGS  += $(patsubst %,-I%,$(includes))

ifdef MY_HEADER_DIR
  CPPFLAGS += -D'HEADER_DIR="$(MY_HEADER_DIR)"'
else
  CPPFLAGS += -D'HEADER_DIR="$(ROOTDIR)/ROMS/Include"'
endif

$(SCRATCH_DIR):
	$(shell $(TEST) -d $(SCRATCH_DIR) || $(MKDIR) $(SCRATCH_DIR) )

#--------------------------------------------------------------------------
#  Special CPP macros for mod_strings.F
#--------------------------------------------------------------------------

$(SCRATCH_DIR)/mod_strings.f90: CPPFLAGS += -DMY_OS='"$(OS)"' \
              -DMY_CPU='"$(CPU)"' -DMY_FORT='"$(FORT)"' \
              -DMY_FC='"$(FC)"' -DMY_FFLAGS='"$(FFLAGS)"'

#--------------------------------------------------------------------------
#  ROMS/TOMS libraries.
#--------------------------------------------------------------------------

MYLIB := libroms.a

.PHONY: libraries

libraries: $(libraries)

#--------------------------------------------------------------------------
#  Target to create ROMS/TOMS dependecies.
#--------------------------------------------------------------------------

$(SCRATCH_DIR)/$(NETCDF_MODFILE): | $(SCRATCH_DIR)
	cp -f $(NETCDF_INCDIR)/$(NETCDF_MODFILE) $(SCRATCH_DIR)

$(SCRATCH_DIR)/$(TYPESIZES_MODFILE): | $(SCRATCH_DIR)
	cp -f $(NETCDF_INCDIR)/$(TYPESIZES_MODFILE) $(SCRATCH_DIR)

$(SCRATCH_DIR)/MakeDepend: makefile \
                           $(SCRATCH_DIR)/$(NETCDF_MODFILE) \
                           $(SCRATCH_DIR)/$(TYPESIZES_MODFILE) \
                           | $(SCRATCH_DIR)
	@ $(SFMAKEDEPEND) $(MDEPFLAGS) $(sources) > $(SCRATCH_DIR)/MakeDepend
	cp -p $(MAKE_MACROS) $(SCRATCH_DIR)

.PHONY: depend

SFMAKEDEPEND := ./ROMS/Bin/sfmakedepend

depend: $(SCRATCH_DIR)
	$(SFMAKEDEPEND) $(MDEPFLAGS) $(sources) > $(SCRATCH_DIR)/MakeDepend

ifneq ($(MAKECMDGOALS),clean)
  -include $(SCRATCH_DIR)/MakeDepend
endif

#--------------------------------------------------------------------------
#  Target to create ROMS/TOMS tar file.
#--------------------------------------------------------------------------

.PHONY: tarfile

tarfile:
		tar --exclude=".svn" -cvf roms-3_7.tar *

.PHONY: zipfile

zipfile:
		zip -r roms-3_7.zip *

.PHONY: gzipfile

gzipfile:
		gzip -v roms-3_7.gzip *

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

.PHONY: clean

clean:
	$(RM) -r $(clean_list)

.PHONY: rm_macros

rm_macros:
	$(RM) -r $(MAKE_MACROS)

#--------------------------------------------------------------------------
#  A handy debugging target. This will allow to print the value of any
#  makefile defined macro (see http://tinyurl.com/8ax3j). For example,
#  to find the value of CPPFLAGS execute:
#
#        gmake print-CPPFLAGS
#  or
#        make print-CPPFLAGS
#--------------------------------------------------------------------------

.PHONY: print-%

print-%:
	@echo $* = $($*)
# DO NOT DELETE THIS LINE - used by make depend
coupler.o: mct_coupler.h mct_roms_swan.h tile.h mct_roms_wrf.h cppdefs.h
coupler.o: globaldefs.h esmf_coupler.h
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/distribute.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_coupler.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_esmf_esm.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_forces.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_grid.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_iounits.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_kinds.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_ocean.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_parallel.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_param.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_scalars.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_sedbed.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_sediment.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_stepping.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/roms_export.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/roms_import.o
coupler.o: /home/chuning/projects/iceshelf_test/Build_roms/strings.o

esmf_atm.o: esmf_atm_void.h esmf_atm_wrf.h esmf_atm_regcm.h esmf_atm_coamps.h
esmf_atm.o: cppdefs.h globaldefs.h
esmf_atm.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_esmf_esm.o
esmf_atm.o: /home/chuning/projects/iceshelf_test/Build_roms/strings.o

esmf_data.o: cppdefs.h globaldefs.h
esmf_data.o: /home/chuning/projects/iceshelf_test/Build_roms/dateclock.o
esmf_data.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_esmf_esm.o
esmf_data.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_iounits.o
esmf_data.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_netcdf.o
esmf_data.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_parallel.o
esmf_data.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_param.o
esmf_data.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_scalars.o
esmf_data.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_strings.o
esmf_data.o: /home/chuning/projects/iceshelf_test/Build_roms/strings.o

esmf_esm.o: cppdefs.h globaldefs.h
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/coupler.o
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/esmf_atm.o
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/esmf_atm.o
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/esmf_atm.o
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/esmf_atm.o
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/esmf_data.o
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/esmf_ice.o
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/esmf_roms.o
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/esmf_wav.o
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/esmf_wav.o
esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_esmf_esm.o

esmf_ice.o: esmf_ice_void.h cppdefs.h globaldefs.h esmf_ice_cice.h
esmf_ice.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_esmf_esm.o

esmf_roms.o: cppdefs.h globaldefs.h
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/dateclock.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/exchange_2d.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_esmf_esm.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_forces.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_grid.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_iounits.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_mixing.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_ncparam.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_netcdf.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_ocean.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_param.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_scalars.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_stepping.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/mp_exchange.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/roms_kernel.o
esmf_roms.o: /home/chuning/projects/iceshelf_test/Build_roms/strings.o

esmf_wav.o: esmf_wav_wam.h esmf_wav_void.h cppdefs.h globaldefs.h
esmf_wav.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_esmf_esm.o

master.o: cppdefs.h globaldefs.h roms.h esmf_driver.h mct_driver.h
master.o: /home/chuning/projects/iceshelf_test/Build_roms/coupler.o
master.o: /home/chuning/projects/iceshelf_test/Build_roms/esmf_esm.o
master.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_coupler.o
master.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_esmf_esm.o
master.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_iounits.o
master.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_parallel.o
master.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_param.o
master.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_scalars.o
master.o: /home/chuning/projects/iceshelf_test/Build_roms/roms_kernel.o
master.o: /home/chuning/projects/iceshelf_test/Build_roms/set_pio.o

mod_esmf_esm.o: cppdefs.h globaldefs.h
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/dateclock.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/def_dim.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/def_var.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/distribute.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/inp_decode.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_iounits.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_kinds.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_netcdf.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_parallel.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_param.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_scalars.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_strings.o
mod_esmf_esm.o: /home/chuning/projects/iceshelf_test/Build_roms/strings.o

propagator.o: propagator_op.h propagator_hop.h cppdefs.h globaldefs.h
propagator.o: propagator_so.h propagator_hso.h propagator_fte.h
propagator.o: propagator_so_semi.h propagator_afte.h propagator_fsv.h
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/close_io.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/dotproduct.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/ini_adjust.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/inner2state.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_coupling.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_forces.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_iounits.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_kinds.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_netcdf.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_ocean.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_parallel.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_param.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_scalars.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_stepping.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_storage.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/packing.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/set_depth.o
propagator.o: /home/chuning/projects/iceshelf_test/Build_roms/strings.o

roms_export.o: set_bounds.h cppdefs.h globaldefs.h
roms_export.o: /home/chuning/projects/iceshelf_test/Build_roms/distribute.o
roms_export.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_kinds.o
roms_export.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_ncparam.o
roms_export.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_param.o

roms_import.o: cppdefs.h globaldefs.h
roms_import.o: /home/chuning/projects/iceshelf_test/Build_roms/distribute.o
roms_import.o: /home/chuning/projects/iceshelf_test/Build_roms/exchange_2d.o
roms_import.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_kinds.o
roms_import.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_ncparam.o
roms_import.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_param.o
roms_import.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_scalars.o
roms_import.o: /home/chuning/projects/iceshelf_test/Build_roms/mp_exchange.o

roms_kernel.o: fte_roms.h op_roms.h array_modes.h optobs_roms.h
roms_kernel.o: tl_r4dvar_roms.h r4dvar_roms.h obs_sen_r4dvar_analysis.h
roms_kernel.o: ad_roms.h correlation.h picard_roms.h so_roms.h tl_roms.h
roms_kernel.o: nl_roms.h adsen_roms.h so_semi_roms.h tl_rbl4dvar_roms.h
roms_kernel.o: obs_sen_i4dvar_analysis.h pert_roms.h symmetry.h
roms_kernel.o: hessian_so_roms.h rbl4dvar_roms.h split_r4dvar_roms.h
roms_kernel.o: obs_sen_rbl4dvar_forecast.h hessian_op_roms.h afte_roms.h
roms_kernel.o: tlcheck_roms.h cppdefs.h globaldefs.h fsv_roms.h
roms_kernel.o: split_rbl4dvar_roms.h i4dvar_roms.h rp_roms.h
roms_kernel.o: split_i4dvar_roms.h obs_sen_rbl4dvar_analysis.h
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/analytical.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/array_modes.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/close_io.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/congrad.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/convolve.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/coupler.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/def_dai.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/def_gst.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/def_impulse.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/def_mod.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/def_norm.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/distribute.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/dotproduct.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/get_gst.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/get_state.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/i4dvar.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/ini_adjust.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/inp_par.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_boundary.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_forces.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_fourdvar.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_iounits.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_ncparam.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_netcdf.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_ocean.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_parallel.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_param.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_pio_netcdf.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_scalars.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_stepping.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/mod_storage.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/normalization.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/packing.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/propagator.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/r4dvar.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/rbl4dvar.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/rpcg_lanczos.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/stats_modobs.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/stdinp_mod.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/strings.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/wrt_dai.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/wrt_gst.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/wrt_impulse.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/wrt_ini.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/wrt_rst.o
roms_kernel.o: /home/chuning/projects/iceshelf_test/Build_roms/zeta_balance.o

i4dvar.o: cppdefs.h globaldefs.h
i4dvar.o: /home/chuning/projects/iceshelf_test/Build_roms/back_cost.o
i4dvar.o: /home/chuning/projects/iceshelf_test/Build_roms/cgradient.o
i4dvar.o: /home/chuning/projects/iceshelf_test/Build_roms/close_io.o
i4dvar.o: /home/chuning/projects/iceshelf_test/Build_roms/cost_grad.o
i4dvar.o: /home/chuning/projects/iceshelf_test/Build_roms/def_hessian.o
i4dvar.o: /home/chuning/projec
