#include "cppdefs.h"
      MODULE ice_advect_mod
#ifdef ICE_ADVECT
!
!=================================================== W. Paul Budgell ===
!  Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!================================================== Hernan G. Arango ===
!                                                                      !
!  This subroutine performs advection of ice using one of 2 options:   !
!  (1) Smolarkiewicz second-order upwind scheme.                       !
!      Reference:                                                      !
!      Smolarkiewicz and Grabowski (1990)                              !
!  (2) MPDATA advection scheme.                                        !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: ice_advect

      CONTAINS

# if defined ICE_SMOLAR
#  include "ice_smolar.h"
# endif

#endif
      END MODULE ice_advect_mod
