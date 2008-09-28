#include "cppdefs.h"
      FUNCTION def_dim (ng, model, ncid, ncname, DimName, DimSize,      &
     &                  DimID)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2008 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine defines the requested NetCDF dimension.                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng       Nested grid number (integer)                            !
!     model    Calling model identifier (integer)                      !
!     ncid     NetCDF file ID (integer)                                !
!     ncname   NetCDF file name (string)                               !
!     DimName  Dimension name (string)                                 !
!     DimSize  Dimension size (integer)                                !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     def_dim  Error flag (integer)                                    !
!     DimId    NetCDF dimension ID (integer)                           !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_netcdf
      USE mod_scalars

# ifdef DISTRIBUTE
!
      USE distribute_mod, ONLY : mp_bcasti
# endif
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent (in) :: ng, model, ncid
      integer, intent (in) :: DimSize

      integer, intent (out) :: DimID

      character (len=*), intent(in) :: ncname
      character (len=*), intent(in) :: DimName
!
!  Local variable declarations.
!
      integer :: status

      integer :: def_dim

#ifdef DISTRIBUTE
      integer, dimension(3) :: buffer
#endif
!
!-----------------------------------------------------------------------
!  Define requested NetCDF dimension.
!-----------------------------------------------------------------------
!
      IF (OutThread) THEN
        status=nf90_def_dim(ncid, TRIM(DimName), DimSize, DimId)
        IF (status.ne.nf90_noerr) THEN
          IF (Master) WRITE (stdout,10) TRIM(DimName), TRIM(ncname)
          exit_flag=3
          ioerror=status
        END IF
      END IF

#ifdef DISTRIBUTE
!
!  Broadcast information to all threads in the group.
!
      buffer(1)=DimID
      buffer(2)=status
      buffer(3)=exit_flag
      CALL mp_bcasti (ng, model, buffer, 3)
      DimID=buffer(1)
      status=buffer(2)
      exit_flag=buffer(3)
#endif
!
!  Set error flag.
!
      def_dim=status
!
 10   FORMAT (/,' DEF_VAR - error while defining dimension: ',a,        &
     &        /,11x,'in file: ',a)

      RETURN
      END FUNCTION def_dim