#include "cppdefs.h"
      MODULE wrt_avg_mod
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
!
!git $Id$
!svn $Id: wrt_avg.F 1064 2021-05-10 19:55:56Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This module writes model time-averaged fields into output file      !
!  using the standard NetCDF library or the Parallel-IO (PIO) library. !
!                                                                      !
!  Notice that only momentum is affected by the full time-averaged     !
!  masks.  If applicable, these mask contains information about        !
!  river runoff and time-dependent wetting and drying variations.      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_average
      USE mod_forces
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
#ifdef ICE_MODEL
      USE mod_ice
#endif
# if defined SEDIMENT || defined BBL_MODEL
      USE mod_sedbed
      USE mod_sediment
# endif
# if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
      USE mod_tides
# endif
!
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
# ifdef SOLVE3D
      USE nf_fwrite3d_mod, ONLY : nf_fwrite3d
# endif
      USE strings_mod,     ONLY : FoundError
!
      implicit none
!
      PUBLIC  :: wrt_avg
      PRIVATE :: wrt_avg_nf90
# if defined PIO_LIB && defined DISTRIBUTE
      PRIVATE :: wrt_avg_pio
# endif
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE wrt_avg (ng, tile)
!***********************************************************************
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
!-----------------------------------------------------------------------
!  Write out time-averaged fields according to IO type.
!-----------------------------------------------------------------------
!
      LBi=BOUNDS(ng)%LBi(tile)
      UBi=BOUNDS(ng)%UBi(tile)
      LBj=BOUNDS(ng)%LBj(tile)
      UBj=BOUNDS(ng)%UBj(tile)
!
      SELECT CASE (AVG(ng)%IOtype)
        CASE (io_nf90)
          CALL wrt_avg_nf90 (ng, tile,                                  &
     &                       LBi, UBi, LBj, UBj)

# if defined PIO_LIB && defined DISTRIBUTE
        CASE (io_pio)
          CALL wrt_avg_pio (ng, tile,                                   &
     &                      LBi, UBi, LBj, UBj)
# endif
        CASE DEFAULT
          IF (Master) THEN
            WRITE (stdout,10) AVG(ng)%IOtype
  10        FORMAT (' WRT_AVG - Illegal output type, io_type = ',i0)
          END IF
          exit_flag=3
      END SELECT
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
      RETURN
      END SUBROUTINE wrt_avg
!
!***********************************************************************
      SUBROUTINE wrt_avg_nf90 (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj)
!***********************************************************************
!
      USE mod_netcdf
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
!
!  Local variable declarations.
!
      integer :: Fcount, gfactor, gtype, i, itrc, status
!
      real(dp) :: scale
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__//", wrt_avg_nf90"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Write out time-averaged fields when appropriate.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Set grid type factor to write full (gfactor=1) fields or water
!  points (gfactor=-1) fields only.
!
# if defined WRITE_WATER && defined MASKING
      gfactor=-1
# else
      gfactor=1
# endif
!
!  Set time record index.
!
      AVG(ng)%Rindex=AVG(ng)%Rindex+1
      Fcount=AVG(ng)%load
      AVG(ng)%Nrec(Fcount)=AVG(ng)%Nrec(Fcount)+1
!
!  Write out averaged time.
!
      CALL netcdf_put_fvar (ng, iNLM, AVG(ng)%name,                     &
     &                      TRIM(Vname(1,idtime)), AVGtime(ng:),        &
     &                      (/AVG(ng)%Rindex/), (/1/),                  &
     &                      ncid = AVG(ng)%ncid,                        &
     &                      varid = AVG(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Write out free-surface (m).
!
      IF (Aout(idFsur,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idFsur), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     AVERAGE(ng) % avgzeta)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idFsur)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

# if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided free-surface (m).
!
      IF (Aout(idFsuD,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idFsuD), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     TIDES(ng) % zeta_detided)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idFsuD)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif

# ifdef ICE_MODEL
!
!  Write out ice 2D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idUice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idUice), &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % umask_avg,                          &
#  endif
     &                   AVERAGE(ng) % avguice)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice 2D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idVice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idVice), &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % vmask_avg,                          &
#  endif
     &                   AVERAGE(ng) % avgvice)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out 2D Eastward ice momentum component (m/s) at RHO-points.
!
      IF (Aout(idUiceE,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idUiceE),                        &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avguiceE)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUiceE)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out 2D Northward ice momentum component (m/s) at RHO-points.
!
      IF (Aout(idViceN,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idViceN),                        &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgviceN)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idViceN)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice concentration.
!
      IF (Aout(idAice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idAice), &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgaice)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice thickness.
!
      IF (Aout(idHice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idHice), &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avghice)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice/snow surface temperature.
!
      IF (Aout(idTice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idTice), &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgtice)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice internal temperature.
!
      IF (Aout(idTimid,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idTimid),&
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgtimid)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTimid)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice top temperature.
!
      IF (Aout(idT2ice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idT2ice),&
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgt2ice)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idT2ice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out snow thickness.
!
      IF (Aout(idHsno,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idHsno), &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avghsno)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHsno)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice age.
!
      IF (Aout(idAgeice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                   AVG(ng)%Vid(idAgeice),                         &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgageice)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAgeice)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice-ocean mass flux
!
      IF (Aout(idIomflx,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                   AVG(ng)%Vid(idIomflx),                         &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgiomflx)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIomflx)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 11
!
      IF (Aout(idSig11,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idSig11),&
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgsig11)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig11)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 12
!
      IF (Aout(idSig12,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idSig12),&
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgsig12)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig12)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out internal ice stress component 22
!
      IF (Aout(idSig22,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idSig22),&
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgsig22)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig22)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out temperature of molecular sublayer under ice.
!
      IF (Aout(idT0mk,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idT0mk), &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgT0mk)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idT0mk)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out salinity of molecular sublayer under ice.
!
      IF (Aout(idS0mk,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idS0mk), &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgS0mk)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idS0mk)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out frazil ice growth rate.
!
      IF (Aout(idWfr,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idWfr),  &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgWfr)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWfr)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate.
!
      IF (Aout(idWai,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idWai),  &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgWai)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWai)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate.
!
      IF (Aout(idWao,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idWao),  &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgWao)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWao)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice growth/melt rate.
!
      IF (Aout(idWio,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idWio),  &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgWio)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWio)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice melt runoff rate.
!
      IF (Aout(idWro,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idWro),  &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgWro)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWro)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice divergence rate.
!
      IF (Aout(idWdiv,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idWdiv), &
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgWdiv)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWdiv)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice-water friction velocity
!
      IF (Aout(idTauiw,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idTauiw),&
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgutau_iw)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTauiw)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice-water momentum transfer coefficient
!
      IF (Aout(idChuiw,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idChuiw),&
     &                   AVG(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#  endif
     &                   AVERAGE(ng) % avgchu_iw)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idChuiw)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
#  ifdef ICE_DIAGS
!
!  Write out fraction of shortwave penetrating into the ice
!
      IF (Aout(idfrswp,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idfrswp),                        &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgfrswp)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idfrswp)),                   &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out reduction factor for near-IR fraction of SW under ice
!
      IF (Aout(idrfaci,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idrfaci),                        &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgrfaci)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idrfaci)),                   &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ocean albedo
!
      IF (Aout(idAlbe,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idAlbe),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgAlbe)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAlbe)),                    &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice albedo
!
      IF (Aout(idAlbI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idAlbI),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgAlbI)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAlbI)),                    &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out downward longwave radiation
!
      IF (Aout(idLWdn,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idLWdn),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgLWdn)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLWdn)),                    &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out downward shortwave
!
      IF (Aout(idSWdn,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idSWdn),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSWdn)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSWdn)),                    &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out latent heat over ice
!
      IF (Aout(idLatI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idLatI),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgLatI)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLatI)),                    &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out sensible heat over ice
!
      IF (Aout(idSenI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idSenI),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSenI)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSenI)),                    &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward longwave over ice
!
      IF (Aout(idLWupI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idLWupI),                        &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgLWupI)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLWupI)),                   &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward shortwave over ice
!
      IF (Aout(idSWupI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idSWupI),                        &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSWupI)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSWupI)),                   &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward shortwave over ice
!
      IF (Aout(idSWinI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idSWinI),                        &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSWinI)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSWinI)),                   &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out shortwave through ice
!
      IF (Aout(idSWthruI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idSWthruI),                      &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSWthruI)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSWthruI)),                 &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salt flux from ice
!
      IF (Aout(idSaltI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idSaltI),                        &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSaltI)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSaltI)),                   &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salt flux in leads
!
      IF (Aout(idSaltL,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idSaltL),                        &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSaltL)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSaltL)),                   &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out air-ocean heat flux
!
      IF (Aout(idQao,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idQao),                          &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgQao)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idQao)),                     &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-air heat flux
!
      IF (Aout(idQai,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idQai),                          &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgQai)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idQai)),                     &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-ocean heat flux
!
      IF (Aout(idQio,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idQio),                          &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgQio)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idQio)),                     &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice heat flux
!
      IF (Aout(idQi2,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idQi2),                          &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgQi2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idQi2)),                     &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out snow-ice conversion
!
      IF (Aout(idSnoI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idSnoI),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSnoI)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSnoI)),                    &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
# endif

# ifdef ICESHELF
!
!  Write out ice shelf melt rate (m/s).
!
      IF (Aout(idismr,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idismr), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgismr)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idismr)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  ifdef SURFACE_OVERFLUX_FIX
      IF (Aout(idssflxSOF,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idssflxSOF),                     &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgssflxSOF)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idssflxSOF)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
      IF (Aout(idshflxSOF,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idshflxSOF),                     &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgshflxSOF)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idshflxSOF)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
# endif
!
!  Write out 2D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idUbar,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idUbar), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
# endif
     &                     AVERAGE(ng) % avgu2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbar)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

# if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 2D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idu2dD,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idu2dD), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#  endif
     &                     TIDES(ng) % ubar_detided)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu2dD)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
!
!  Write out 2D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idVbar,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idVbar), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask_avg,                        &
# endif
     &                     AVERAGE(ng) % avgv2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbar)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

# if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 2D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idv2dD,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idv2dD), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask_avg,                        &
#  endif
     &                     TIDES(ng) % vbar_detided)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv2dD)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
!
!  Write out 2D Eastward momentum component (m/s) at RHO-points.
!
      IF (Aout(idu2dE,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idu2dE), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_avg,                        &
# endif
     &                     AVERAGE(ng) % avgu2dE)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu2dE)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Northward momentum component (m/s) at RHO-points.
!
      IF (Aout(idv2dN,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idv2dN), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_avg,                        &
# endif
     &                     AVERAGE(ng) % avgv2dN)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv2dN)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

# if defined FORWARD_WRITE && defined SOLVE3D
!
!  Write out time-averaged mass fluxes for 3D momentum coupling.
!
      IF (Aout(idUfx1,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idUfx1), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgDU_avg1)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUfx1)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
      IF (Aout(idUfx2,ng)) THEN
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idUfx2), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgDU_avg2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUfx2)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
      IF (Aout(idVfx1,ng)) THEN
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idVfx1), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgDV_avg1)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVfx1)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
      IF (Aout(idVfx2,ng)) THEN
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idVfx2), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgDV_avg2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVfx2)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# ifdef NEARSHORE_MELLOR
!
!  Write out 2D radiation stress, Sxx-component.
!
      IF (Aout(idW2xx,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idW2xx), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgSxx2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2xx)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress, Sxy-component.
!
      IF (Aout(idW2xy,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idW2xy), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgSxy2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2xy)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress, Syy-component.
!
      IF (Aout(idW2yy,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idW2yy), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgSyy2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2yy)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress in the XI-direction.
!
      IF (Aout(idU2rs,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idU2rs), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask,                            &
#  endif
     &                     AVERAGE(ng) % avgu2RS)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU2rs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress in the ETA-direction
!
      IF (Aout(idV2rs,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idV2rs), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#  endif
     &                     AVERAGE(ng) % avgv2RS)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV2rs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D stokes momentum component (m/s) in the XI-direction.
!
      IF (Aout(idU2Sd,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idU2Sd), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask,                            &
#  endif
     &                     AVERAGE(ng) % avgu2Sd)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU2Sd)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D stokes momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idV2Sd,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idV2Sd), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#  endif
     &                     AVERAGE(ng) % avgv2Sd)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV2Sd)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# ifdef SOLVE3D
!
!  Write out 3D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idUvel,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idUvel), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgu3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#  if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 3D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idu3dD,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idu3dD), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#   endif
     &                     TIDES(ng) % u_detided)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu3dD)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
!
!  Write out 3D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idVvel,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idVvel), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % vmask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgv3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#  if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 3D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idv3dD,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idv3dD), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % vmask_avg,                        &
#   endif
     &                     TIDES(ng) % v_detided)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv3dD)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
!
!  Write out 3D Eastward momentum component (m/s) at RHO-points.
!
      IF (Aout(idu3dE,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idu3dE), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgu3dE)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu3dE)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D Northward momentum component (m/s) at RHO-points.
!
      IF (Aout(idv3dN,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idv3dN), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgv3dN)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv3dN)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#  ifdef NEARSHORE_MELLOR
!
!  Write out 3D radiation stress, Sxx-component.
!
      IF (Aout(idW3xx,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idW3xx), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSxx3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3xx)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress, Sxy-component.
!
      IF (Aout(idW3xy,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idW3xy), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSxy3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3xy)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress, Syy-component.
!
      IF (Aout(idW3yy,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idW3yy), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSyy3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3yy)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#   ifdef NEARSHORE_MELLOR05
!
!  Write out 3D radiation stress, Szx-component.
!
      IF (Aout(idW3zx,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idW3zx), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgSzx3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3zx)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress, Szy-component.
!
      IF (Aout(idW3zy,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idW3zy), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgSzy3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3zy)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
!
!  Write out 3D radiation stress in the XI-direction, u_Rstress.
!
      IF (Aout(idU3rs,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idU3rs), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % umask,                            &
#   endif
     &                     AVERAGE(ng) % avgu3RS)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU3rs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress in the ETA-direction, v_Rstress.
!
      IF (Aout(idV3rs,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idV3rs), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#   endif
     &                     AVERAGE(ng) % avgv3RS)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV3rs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D stokes momentum component (m/s) in the XI-direction.
!
      IF (Aout(idU3Sd,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idU3Sd), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % umask,                            &
#   endif
     &                     AVERAGE(ng) % avgu3Sd)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU3Sd)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D stokes momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idV3Sd,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idV3Sd), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#   endif
     &                     AVERAGE(ng) % avgv3Sd)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV3Sd)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
!
!  Write out S-coordinate omega vertical velocity (m/s).
!
      IF (Aout(idOvel,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idOvel), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgw3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idOvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out "true" vertical velocity (m/s).
!
      IF (Aout(idWvel,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idWvel), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgwvel)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idOvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out tracer type variables.
!
      DO itrc=1,NT(ng)
        IF (Aout(idTvar(itrc),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Tid(itrc), &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#  endif
     &                       AVERAGE(ng) % avgt(:,:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTvar(itrc))),            &
     &                          AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO

#  if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
      DO itrc=1,NAT
        IF (Aout(idTrcD(itrc),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid,                    &
     &                       AVG(ng)%Vid(idTrcD(itrc)),                 &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#   ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#   endif
     &                       TIDES(ng) % t_detided(:,:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTrcD(itrc))),            &
     &                          AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif
!
!  Write out density anomaly.
!
      IF (Aout(idDano,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idDano), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgrho)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idDano)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  ifdef LMD_SKPP
!
!  Write out depth of surface boundary layer.
!
      IF (Aout(idHsbl,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idHsbl), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avghsbl)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHsbl)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
#  ifdef LMD_BKPP
!
!  Write out depth of bottom boundary layer.
!
      IF (Aout(idHbbl,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idHbbl), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avghbbl)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHbbl)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
# endif
!
!  Write out 2D potential vorticity.
!
      IF (Aout(id2dPV,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*p2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(id2dPV), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % pmask,                            &
# endif
     &                     AVERAGE(ng) % avgpvor2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id2dPV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D relative vorticity.
!
      IF (Aout(id2dRV,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*p2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(id2dRV), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % pmask,                            &
# endif
     &                     AVERAGE(ng) % avgrvor2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id2dRV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# ifdef SOLVE3D
!
!  Write out 3D potential vorticity.
!
      IF (Aout(id3dPV,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*p3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(id3dPV), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % pmask,                            &
#  endif
     &                     AVERAGE(ng) % avgpvor3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id3dPV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D relative vorticity.
!
      IF (Aout(id3dRV,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*p3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(id3dRV), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % pmask,                            &
#  endif
     &                     AVERAGE(ng) % avgrvor3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id3dRV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
!
!  Write out quadratic <zeta*zeta> term.
!
      IF (Aout(idZZav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idZZav), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     AVERAGE(ng) % avgZZ)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idZZav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <ubar*ubar> term.
!
      IF (Aout(idU2av,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idU2av), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask,                            &
# endif
     &                     AVERAGE(ng) % avgU2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU2av)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <vbar*vbar> term.
!
      IF (Aout(idV2av,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idV2av), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask,                            &
# endif
     &                     AVERAGE(ng) % avgV2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV2av)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# ifdef SOLVE3D
!
!  Write u-volume flux.
!
      IF (Aout(idHUav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idHUav), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % umask,                            &
#  endif
     &                     AVERAGE(ng) % avgHuon)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHUav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write v-volume flux.
!
      IF (Aout(idHVav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idHVav), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#  endif
     &                     AVERAGE(ng) % avgHvom)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHVav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <u*u> term.
!
      IF (Aout(idUUav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idUUav), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % umask,                            &
#  endif
     &                     AVERAGE(ng) % avgUU)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUUav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <u*v> term.
!
      IF (Aout(idUVav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idUVav), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgUV)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUVav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <v*v> term.
!
      IF (Aout(idVVav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idVVav), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#  endif
     &                     AVERAGE(ng) % avgVV)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVVav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <t*t> term.
!
      DO i=1,NT(ng)
        IF (Aout(idTTav(i),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid,                    &
     &                       AVG(ng)%Vid(idTTav(i)),                    &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#  endif
     &                       AVERAGE(ng) % avgTT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out active tracer volume fluxes.
!
      DO i=1,NT(ng)
        IF (Aout(iHUTav(i),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*u3dvar
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid,                    &
     &                       AVG(ng)%Vid(iHUTav(i)),                    &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % umask,                          &
#  endif
     &                       AVERAGE(ng) % avgHuonT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,iHUTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
        IF (Aout(iHVTav(i),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*v3dvar
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid,                    &
     &                       AVG(ng)%Vid(iHVTav(i)),                    &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % vmask,                          &
#  endif
     &                       AVERAGE(ng) % avgHvomT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,iHVTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out quadratic <u*t> and <v*t> terms.
!
      DO i=1,NT(ng)
        IF (Aout(idUTav(i),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*u3dvar
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid,                    &
     &                       AVG(ng)%Vid(idUTav(i)),                    &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % umask,                          &
#  endif
     &                       AVERAGE(ng) % avgUT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idUTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
        IF (Aout(idVTav(i),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*v3dvar
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid,                    &
     &                       AVG(ng)%Vid(idVTav(i)),                    &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % vmask,                          &
#  endif
     &                       AVERAGE(ng) % avgVT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idVTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO

#  if defined LMD_MIXING || defined MY25_MIXING || defined GLS_MIXING
!
!  Write out vertical viscosity coefficient.
!
      IF (Aout(idVvis,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idVvis), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgAKv,                        &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVvis)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out vertical diffusion coefficient for potential temperature.
!
      IF (Aout(idTdif,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idTdif), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgAKt,                        &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTdif)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out vertical diffusion coefficient for salinity.
!
      IF (Aout(idSdif,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idSdif), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgAKs,                        &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSdif)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
#  if defined BULK_FLUXES || defined ECOSIM || defined ATM_PRESS
!
!  Write out surface air pressure.
!
      IF (Aout(idPair,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idPair), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgPair)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idPair)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
#  if defined BULK_FLUXES
!
!  Write out surface air temperature.
!
      IF (Aout(idTair,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idTair), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgTair)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTair)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
#  if defined BULK_FLUXES || defined ECOSIM
!
!  Write out surface winds.
!
      IF (Aout(idUair,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idUair), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgUwind)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUair)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

      IF (Aout(idVair,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idVair), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgVwind)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVair)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
!
!  Write out surface net heat flux.
!
      IF (Aout(idTsur(itemp),ng)) THEN
#  ifdef ADJOINT
#   if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
       defined OPT_OBSERVATIONS
        scale=1.0_dp/(rho0*Cp)
#   else
        scale=1.0_dp
#   endif
#  else
        scale=rho0*Cp
#  endif
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idTsur(itemp)),                  &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgstf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTsur(itemp))),             &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface salt flux  (PSU m/s = kg salt/m2/s).
!
      IF (Aout(idTsur(isalt),ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                      &
     &                     AVG(ng)%Vid(idTsur(isalt)),                  &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgswf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTsur(isalt))),             &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  ifdef BULK_FLUXES
!
!  Write out latent heat flux.
!
      IF (Aout(idLhea,ng)) THEN
#   ifdef ADJOINT
        scale=1.0_dp
#   else
        scale=rho0*Cp
#   endif
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idLhea), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avglhf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLhea)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out sensible heat flux.
!
      IF (Aout(idShea,ng)) THEN
#   ifdef ADJOINT
        scale=1.0_dp
#   else
        scale=rho0*Cp
#   endif
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idShea), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgshf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idShea)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out longwave radiation flux.
!
      IF (Aout(idLrad,ng)) THEN
#   ifdef ADJOINT
        scale=1.0_dp
#   else
        scale=rho0*Cp
#   endif
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idLrad), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avglrf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLrad)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   ifdef EMINUSP
!
!  Write out evaportaion rate (kg/m2/s).
!
      IF (Aout(idevap,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idevap), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgevap)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idevap)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out precipitation rate (kg/m2/s).
!
      IF (Aout(idrain,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idrain), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgrain)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idrain)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#  endif
#  ifdef SHORTWAVE
!
!  Write out shortwave radiation flux.
!
      IF (Aout(idSrad,ng)) THEN
#    ifdef ADJOINT
        scale=1.0_dp
#    else
        scale=rho0*Cp
#    endif
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idSrad), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgsrf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSrad)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
# endif
!
!  Write out surface u-momentum stress.
!
      IF (Aout(idUsms,ng)) THEN
# ifdef ADJOINT
#  if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
      defined OPT_OBSERVATIONS
        scale=1.0_dp/rho0
#  else
        scale=1.0_dp
#  endif
# else
        scale=rho0
# endif
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idUsms), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask,                            &
# endif
     &                     AVERAGE(ng) % avgsus)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUsms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface v-momentum stress.
!
      IF (Aout(idVsms,ng)) THEN
# ifdef ADJOINT
#  if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
      defined OPT_OBSERVATIONS
        scale=1.0_dp/rho0
#  else
        scale=1.0_dp
#  endif
# else
        scale=rho0
# endif
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idVsms), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask,                            &
# endif
     &                     AVERAGE(ng) % avgsvs)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVsms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom u-momentum stress.
!
      IF (Aout(idUbms,ng)) THEN
# ifdef ADJOINT
#  if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
      defined OPT_OBSERVATIONS
        scale=1.0_dp/rho0
#  else
        scale=1.0_dp
#  endif
# else
        scale=rho0
# endif
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idUbms), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask,                            &
# endif
     &                     AVERAGE(ng) % avgbus)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom v-momentum stress.
!
      IF (Aout(idVbms,ng)) THEN
# ifdef ADJOINT
#  if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
      defined OPT_OBSERVATIONS
        scale=1.0_dp/rho0
#  else
        scale=1.0_dp
#  endif
# else
        scale=rho0
# endif
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid, AVG(ng)%Vid(idVbms), &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask,                            &
# endif
     &                     AVERAGE(ng) % avgbvs)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# if defined SEDIMENT && defined BEDLOAD
!
!  Write out bed load transport in U-direction.
!
      DO itrc=1,NST
        IF (Aout(idUbld(itrc),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*u2dvar
          status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                    &
     &                       AVG(ng)%Vid(idUbld(itrc)),                 &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % umask,                          &
#  endif
     &                       SEDBED(ng) % avgbedldu(:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idUbld(itrc))),            &
     &                          AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
!  Write out bed load transport in V-direction.
!
        IF (Aout(idVbld(itrc),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*v2dvar
          status=nf_fwrite2d(ng, iNLM, AVG(ng)%ncid,                    &
     &                       AVG(ng)%Vid(idVbld(itrc)),                 &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % vmask,                          &
#  endif
     &                       SEDBED(ng) % avgbedldv(:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idVbld(itrc))),            &
     &                          AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
# endif
!
!-----------------------------------------------------------------------
!  Synchronize time-average NetCDF file to disk to allow other processes
!  to access data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, iNLM, AVG(ng)%name, AVG(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

#ifdef NESTING
      IF (Master) WRITE (stdout,20) AVG(ng)%Rindex, ng
#else
      IF (Master) WRITE (stdout,20) AVG(ng)%Rindex
#endif
!
  10  FORMAT (/,' WRT_AVG_NF90 - error while writing variable: ',a,     &
     &        /,16x,'into averages NetCDF file for time record: ',i0)
  20  FORMAT (2x,'WRT_AVG_NF90     - wrote averaged',t40,'fields',t59,  &
# ifdef NESTING
     &        'in record = ',i0,t92,i2.2)
# else
     &        'in record = ',i0)
# endif
!
      RETURN
      END SUBROUTINE wrt_avg_nf90

# if defined PIO_LIB && defined DISTRIBUTE
!
!***********************************************************************
      SUBROUTINE wrt_avg_pio (ng, tile,                                 &
     &                        LBi, UBi, LBj, UBj)
!***********************************************************************
!
      USE mod_pio_netcdf
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
!
!  Local variable declarations.
!
      integer :: Fcount, i, itrc, status
!
      real(dp) :: scale
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__//", wrt_avg_pio"
!
      TYPE (IO_desc_t), pointer :: ioDesc
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Write out time-averaged fields when appropriate.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Set time record index.
!
      AVG(ng)%Rindex=AVG(ng)%Rindex+1
      Fcount=AVG(ng)%load
      AVG(ng)%Nrec(Fcount)=AVG(ng)%Nrec(Fcount)+1
!
!  Write out averaged time.
!
      CALL pio_netcdf_put_fvar (ng, iNLM, AVG(ng)%name,                 &
     &                          TRIM(Vname(1,idtime)), AVGtime(ng:),    &
     &                          (/AVG(ng)%Rindex/), (/1/),              &
     &                          pioFile = AVG(ng)%pioFile,              &
     &                          pioVar = AVG(ng)%pioVar(idtime)%vd)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Write out free-surface (m).
!
      IF (Aout(idFsur,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idFsur)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idFsur),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgzeta)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idFsur)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#  if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided free-surface (m).
!
      IF (Aout(idFsuD,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idFsuD)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idFsuD),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     TIDES(ng) % zeta_detided)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idFsuD)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
#  ifdef ICESHELF
!
!  Write out ice shelf melt rate (m/s).
!
      IF (Aout(idismr,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idismr)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idismr),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgismr)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idismr)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   ifdef SURFACE_OVERFLUX_FIX
      IF (Aout(idssflxSOF,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idssflxSOF)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idssflxSOF),                  &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgssflxSOF)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idssflxSOF)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
      IF (Aout(idshflxSOF,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idshflxSOF)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idshflxSOF),                  &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgshflxSOF)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idshflxSOF)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#  endif
!
!  Write out 2D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idUbar,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idUbar)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idUbar),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgu2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbar)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#  if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 2D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idu2dD,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idu2dD)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idu2dD),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#   endif
     &                     TIDES(ng) % ubar_detided)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu2dD)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
!
!  Write out 2D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idVbar,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idVbar)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idVbar),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgv2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbar)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#  if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 2D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idv2dD,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idv2dD)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idv2dD),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % vmask_avg,                        &
#   endif
     &                     TIDES(ng) % vbar_detided)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv2dD)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
!
!  Write out 2D Eastward momentum component (m/s) at RHO-points.
!
      IF (Aout(idu2dE,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idu2dE)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idu2dE),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgu2dE)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu2dE)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Northward momentum component (m/s) at RHO-points.
!
      IF (Aout(idv2dN,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idv2dN)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idv2dN),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg,                        &
#  endif
     &                     AVERAGE(ng) % avgv2dN)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv2dN)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#  if defined FORWARD_WRITE && defined SOLVE3D
!
!  Write out time-averaged mass fluxes for 3D momentum coupling.
!
      IF (Aout(idUfx1,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idUfx1)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idUfx1),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#   endif
     &                     AVERAGE(ng) % avgDU_avg1)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUfx1)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
      IF (Aout(idUfx2,ng)) THEN
        IF (AVG(ng)%pioVar(idUfx2)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idUfx2),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#   endif
     &                     AVERAGE(ng) % avgDU_avg2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUfx2)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
      IF (Aout(idVfx1,ng)) THEN
        IF (AVG(ng)%pioVar(idVfx1)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idVfx1),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % vmask_avg,                        &
#   endif
     &                     AVERAGE(ng) % avgDV_avg1)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVfx1)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
      IF (Aout(idVfx2,ng)) THEN
        IF (AVG(ng)%pioVar(idVfx2)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idVfx2),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#   endif
     &                     AVERAGE(ng) % avgDV_avg2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVfx2)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
#  ifdef NEARSHORE_MELLOR
!
!  Write out 2D radiation stress, Sxx-component.
!
      IF (Aout(idW2xx,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idW2xx)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idW2xx),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSxx2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2xx)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress, Sxy-component.
!
      IF (Aout(idW2xy,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idW2xy)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idW2xy),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSxy2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2xy)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress, Syy-component.
!
      IF (Aout(idW2yy,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idW2yy)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idW2yy),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgSyy2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2yy)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress in the XI-direction.
!
      IF (Aout(idU2rs,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idU2rs)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idU2rs),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % umask,                            &
#   endif
     &                     AVERAGE(ng) % avgu2RS)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU2rs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress in the ETA-direction
!
      IF (Aout(idV2rs,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idV2rs)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idV2rs),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#   endif
     &                     AVERAGE(ng) % avgv2RS)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV2rs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D stokes momentum component (m/s) in the XI-direction.
!
      IF (Aout(idU2Sd,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idU2Sd)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idU2Sd),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % umask,                            &
#   endif
     &                     AVERAGE(ng) % avgu2Sd)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU2Sd)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D stokes momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idV2Sd,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idV2Sd)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idV2Sd),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#   endif
     &                     AVERAGE(ng) % avgv2Sd)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV2Sd)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
#  ifdef SOLVE3D
!
!  Write out 3D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idUvel,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idUvel)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idUvel),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#   endif
     &                     AVERAGE(ng) % avgu3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#   if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 3D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idu3dD,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idu3dD)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idu3dD),                      &
     &                     ioDesc,                                      &
     &                     AVG(ng)%Rindex,                              &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % umask_avg,                        &
#    endif
     &                     TIDES(ng) % u_detided)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu3dD)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
!
!  Write out 3D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idVvel,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idVvel)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idVvel),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % vmask_avg,                        &
#   endif
     &                     AVERAGE(ng) % avgv3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#   if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 3D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idv3dD,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idv3dD)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idv3dD),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % vmask_avg,                        &
#    endif
     &                     TIDES(ng) % v_detided)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv3dD)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
!
!  Write out 3D Eastward momentum component (m/s) at RHO-points.
!
      IF (Aout(idu3dE,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idu3dE)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idu3dE),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg,                        &
#   endif
     &                     AVERAGE(ng) % avgu3dE)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu3dE)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D Northward momentum component (m/s) at RHO-points.
!
      IF (Aout(idv3dN,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idv3dN)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idv3dN),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg,                        &
#   endif
     &                     AVERAGE(ng) % avgv3dN)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv3dN)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#   ifdef NEARSHORE_MELLOR
!
!  Write out 3D radiation stress, Sxx-component.
!
      IF (Aout(idW3xx,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idW3xx)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idW3xx),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgSxx3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3xx)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress, Sxy-component.
!
      IF (Aout(idW3xy,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idW3xy)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idW3xy),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgSxy3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3xy)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress, Syy-component.
!
      IF (Aout(idW3yy,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idW3yy)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idW3yy),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgSyy3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3yy)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#    ifdef NEARSHORE_MELLOR05
!
!  Write out 3D radiation stress, Szx-component.
!
      IF (Aout(idW3zx,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idW3zx)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idW3zx),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#     ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#     endif
     &                     AVERAGE(ng) % avgSzx3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3zx)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress, Szy-component.
!
      IF (Aout(idW3zy,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idW3zy)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idW3zy),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#     ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#     endif
     &                     AVERAGE(ng) % avgSzy3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW3zy)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#    endif
!
!  Write out 3D radiation stress in the XI-direction, u_Rstress.
!
      IF (Aout(idU3rs,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idU3rs)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idU3rs),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % umask,                            &
#    endif
     &                     AVERAGE(ng) % avgu3RS)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU3rs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D radiation stress in the ETA-direction, v_Rstress.
!
      IF (Aout(idV3rs,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idV3rs)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idV3rs),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#    endif
     &                     AVERAGE(ng) % avgv3RS)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV3rs)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D stokes momentum component (m/s) in the XI-direction.
!
      IF (Aout(idU3Sd,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idU3Sd)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idU3Sd),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % umask,                            &
#    endif
     &                     AVERAGE(ng) % avgu3Sd)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU3Sd)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D stokes momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idV3Sd,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idV3Sd)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idV3Sd),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#    endif
     &                     AVERAGE(ng) % avgv3Sd)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV3Sd)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
!
!  Write out S-coordinate omega vertical velocity (m/s).
!
      IF (Aout(idOvel,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idOvel)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_w3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_w3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idOvel),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgw3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idOvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out "true" vertical velocity (m/s).
!
      IF (Aout(idWvel,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idWvel)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_w3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_w3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idWvel),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgwvel)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idOvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out tracer type variables.
!
      DO itrc=1,NT(ng)
        IF (Aout(idTvar(itrc),ng)) THEN
          scale=1.0_dp
          IF (AVG(ng)%pioTrc(itrc)%dkind.eq.PIO_double) THEN
            ioDesc => ioDesc_dp_r3dvar(ng)
          ELSE
            ioDesc => ioDesc_sp_r3dvar(ng)
          END IF
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                 &
     &                       AVG(ng)%pioTrc(itrc),                      &
     &                       AVG(ng)%Rindex,                            &
     &                       ioDesc,                                    &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#   ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#   endif
     &                       AVERAGE(ng) % avgt(:,:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTvar(itrc))),            &
     &                          AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO

#   if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
      DO itrc=1,NAT
        IF (Aout(idTrcD(itrc),ng)) THEN
          scale=1.0_dp
          IF (AVG(ng)%pioVar(idTrcD(itrc))%dkind.eq.PIO_double) THEN
            ioDesc => ioDesc_dp_r3dvar(ng)
          ELSE
            ioDesc => ioDesc_sp_r3dvar(ng)
          END IF
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                 &
     &                       AVG(ng)%pioVar(idTrcD(itrc)),              &
     &                       AVG(ng)%Rindex,                            &
     &                       ioDesc,                                    &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#    ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#    endif
     &                       TIDES(ng) % t_detided(:,:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTrcD(itrc))),            &
     &                          AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#   endif
!
!  Write out density anomaly.
!
      IF (Aout(idDano,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idDano)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idDano),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgrho)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idDano)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#   ifdef LMD_SKPP
!
!  Write out depth of surface boundary layer.
!
      IF (Aout(idHsbl,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idHsbl)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idHsbl),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avghsbl)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHsbl)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#   ifdef LMD_BKPP
!
!  Write out depth of bottom boundary layer.
!
      IF (Aout(idHbbl,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idHbbl)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idHbbl),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avghbbl)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHbbl)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#  endif
!
!  Write out 2D potential vorticity.
!
      IF (Aout(id2dPV,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(id2dPV)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_p2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_p2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(id2dPV),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % pmask,                            &
#  endif
     &                     AVERAGE(ng) % avgpvor2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id2dPV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D relative vorticity.
!
      IF (Aout(id2dRV,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(id2dRV)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_p2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_p2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(id2dRV),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % pmask,                            &
#  endif
     &                     AVERAGE(ng) % avgrvor2d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id2dRV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#  ifdef SOLVE3D
!
!  Write out 3D potential vorticity.
!
      IF (Aout(id3dPV,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(id3dPV)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_p3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_p3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(id3dPV),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % pmask,                            &
#   endif
     &                     AVERAGE(ng) % avgpvor3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id3dPV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D relative vorticity.
!
      IF (Aout(id3dRV,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(id3dRV)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_p3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_p3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(id3dRV),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % pmask,                            &
#   endif
     &                     AVERAGE(ng) % avgrvor3d)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id3dRV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
!
!  Write out quadratic <zeta*zeta> term.
!
      IF (Aout(idZZav,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idZZav)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idZZav),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     AVERAGE(ng) % avgZZ)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idZZav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <ubar*ubar> term.
!
      IF (Aout(idU2av,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idU2av)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idU2av),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask,                            &
#  endif
     &                     AVERAGE(ng) % avgU2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU2av)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <vbar*vbar> term.
!
      IF (Aout(idV2av,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idV2av)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idV2av),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#  endif
     &                     AVERAGE(ng) % avgV2)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV2av)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#  ifdef SOLVE3D
!
!  Write u-volume flux.
!
      IF (Aout(idHUav,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idHUav)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idHUav),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % umask,                            &
#   endif
     &                     AVERAGE(ng) % avgHuon)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHUav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write v-volume flux.
!
      IF (Aout(idHVav,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idHVav)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idHVav),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#   endif
     &                     AVERAGE(ng) % avgHvom)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHVav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <u*u> term.
!
      IF (Aout(idUUav,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idUUav)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idUUav),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % umask,                            &
#   endif
     &                     AVERAGE(ng) % avgUU)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUUav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <u*v> term.
!
      IF (Aout(idUVav,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idUVav)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idUVav),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgUV)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUVav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <v*v> term.
!
      IF (Aout(idVVav,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idVVav)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idVVav),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#   ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#   endif
     &                     AVERAGE(ng) % avgVV)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVVav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <t*t> term.
!
      DO i=1,NT(ng)
        IF (Aout(idTTav(i),ng)) THEN
          scale=1.0_dp
          IF (AVG(ng)%pioVar(idTTav(i))%dkind.eq.PIO_double) THEN
            ioDesc => ioDesc_dp_r3dvar(ng)
          ELSE
            ioDesc => ioDesc_sp_r3dvar(ng)
          END IF
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                 &
     &                       AVG(ng)%pioVar(idTTav(i)),                 &
     &                       AVG(ng)%Rindex,                            &
     &                       ioDesc,                                    &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#   ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#   endif
     &                       AVERAGE(ng) % avgTT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out active tracer volume fluxes.
!
      DO i=1,NT(ng)
        IF (Aout(iHUTav(i),ng)) THEN
          scale=1.0_dp
          IF (AVG(ng)%pioVar(iHUTav(i))%dkind.eq.PIO_double) THEN
            ioDesc => ioDesc_dp_u3dvar(ng)
          ELSE
            ioDesc => ioDesc_sp_u3dvar(ng)
          END IF
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                 &
     &                       AVG(ng)%pioVar(iHUTav(i)),                 &
     &                       AVG(ng)%Rindex,                            &
     &                       ioDesc,                                    &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#   ifdef MASKING
     &                       GRID(ng) % umask,                          &
#   endif
     &                       AVERAGE(ng) % avgHuonT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,iHUTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
        IF (Aout(iHVTav(i),ng)) THEN
          scale=1.0_dp
          IF (AVG(ng)%pioVar(iHVTav(i))%dkind.eq.PIO_double) THEN
            ioDesc => ioDesc_dp_v3dvar(ng)
          ELSE
            ioDesc => ioDesc_sp_v3dvar(ng)
          END IF
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                 &
     &                       AVG(ng)%pioVar(iHVTav(i)),                 &
     &                       AVG(ng)%Rindex,                            &
     &                       ioDesc,                                    &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#   ifdef MASKING
     &                       GRID(ng) % vmask,                          &
#   endif
     &                       AVERAGE(ng) % avgHvomT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,iHVTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out quadratic <u*t> and <v*t> terms.
!
      DO i=1,NT(ng)
        IF (Aout(idUTav(i),ng)) THEN
          scale=1.0_dp
          IF (AVG(ng)%pioVar(idUTav(i))%dkind.eq.PIO_double) THEN
            ioDesc => ioDesc_dp_u3dvar(ng)
          ELSE
            ioDesc => ioDesc_sp_u3dvar(ng)
          END IF
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                 &
     &                       AVG(ng)%pioVar(idUTav(i)),                 &
     &                       AVG(ng)%Rindex,                            &
     &                       ioDesc,                                    &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#   ifdef MASKING
     &                       GRID(ng) % umask,                          &
#   endif
     &                       AVERAGE(ng) % avgUT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idUTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
        IF (Aout(idVTav(i),ng)) THEN
          scale=1.0_dp
          IF (AVG(ng)%pioVar(idVTav(i))%dkind.eq.PIO_double) THEN
            ioDesc => ioDesc_dp_v3dvar(ng)
          ELSE
            ioDesc => ioDesc_sp_v3dvar(ng)
          END IF
          status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                 &
     &                       AVG(ng)%pioVar(idVTav(i)),                 &
     &                       AVG(ng)%Rindex,                            &
     &                       ioDesc,                                    &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#   ifdef MASKING
     &                       GRID(ng) % vmask,                          &
#   endif
     &                       AVERAGE(ng) % avgVT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idVTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO

#   if defined LMD_MIXING || defined MY25_MIXING || defined GLS_MIXING
!
!  Write out vertical viscosity coefficient.
!
      IF (Aout(idVvis,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idVvis)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_w3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_w3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idVvis),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgAKv,                        &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVvis)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out vertical diffusion coefficient for potential temperature.
!
      IF (Aout(idTdif,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idTdif)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_w3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_w3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idTdif),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgAKt,                        &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTdif)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out vertical diffusion coefficient for salinity.
!
      IF (Aout(idSdif,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idSdif)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_w3dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_w3dvar(ng)
        END IF
        status=nf_fwrite3d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idSdif),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgAKs,                        &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSdif)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#   if defined BULK_FLUXES || defined ECOSIM || defined ATM_PRESS
!
!  Write out surface air pressure.
!
      IF (Aout(idPair,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idPair)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idPair),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgPair)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idPair)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#   if defined BULK_FLUXES
!
!  Write out surface air temperature.
!
      IF (Aout(idTair,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idTair)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idTair),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgTair)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTair)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#   if defined BULK_FLUXES || defined ECOSIM
!
!  Write out surface winds.
!
      IF (Aout(idUair,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idUair)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idUair),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgUwind)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUair)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
      IF (Aout(idVair,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idVair)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idVair),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgVwind)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVair)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
!
!  Write out surface net heat flux.
!
      IF (Aout(idTsur(itemp),ng)) THEN
#   ifdef ADJOINT
#    if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
       defined OPT_OBSERVATIONS
        scale=1.0_dp/(rho0*Cp)
#    else
        scale=1.0_dp
#    endif
#   else
        scale=rho0*Cp
#   endif
        IF (AVG(ng)%pioVar(idTsur(itemp))%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idTsur(itemp)),               &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgstf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTsur(itemp))),             &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface salt flux  (PSU m/s = kg salt/m2/s).
!
      IF (Aout(idTsur(isalt),ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idTsur(isalt))%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idTsur(isalt)),               &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     AVERAGE(ng) % avgswf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTsur(isalt))),             &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#   ifdef BULK_FLUXES
!
!  Write out latent heat flux.
!
      IF (Aout(idLhea,ng)) THEN
#    ifdef ADJOINT
        scale=1.0_dp
#    else
        scale=rho0*Cp
#    endif
        IF (AVG(ng)%pioVar(idLhea)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idLhea),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avglhf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLhea)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out sensible heat flux.
!
      IF (Aout(idShea,ng)) THEN
#    ifdef ADJOINT
        scale=1.0_dp
#    else
        scale=rho0*Cp
#    endif
        IF (AVG(ng)%pioVar(idShea)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idShea),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avgshf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idShea)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out longwave radiation flux.
!
      IF (Aout(idLrad,ng)) THEN
#    ifdef ADJOINT
        scale=1.0_dp
#    else
        scale=rho0*Cp
#    endif
        IF (AVG(ng)%pioVar(idLrad)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idLrad),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     AVERAGE(ng) % avglrf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLrad)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#    ifdef EMINUSP
!
!  Write out evaportaion rate (kg/m2/s).
!
      IF (Aout(idevap,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idevap)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idevap),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#     ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#     endif
     &                     AVERAGE(ng) % avgevap)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idevap)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out precipitation rate (kg/m2/s).
!
      IF (Aout(idrain,ng)) THEN
        scale=1.0_dp
        IF (AVG(ng)%pioVar(idrain)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idrain),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#     ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#     endif
     &                     AVERAGE(ng) % avgrain)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idrain)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#    endif
#   endif
#   ifdef SHORTWAVE
!
!  Write out shortwave radiation flux.
!
      IF (Aout(idSrad,ng)) THEN
#     ifdef ADJOINT
        scale=1.0_dp
#     else
        scale=rho0*Cp
#     endif
        IF (AVG(ng)%pioVar(idSrad)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_r2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_r2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idSrad),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#     ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#     endif
     &                     AVERAGE(ng) % avgsrf)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSrad)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#  endif
!
!  Write out surface u-momentum stress.
!
      IF (Aout(idUsms,ng)) THEN
#  ifdef ADJOINT
#   if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
      defined OPT_OBSERVATIONS
        scale=1.0_dp/rho0
#   else
        scale=1.0_dp
#   endif
#  else
        scale=rho0
#  endif
        IF (AVG(ng)%pioVar(idUsms)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idUsms),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask,                            &
#  endif
     &                     AVERAGE(ng) % avgsus)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUsms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface v-momentum stress.
!
      IF (Aout(idVsms,ng)) THEN
#  ifdef ADJOINT
#   if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
      defined OPT_OBSERVATIONS
        scale=1.0_dp/rho0
#   else
        scale=1.0_dp
#   endif
#  else
        scale=rho0
#  endif
        IF (AVG(ng)%pioVar(idVsms)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idVsms),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#  endif
     &                     AVERAGE(ng) % avgsvs)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVsms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom u-momentum stress.
!
      IF (Aout(idUbms,ng)) THEN
#  ifdef ADJOINT
#   if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
      defined OPT_OBSERVATIONS
        scale=1.0_dp/rho0
#   else
        scale=1.0_dp
#   endif
#  else
        scale=rho0
#  endif
        IF (AVG(ng)%pioVar(idUbms)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_u2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_u2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idUbms),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask,                            &
#  endif
     &                     AVERAGE(ng) % avgbus)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom v-momentum stress.
!
      IF (Aout(idVbms,ng)) THEN
#  ifdef ADJOINT
#   if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
      defined OPT_OBSERVATIONS
        scale=1.0_dp/rho0
#   else
        scale=1.0_dp
#   endif
#  else
        scale=rho0
#  endif
        IF (AVG(ng)%pioVar(idVbms)%dkind.eq.PIO_double) THEN
          ioDesc => ioDesc_dp_v2dvar(ng)
        ELSE
          ioDesc => ioDesc_sp_v2dvar(ng)
        END IF
        status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                   &
     &                     AVG(ng)%pioVar(idVbms),                      &
     &                     AVG(ng)%Rindex,                              &
     &                     ioDesc,                                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask,                            &
#  endif
     &                     AVERAGE(ng) % avgbvs)
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

#  if defined SEDIMENT && defined BEDLOAD
!
!  Write out bed load transport in U-direction.
!
      DO itrc=1,NST
        IF (Aout(idUbld(itrc),ng)) THEN
          scale=1.0_dp
          IF (AVG(ng)%pioVar(idUbld(itrc))%dkind.eq.PIO_double) THEN
            ioDesc => ioDesc_dp_u2dvar(ng)
          ELSE
            ioDesc => ioDesc_sp_u2dvar(ng)
          END IF
          status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                 &
     &                       AVG(ng)%pioVar(idUbld(itrc)),              &
     &                       AVG(ng)%Rindex,                            &
     &                       ioDesc,                                    &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#   ifdef MASKING
     &                       GRID(ng) % umask,                          &
#   endif
     &                       SEDBED(ng) % avgbedldu(:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idUbld(itrc))),            &
     &                          AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
!  Write out bed load transport in V-direction.
!
        IF (Aout(idVbld(itrc),ng)) THEN
          scale=1.0_dp
          IF (AVG(ng)%pioVar(idVbld(itrc))%dkind.eq.PIO_double) THEN
            ioDesc => ioDesc_dp_v2dvar(ng)
          ELSE
            ioDesc => ioDesc_sp_v2dvar(ng)
          END IF
          status=nf_fwrite2d(ng, iNLM, AVG(ng)%pioFile,                 &
     &                       AVG(ng)%pioVar(idVbld(itrc)),              &
     &                       AVG(ng)%Rindex,                            &
     &                       ioDesc,                                    &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#   ifdef MASKING
     &                       GRID(ng) % vmask,                          &
#   endif
     &                       SEDBED(ng) % avgbedldv(:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idVbld(itrc))),            &
     &                          AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif
!
!-----------------------------------------------------------------------
!  Synchronize time-average NetCDF file to disk to allow other processes
!  to access data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL pio_netcdf_sync (ng, iNLM, AVG(ng)%name, AVG(ng)%pioFile)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# ifdef NESTING
      IF (Master) WRITE (stdout,20) AVG(ng)%Rindex, ng
# else
      IF (Master) WRITE (stdout,20) AVG(ng)%Rindex
# endif
!
  10  FORMAT (/,' WRT_AVG_PIO - error while writing variable: ',a,      &
     &        /,16x,'into averages NetCDF file for time record: ',i0)
  20  FORMAT (2x,'WRT_AVG_PIO      - wrote averaged',t40,'fields',t59,  &
#  ifdef NESTING
     &        'in record = ',i0,t92,i2.2)
#  else
     &        'in record = ',i0)
#  endif
!
      RETURN
      END SUBROUTINE wrt_avg_pio
# endif
#endif
      END MODULE wrt_avg_mod
