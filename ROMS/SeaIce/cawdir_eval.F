#include "cppdefs.h"
      MODULE cawdir_eval_mod
#ifdef ICE_ALB_DIRDIFF
! ----------------------------------------------------------------------
!  compute 24 hrs mean albedo at the marine surface layer
! ----------------------------------------------------------------------
!
      implicit none

      PRIVATE
      PUBLIC cawdir_eval

      CONTAINS
!
!***********************************************************************
      SUBROUTINE cawdir_eval (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid
# ifdef ICE_MODEL
      USE mod_ice
# endif
      USE mod_stepping
!
      integer, intent(in) :: ng, tile

# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 17, __LINE__, __FILE__)
# endif
      CALL cawdir_eval_tile (ng, tile,                                  &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
# ifdef MASKING
     &                       GRID(ng) % rmask,                          &
# endif
     &                       GRID(ng) % latr,                           &
     &                       GRID(ng) % lonr,                           &
# ifdef CLOUDS
     &                       FORCES(ng) % cloud,                        &
# endif
     &                       FORCES(ng) % cawdir)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 17, __LINE__, __FILE__)
# endif
      RETURN
      END SUBROUTINE cawdir_eval
!
!********************************************************************
      SUBROUTINE cawdir_eval_tile (ng, tile,                            &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
# ifdef MASKING
     &                             rmask,                               &
# endif
     &                             latr, lonr                           &
# ifdef CLOUDS
     &                             cloud,                               &
# endif
     &                             cawdir)

!********************************************************************
!
      USE mod_param
      USE mod_scalars
      USE dateclock_mod,   ONLY : caldate
!
      USE bc_2d_mod, ONLY : bc_r2d_tile
      USE exchange_2d_mod
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS

# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: latr(LBi:,LBj:)
      real(r8), intent(in) :: lonr(LBi:,LBj:)
#  ifdef CLOUDS
      real(r8), intent(in) :: cloud(LBi:,LBj:)
#  endif
      real(r8), intent(out) :: cawdir(LBi:,LBj:)
# else
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: latr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: lonr(LBi:UBi,LBj:UBj)
#  ifdef CLOUDS
      real(r8), intent(in) :: cloud(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(out) :: cawdir(LBi:UBi,LBj:UBj)
# endif
!
!  Local variable declarations.
!
      integer :: ifrac, npart, iday, month, year
      integer :: i, j, k

      real(r8) :: pi2, deg, eepsil, fraci
      real(r8) :: absh2o, s0caw, day, dangle
      real(r8) :: decli, sundv, cc, intpol
      real(r8) :: sin2, cos2, stot, sads, bioday
      real(r8) :: biohr, hangle, langle, cosz, srad
      real(r8) :: sdir, sdif, altdeg, cfac, ssurf
      real(r8) :: albdir, rlat, yday

# include "set_bounds.h"
!
! --- ------------------------------------------------------------------
! --- compute 24 hrs mean albedo at the marine surface layer
! --- ------------------------------------------------------------------
!
! --- set various quantities
!
!        2 times pi
!
      pi2=2._r8*pi
!
!        convert from radians to degrees
!
      deg=360._r8/pi2
!
!        small number
!
      eepsil=1.0E-9_r8
!
!        split each 12 hrs day into ifrac parts
!
      ifrac=24
!
!        1 over ifrac
!
      fraci=1._r8/REAL(ifrac,r8)
!
! ---    absorption of water and ozone
!
      absh2o=0.09_r8
!
! w/m^2  solar constant
!
      s0caw=1365._r8
!
! --- ------------------------------------------------------------------
! --- compute 24 hrs mean albedo at the marine surface layer
! --- ------------------------------------------------------------------
!
      CALL caldate (tdays(ng), yd_dp=yday, dd_dp=day)
!
! 0 < day < 364
!
      day=MOD(yday-1._r8,365._r8)
!
! day-number-angle, in radians
!
      dangle=pi2*day/365._r8
!
! --- compute astronomic quantities
!
      decli=.006918_r8+.070257_r8*SIN(dangle)   -.399912_r8*COS(dangle) &
     &     +.000907_r8*SIN(2._r8*dangle)-.006758_r8*COS(2._r8*dangle)   &
     &     +.001480_r8*SIN(3._r8*dangle)-.002697_r8*COS(3._r8*dangle)
!
      sundv=1.00011_r8+.001280_r8*SIN(dangle)   +.034221_r8*COS(dangle) &
     &      +.000077_r8*SIN(2._r8*dangle)+.000719_r8*COS(2._r8*dangle)
!
! --- scan through each of the grid cells
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
!
! --- compute cloudiness fraction
!
# ifdef CLOUDS
          cc = cloud(i,j)
# else
          cc = 0.7_r8
# endif
!
! --- compute astronomic quantities
!
          rlat = latr(i,j)*deg2rad
          sin2=SIN(rlat)*SIN(decli)
          cos2=COS(rlat)*COS(decli)
!
! --- split each day into ifrac parts, and compute the solar radiance for
! --- each part. by assuming symmetry of the irradiance about noon, it
! --- is sufficient to compute the irradiance for the first 12 hrs of
! --- the (24 hrs) day (mean for the first 12 hrs equals then the mean
! --- for the last 12 hrs)
!
          stot=0._r8
          sads=0._r8
!
          DO npart=1,ifrac
!
            bioday=day+(npart-.5_r8)*fraci*.5_r8
!
! hour of day in seconds
! hour of day;  biohr=0  at noon
! hour angle, in radians
!
            biohr=bioday*86400._r8
            biohr=MOD(biohr+43200._r8,86400._r8)
            hangle=pi2*biohr/86400._r8
# ifdef ICE_ALB_GREGORIAN
!
! add a correction term to correct the time difference between Gregorian
!   time and local time.
!
!   CW 2022-01-07
!
            langle=pi2*MOD(lonr(i,j), 360._r8)/360._r8
            hangle=hangle + langle
# endif
!
! cosine of the zenith angle
!
            cosz=MIN(1._r8,MAX(0._r8,sin2+cos2*COS(hangle)))
!
! extraterrestrial radiation
!
            srad=s0caw*sundv*cosz
!
! direct radiation component
!
            sdir=srad*0.7_r8**MIN(100._r8,1._r8/(cosz+eepsil))
!
! diffusive radiation component
!
            sdif=((1._r8-absh2o)*srad-sdir)*.5_r8
!
! solar noon altitude in degrees
!
            altdeg=MAX(0._r8,ASIN(sin2+cos2))*deg
!
! cloudiness correction
!
            cfac=(1._r8-0.62_r8*cc+0.0019_r8*altdeg)
!
            ssurf=(sdir+sdif)*cfac+eepsil
!
            stot=stot+ssurf
!
! albedo for direct light
!
            albdir=0.03_r8*EXP(0.742_r8*ACOS(cosz)**2.866_r8)
!
! radiation weighted sum of direct albedo
!
            sads=sads+albdir*ssurf
          ENDDO
!
! daily mean co-albedo over water
!
          cawdir(i,j)=1._r8-sads/stot
#ifdef MASKING
          cawdir(i,j) = cawdir(i,j)*rmask(i,j)
#endif
        ENDDO
      ENDDO
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          cawdir)
      END IF
#ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    cawdir)
#endif

      RETURN
      END SUBROUTINE cawdir_eval_tile
#endif
      END MODULE cawdir_eval_mod
