#include "cppdefs.h"
      MODULE ice_elastic_mod
#ifdef ICE_EVP
!
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2022 The ROMS/TOMS Group          Chuning Wang   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine timesteps the ice momentum equations.                  !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: ice_elastic

      CONTAINS

      SUBROUTINE ice_elastic (ng, tile)

      USE mod_param
      USE mod_grid
      USE mod_ice
      USE mod_stepping
#ifdef ICE_SHOREFAST
      USE mod_coupling
#endif
#ifdef ICE_LANDFAST_CLM
      USE mod_forces
#endif

      integer, intent(in) :: ng, tile
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 94, __LINE__, __FILE__)
# endif
!
      CALL ice_elastic_tile (ng, tile,                                  &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
     &                       liold(ng), liuol(ng), liunw(ng),           &
     &                       lieol(ng), lienw(ng),                      &
# ifdef MASKING
     &                       GRID(ng) % umask,                          &
     &                       GRID(ng) % vmask,                          &
# endif
# ifdef WET_DRY
     &                       GRID(ng) % umask_wet,                      &
     &                       GRID(ng) % vmask_wet,                      &
# endif
# ifdef ICESHELF
     &                       GRID(ng) % zice,                           &
# endif
     &                       GRID(ng) % pm,                             &
     &                       GRID(ng) % pn,                             &
     &                       GRID(ng) % om_u,                           &
     &                       GRID(ng) % on_u,                           &
     &                       GRID(ng) % om_v,                           &
     &                       GRID(ng) % on_v,                           &
     &                       GRID(ng) % f,                              &
# ifdef ICE_SHOREFAST
     &                       GRID(ng) % h,                              &
     &                       COUPLING(ng) % Zt_avg1,                    &
# endif
# ifdef ICE_LANDFAST
     &                       ICE(ng) % h_loadu,                         &
     &                       ICE(ng) % h_loadv,                         &
# endif
# ifdef ICE_LANDFAST_CLM
     &                       FORCES(ng) % fastice_clm,                  &
# endif
     &                       ICE(ng) % uwater,                          &
     &                       ICE(ng) % vwater,                          &
     &                       ICE(ng) % sealev,                          &
     &                       ICE(ng) % ai,                              &
     &                       ICE(ng) % hi,                              &
     &                       ICE(ng) % sig11,                           &
     &                       ICE(ng) % sig22,                           &
     &                       ICE(ng) % sig12,                           &
     &                       ICE(ng) % tauaiu,                          &
     &                       ICE(ng) % tauaiv,                          &
     &                       ICE(ng) % chu_iw,                          &
     &                       ICE(ng) % uie,                             &
     &                       ICE(ng) % vie,                             &
     &                       ICE(ng) % ui,                              &
     &                       ICE(ng) % vi)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 94, __LINE__, __FILE__)
# endif
!
      RETURN
      END SUBROUTINE ice_elastic
!
!***********************************************************************
      SUBROUTINE ice_elastic_tile (ng, tile,                            &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
     &                             liold, liuol, liunw, lieol, lienw,   &
# ifdef MASKING
     &                             umask, vmask,                        &
# endif
# ifdef WET_DRY
     &                             umask_wet, vmask_wet,                &
# endif
# ifdef ICESHELF
     &                             zice,                                &
# endif
     &                             pm, pn, om_u, on_u, om_v, on_v,      &
     &                             f,                                   &
# ifdef ICE_SHOREFAST
     &                             h,                                   &
     &                             Zt_avg1,                             &
# endif
# ifdef ICE_LANDFAST
     &                             h_loadu, h_loadv,                    &
# endif
# ifdef ICE_LANDFAST_CLM
     &                             fastice_clm,                         &
# endif
     &                             uwater, vwater, sealev,              &
     &                             ai, hi, sig11, sig22, sig12,         &
     &                             tauaiu, tauaiv, chu_iw,              &
     &                             uie, vie,                            &
     &                             ui, vi)
!***********************************************************************
!

      USE mod_param
      USE mod_scalars
!
      USE uibc_mod, ONLY : uibc_tile
      USE vibc_mod, ONLY : vibc_tile
!
      USE exchange_2d_mod
#ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
#endif
      USE mod_clima
!
      implicit none

!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: liold, liuol, liunw, lieol, lienw
# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: umask_wet(LBi:,LBj:)
      real(r8), intent(in) :: vmask_wet(LBi:,LBj:)
#  endif
#  ifdef ICESHELF
      real(r8), intent(in) :: zice(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: om_u(LBi:,LBj:)
      real(r8), intent(in) :: on_u(LBi:,LBj:)
      real(r8), intent(in) :: om_v(LBi:,LBj:)
      real(r8), intent(in) :: on_v(LBi:,LBj:)
      real(r8), intent(in) :: f(LBi:,LBj:)
#  ifdef ICE_SHOREFAST
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: Zt_avg1(LBi:,LBj:)
#  endif
#  ifdef ICE_LANDFAST
      real(r8), intent(in) :: h_loadu(LBi:,LBj:)
      real(r8), intent(in) :: h_loadv(LBi:,LBj:)
#  endif
#  ifdef ICE_LANDFAST_CLM
      real(r8), intent(in) :: fastice_clm(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: uwater(LBi:,LBj:)
      real(r8), intent(in) :: vwater(LBi:,LBj:)
      real(r8), intent(in) :: sealev(LBi:,LBj:)
      real(r8), intent(in) :: ai(LBi:,LBj:,:)
      real(r8), intent(in) :: hi(LBi:,LBj:,:)
      real(r8), intent(in) :: sig11(LBi:,LBj:,:)
      real(r8), intent(in) :: sig22(LBi:,LBj:,:)
      real(r8), intent(in) :: sig12(LBi:,LBj:,:)
      real(r8), intent(in) :: tauaiu(LBi:,LBj:)
      real(r8), intent(in) :: tauaiv(LBi:,LBj:)
      real(r8), intent(in) :: chu_iw(LBi:,LBj:)
      real(r8), intent(inout) :: uie(LBi:,LBj:,:)
      real(r8), intent(inout) :: vie(LBi:,LBj:,:)
      real(r8), intent(out) :: ui(LBi:,LBj:,:)
      real(r8), intent(out) :: vi(LBi:,LBj:,:)
# else
#  ifdef MASKING
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: umask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask_wet(LBi:UBi,LBj:UBj)
#  endif
#  ifdef ICESHELF
      real(r8), intent(in) :: zice(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: f(LBi:UBi,LBj:UBj)
#  ifdef ICE_SHOREFAST
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Zt_avg1(LBi:UBi,LBj:UBj)
#  endif
#  ifdef ICE_LANDFAST
      real(r8), intent(in) :: h_loadu(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: h_loadv(LBi:UBi,LBj:UBj)
#  endif
#  ifdef ICE_LANDFAST_CLM
      real(r8), intent(in) :: fastice_clm(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: uwater(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vwater(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: sealev(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: ai(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: hi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: sig11(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: sig22(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: sig12(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: tauaiu(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: tauaiv(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: chu_iw(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: uie(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: vie(LBi:UBi,LBj:UBj,2)
      real(r8), intent(out) :: ui(LBi:UBi,LBj:UBj,2)
      real(r8), intent(out) :: vi(LBi:UBi,LBj:UBj,2)
# endif

! Local variable definitions
!
      integer :: i, j
!
      real(r8) :: cff, cff2, cff3, cff4, udrag, vdrag
      real(r8) :: f1, f2, s1, s2, fakt
      real(r8) :: uforce, vforce
      real(r8) :: alfa, dsum
      real(r8) :: masu, masv
      real(r8) :: chux, chuy
      real(r8) :: auf, avf
      real(r8) :: pmu, pnu, pmv, pnv
      real(r8) :: mfu11, mfu21, mfu12, mfu22
      real(r8) :: mfv11, mfv21, mfv12, mfv22
# ifdef ICE_SHOREFAST
      real(r8) :: clear, hu, hv
# endif
# ifdef ICE_LANDFAST
      real(r8) :: speed, v_u, u_v, tau_bu, tau_bv
# endif

# include "set_bounds.h"
!
# ifdef ICE_lIMIT_BSTRESS
!
!  Set limiting factor for bottom stress. The bottom stress is adjusted
!   to not change the direction of momentum.  It only should slow down
!   to zero. The value of 0.75 is arbitrary limitation assigment.
!
      cff=0.75_r8/dte(ng)
# endif
!
! ----------------------------------------------------------------------
!
! *** momentum equation
!
!     u-eqn
!
      DO j=Jstr,Jend
        DO i=IstrP,Iend
          uforce = 0._r8
          chux = 0.5_r8*(chu_iw(i-1,j)+chu_iw(i,j))
          masu = 0.5_r8*(hi(i,j,liold)+hi(i-1,j,liold))
          masu = MAX(masu,0.1_r8)
          masu = masu*rhoice(ng)
          auf = MAX(0.5_r8*(ai(i-1,j,liold)+ai(i,j,liold)),0.01_r8)
          pmu = 0.5_r8*(pm(i,j) + pm(i-1,j))
          pnu = 0.5_r8*(pn(i,j) + pn(i-1,j))
!
! *** forces from ice rheology x-direction
!
          s1 = (sig11(i,j,lienw)-sig11(i-1,j,lienw))*pmu
# ifdef MASKING
          IF (umask(i,j).gt.0.0_r8.and.umask(i,j+1).lt.1.0_r8) THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
#  ifdef WET_DRY
          ELSE IF (umask_wet(i,j).gt.0.0_r8.and.                        &
     &             umask_wet(i,j+1).lt.1.0_r8) THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
#  endif
#  ifdef ICESHELF
          ELSE IF ( zice(i-1,j  )             .eq.0.0_r8 .and.          &
     &              zice(i  ,j  )             .eq.0.0_r8 .and.          &
     &             (zice(i-1,j+1)+zice(i,j+1)).ne.0.0_r8) THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
#  endif
          ELSE
            f1 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i  ,j+1,lienw) +   &
     &                    sig12(i-1,j+1,lienw)+sig12(i-1,j  ,lienw))
          END IF
# elif defined WET_DRY
          IF (umask_wet(i,j).gt.0.0_r8.and.umask_wet(i,j+1).lt.1.0_r8)  &
     &    THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
#  ifdef ICESHELF
          ELSE IF ( zice(i-1,j  )             .eq.0.0_r8 .and.          &
     &              zice(i  ,j  )             .eq.0.0_r8 .and.          &
     &             (zice(i-1,j+1)+zice(i,j+1)).ne.0.0_r8) THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
#  endif
          ELSE
            f1 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i  ,j+1,lienw) +   &
     &                    sig12(i-1,j+1,lienw)+sig12(i-1,j  ,lienw))
          END IF
# else
#  ifdef ICESHELF
          IF ( zice(i-1,j  )             .eq.0.0_r8 .and.               &
     &         zice(i  ,j  )             .eq.0.0_r8 .and.               &
     &        (zice(i-1,j+1)+zice(i,j+1)).ne.0.0_r8) THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
          ELSE
            f1 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i  ,j+1,lienw) +   &
     &                    sig12(i-1,j+1,lienw)+sig12(i-1,j  ,lienw))
          END IF
#  else
          f1 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i  ,j+1,lienw) +     &
     &                  sig12(i-1,j+1,lienw)+sig12(i-1,j  ,lienw))
#  endif
# endif

# ifdef MASKING
          IF (umask(i,j).gt.0.0_r8.and.umask(i,j-1).lt.1.0_r8) THEN
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
#  ifdef WET_DRY
          ELSE IF (umask_wet(i,j  ).gt.0.0_r8.and.                      &
     &             umask_wet(i,j-1).lt.1.0_r8) THEN
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
#  endif
#  ifdef ICESHELF
          ELSE IF ( zice(i-1,j  )             .eq.0.0_r8 .and.          &
     &              zice(i  ,j  )             .eq.0.0_r8 .and.          &
     &             (zice(i-1,j-1)+zice(i,j-1)).ne.0.0_r8) then
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
#  endif
          ELSE
            f2 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i-1,j  ,lienw) +   &
     &                    sig12(i-1,j-1,lienw)+sig12(i  ,j-1,lienw))
          END IF
# elif defined WET_DRY
          IF (umask_wet(i,j  ).gt.0.0_r8.and.                           &
     &        umask_wet(i,j-1).lt.1.0_r8) THEN
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
#  ifdef ICESHELF
          ELSE IF ( zice(i-1,j  )             .eq.0.0_r8 .and.          &
     &              zice(i  ,j  )             .eq.0.0_r8 .and.          &
     &             (zice(i-1,j-1)+zice(i,j-1)).ne.0.0_r8) then
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
#  endif
          ELSE
            f2 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i-1,j  ,lienw) +   &
     &                    sig12(i-1,j-1,lienw)+sig12(i  ,j-1,lienw))
          END IF
# else
#  ifdef ICESHELF
          IF ( zice(i-1,j  )             .eq.0.0_r8 .and.               &
     &         zice(i  ,j  )             .eq.0.0_r8 .and.               &
     &        (zice(i-1,j-1)+zice(i,j-1)).ne.0.0_r8) then
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i-1,j,lienw))
          ELSE
            f2 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i-1,j  ,lienw) +   &
     &                    sig12(i-1,j-1,lienw)+sig12(i  ,j-1,lienw))
          END IF
#  else
          f2 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i-1,j  ,lienw) +     &
     &                  sig12(i-1,j-1,lienw)+sig12(i  ,j-1,lienw))
#  endif
# endif
!
          s2 = (f1-f2)*pnu
          uforce = s1 + s2
!
! *** wind force
!
          uforce = uforce + tauaiu(i,j)
!
! *** pressure from tilting ocean surface
!
          uforce = uforce - g*(masu)*pmu*(sealev(i,j)-sealev(i-1,j))

          fakt = 0.0_r8
# ifdef MASKING
#  ifdef WET_DRY
          dsum = vmask(i-1,j  )*vmask_wet(i-1,j  ) +                    &
     &           vmask(i  ,j  )*vmask_wet(i  ,j  ) +                    &
     &           vmask(i-1,j+1)*vmask_wet(i-1,j+1) +                    &
     &           vmask(i  ,j+1)*vmask_wet(i  ,j+1)
          if (dsum.gt.0.0_r8) fakt = 1.0_r8/dsum
          mfv11 = 0.5_r8*(hi(i-1,j-1,liold)*f(i-1,j-1)+                 &
     &                    hi(i-1,j  ,liold)*f(i-1,j  ))*                &
     &                   vie(i-1,j  ,lieol)*                            &
     &                   vmask(i-1,j  )*vmask_wet(i-1,j  )
          mfv21 = 0.5_r8*(hi(i  ,j-1,liold)*f(i  ,j-1)+                 &
     &                    hi(i  ,j  ,liold)*f(i  ,j  ))*                &
     &                   vie(i  ,j  ,lieol)*                            &
     &                   vmask(i  ,j  )*vmask_wet(i  ,j  )
          mfv12 = 0.5_r8*(hi(i-1,j  ,liold)*f(i-1,j  )+                 &
     &                    hi(i-1,j+1,liold)*f(i-1,j+1))*                &
     &                   vie(i-1,j+1,lieol)*                            &
     &                   vmask(i-1,j+1)*vmask_wet(i-1,j+1)
          mfv22 = 0.5_r8*(hi(i  ,j  ,liold)*f(i  ,j  )+                 &
     &                    hi(i  ,j+1,liold)*f(i  ,j+1))*                &
     &                   vie(i  ,j+1,lieol)*                            &
     &                   vmask(i  ,j+1)*vmask_wet(i  ,j+1)
#  else
          dsum = vmask(i-1,j)+vmask(i,j)+vmask(i-1,j+1)+vmask(i,j+1)
          if (dsum.gt.0.0_r8) fakt = 1.0_r8/dsum
          mfv11 = 0.5_r8*(hi(i-1,j-1,liold)*f(i-1,j-1)+                 &
     &                    hi(i-1,j  ,liold)*f(i-1,j  ))*                &
     &                   vie(i-1,j  ,lieol)*vmask(i-1,j  )
          mfv21 = 0.5_r8*(hi(i  ,j-1,liold)*f(i  ,j-1)+                 &
     &                    hi(i  ,j  ,liold)*f(i  ,j  ))*                &
     &                   vie(i  ,j  ,lieol)*vmask(i  ,j  )
          mfv12 = 0.5_r8*(hi(i-1,j  ,liold)*f(i-1,j  )+                 &
     &                    hi(i-1,j+1,liold)*f(i-1,j+1))*                &
     &                   vie(i-1,j+1,lieol)*vmask(i-1,j+1)
          mfv22 = 0.5_r8*(hi(i  ,j  ,liold)*f(i  ,j  )+                 &
     &                    hi(i  ,j+1,liold)*f(i  ,j+1))*                &
     &                   vie(i  ,j+1,lieol)*vmask(i  ,j+1)
#  endif
# elif defined WET_DRY
          dsum = vmask_wet(i-1,j  )+vmask_wet(i  ,j  )+                 &
     &           vmask_wet(i-1,j+1)+vmask_wet(i  ,j+1)
          if (dsum.gt.0.0_r8) fakt = 1.0_r8/dsum
          mfv11 = 0.5_r8*(hi(i-1,j-1,liold)*f(i-1,j-1)+                 &
     &                    hi(i-1,j  ,liold)*f(i-1,j  ))*                &
     &                   vie(i-1,j  ,lieol)*vmask_wet(i-1,j  )
          mfv21 = 0.5_r8*(hi(i  ,j-1,liold)*f(i  ,j-1)+                 &
     &                    hi(i  ,j  ,liold)*f(i  ,j  ))*                &
     &                   vie(i  ,j  ,lieol)*vmask_wet(i  ,j  )
          mfv12 = 0.5_r8*(hi(i-1,j  ,liold)*f(i-1,j  )+                 &
     &                    hi(i-1,j+1,liold)*f(i-1,j+1))*                &
     &                   vie(i-1,j+1,lieol)*vmask_wet(i-1,j+1)
          mfv22 = 0.5_r8*(hi(i  ,j  ,liold)*f(i  ,j  )+                 &
     &                    hi(i  ,j+1,liold)*f(i  ,j+1))*                &
     &                   vie(i  ,j+1,lieol)*vmask_wet(i  ,j+1)
# else
          fakt = 0.25_r8
          mfv11 = 0.5_r8*(hi(i-1,j-1,liold)*f(i-1,j-1)+                 &
     &                    hi(i-1,j  ,liold)*f(i-1,j  ))*                &
     &                   vie(i-1,j  ,lieol)
          mfv21 = 0.5_r8*(hi(i  ,j-1,liold)*f(i  ,j-1)+                 &
     &                    hi(i  ,j  ,liold)*f(i  ,j  ))*                &
     &                   vie(i  ,j  ,lieol)
          mfv12 = 0.5_r8*(hi(i-1,j  ,liold)*f(i-1,j  )+                 &
     &                    hi(i-1,j+1,liold)*f(i-1,j+1))*                &
     &                   vie(i-1,j+1,lieol)
          mfv22 = 0.5_r8*(hi(i  ,j  ,liold)*f(i  ,j  )+                 &
     &                    hi(i  ,j+1,liold)*f(i  ,j+1))*                &
     &                   vie(i  ,j+1,lieol)
# endif
!
! *** coriolis force u
!
          uforce = uforce + fakt*rhoice(ng)*                            &
     &                      (mfv11 + mfv21 + mfv12 + mfv22)
!
! *** stress from ocean current
!
          uforce = uforce + auf*chux*rho0*uwater(i,j)
!
          alfa = masu + dte(ng)*auf*rho0*chux
!
# ifdef ICE_LANDFAST
!
! *** stress on bottom of keels
!
          IF (max(ai(i-1,j,liold),ai(i,j,liold)) > 0.01_r8) THEN
!
!  Don't have landfast ice in nudging band
!
            IF (LnudgeAICLM(ng)) THEN
              cff4 = CLIMA(ng)%AInudgcof(i,j)
            ELSE
              cff4 = 0.0_r8
            END IF
            IF (cff4 == 0.0_r8) THEN
              v_u = MIN(vie(i  ,j,lieol), vie(i  ,j-1,lieol),           &
     &                  vie(i-1,j,lieol), vie(i-1,j-1,lieol))
              speed = SQRT(uie(i,j,lieol)**2 + v_u**2)
              udrag = lf_k2(ng)/(speed + lf_u0(ng))*h_loadu(i,j)
              alfa = alfa + udrag
            END IF
          END IF
# endif
!
!  Solving the momentum equation for u
!
          uie(i,j,lienw) = (masu*uie(i,j,lieol) + dte(ng)*uforce)/alfa
!
          IF (LnudgeMICLM(ng)) THEN
            cff = 0.5*(CLIMA(ng)%MInudgcof(i,j) +                       &
     &                 CLIMA(ng)%MInudgcof(i-1,j))
            uie(i,j,lienw)=uie(i,j,lienw)+                              &
     &        dte(ng)*cff*(CLIMA(ng)%uiclm(i,j)-uie(i,j,lienw))
          END IF
# ifdef ICE_SHOREFAST
          hu = 0.5_r8*(h(i-1,j)+MIN(Zt_avg1(i-1,j),0.0_r8) +            &
     &                 h(i  ,j)+MIN(Zt_avg1(i  ,j),0.0_r8))
          masu = 0.5_r8*(hi(i,j,liold)+hi(i-1,j,liold))
          clear = hu-0.9_r8*masu
          clear = MAX(clear,0.0_r8)
          IF (clear.lt.5.0_r8) uie(i,j,lienw) =                         &
     &      MAX(clear-1.0_r8,0.0_r8)/4.0_r8*uie(i,j,lienw)
# endif
# ifdef ICE_LANDFAST_CLM
          uie(i,j,lienw) = 0.5*(fastice_clm(i,j)+fastice_clm(i-1,j))    &
     &                     *uie(i,j,lienw)
# endif
# ifdef MASKING
          uie(i,j,lienw) = umask(i,j)*uie(i,j,lienw)
# endif
# ifdef ICESHELF
          IF ((zice(i-1,j)+zice(i,j)).ne.0.0_r8) THEN
            uie(i,j,lienw) = 0.0_r8
          END IF
# endif
        END DO
      END DO
!
! *** momentum equation
!     v-eqn
!
      DO j=JstrP,Jend
        DO i=Istr,Iend
          vforce = 0._r8
          masv = 0.5_r8*(hi(i,j,liold)+hi(i,j-1,liold))
          masv = MAX(masv,0.1_r8)
          masv = masv*rhoice(ng)
          avf = MAX(0.5_r8*(ai(i,j-1,liold)+ai(i,j,liold)),0.01_r8)
          chuy = 0.5_r8*(chu_iw(i,j-1)+chu_iw(i,j))
          pmv = 0.5_r8*(pm(i,j) + pm(i,j-1))
          pnv = 0.5_r8*(pn(i,j) + pn(i,j-1))
!
! *** forces from ice rheology y-direction
!
          s1 = (sig22(i,j,lienw) - sig22(i,j-1,lienw))*pnv
# ifdef MASKING
          IF (vmask(i,j).gt.0.0_r8.and.vmask(i+1,j).lt.1.0_r8) THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
#  ifdef WET_DRY
          ELSE IF (vmask_wet(i  ,j).gt.0.0_r8.and.                      &
     &             vmask_wet(i+1,j).lt.1.0_r8) THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
#  endif
#  ifdef ICESHELF
          ELSE IF ( zice(i  ,j-1)             .eq.0.0_r8 .and.          &
     &              zice(i  ,j  )             .eq.0.0_r8 .and.          &
     &             (zice(i+1,j-1)+zice(i+1,j)).ne.0.0_r8) then
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
#  endif
          ELSE
            f1 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i+1,j  ,lienw) +   &
     &                    sig12(i+1,j-1,lienw)+sig12(i  ,j-1,lienw))
          END IF
# elif defined WET_DRY
          IF (vmask_wet(i  ,j).gt.0.0_r8.and.                           &
     &        vmask_wet(i+1,j).lt.1.0_r8) THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
#  ifdef ICESHELF
          ELSE IF ( zice(i  ,j-1)             .eq.0.0_r8 .and.          &
     &              zice(i  ,j  )             .eq.0.0_r8 .and.          &
     &             (zice(i+1,j-1)+zice(i+1,j)).ne.0.0_r8) THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
#  endif
          ELSE
            f1 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i+1,j  ,lienw) +   &
     &                    sig12(i+1,j-1,lienw)+sig12(i  ,j-1,lienw))
          END IF
# else
#  ifdef ICESHELF
          IF ( zice(i  ,j-1)             .eq.0.0_r8 .and.               &
     &         zice(i  ,j  )             .eq.0.0_r8 .and.               &
     &        (zice(i+1,j-1)+zice(i+1,j)).ne.0.0_r8) THEN
            f1 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
          ELSE
            f1 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i+1,j  ,lienw) +   &
     &                    sig12(i+1,j-1,lienw)+sig12(i  ,j-1,lienw))
          END IF
#  else
          f1 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i+1,j  ,lienw) +     &
     &                  sig12(i+1,j-1,lienw)+sig12(i  ,j-1,lienw))
#  endif
# endif

# ifdef MASKING
          IF (vmask(i,j).gt.0.0_r8.and.vmask(i-1,j).lt.1.0_r8 ) THEN
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
#  ifdef WET_DRY
          ELSE IF (vmask_wet(i  ,j).gt.0.0_r8.and.                      &
     &             vmask_wet(i-1,j).lt.1.0_r8 ) THEN
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
#  endif
#  ifdef ICESHELF
          ELSE IF ( zice(i  ,j-1)             .eq.0.0_r8 .and.          &
     &              zice(i  ,j  )             .eq.0.0_r8 .and.          &
     &             (zice(i-1,j-1)+zice(i-1,j)).ne.0.0_r8) then
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
#  endif
          ELSE
            f2 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i-1,j  ,lienw) +   &
     &                    sig12(i-1,j-1,lienw)+sig12(i  ,j-1,lienw))
          END IF
# elif defined WET_DRY
          IF (vmask_wet(i  ,j).gt.0.0_r8.and.                           &
     &        vmask_wet(i-1,j).lt.1.0_r8 ) THEN
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
#  ifdef ICESHELF
          ELSE IF ( zice(i  ,j-1)             .eq.0.0_r8 .and.          &
     &              zice(i  ,j  )             .eq.0.0_r8 .and.          &
     &             (zice(i-1,j-1)+zice(i-1,j)).ne.0.0_r8) then
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
#  endif
          ELSE
            f2 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i-1,j  ,lienw) +   &
     &                    sig12(i-1,j-1,lienw)+sig12(i  ,j-1,lienw))
          END IF
# else
#  ifdef ICESHELF
          IF ( zice(i  ,j-1)             .eq.0.0_r8 .and.               &
     &         zice(i  ,j  )             .eq.0.0_r8 .and.               &
     &        (zice(i-1,j-1)+zice(i-1,j)).ne.0.0_r8) then
            f2 = 0.5_r8*(sig12(i,j,lienw)+sig12(i,j-1,lienw))
          ELSE
            f2 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i-1,j  ,lienw) +   &
     &                    sig12(i-1,j-1,lienw)+sig12(i  ,j-1,lienw))
          END IF
#  else
          f2 = 0.25_r8*(sig12(i  ,j  ,lienw)+sig12(i-1,j  ,lienw) +     &
     &                  sig12(i-1,j-1,lienw)+sig12(i  ,j-1,lienw))
#  endif
# endif
          s2 = (f1-f2)*pmv
          vforce = s1+s2
!
! *** wind force
!
          vforce = vforce + tauaiv(i,j)
!
! *** pressure from tilting ocean surface
!
          vforce = vforce - g*(masv)*pnv*(sealev(i,j)-sealev(i,j-1))
!
          fakt = 0.0_r8
# ifdef MASKING
#  ifdef WET_DRY
          dsum = umask(i  ,j-1)*umask_wet(i  ,j-1) +                    &
     &           umask(i+1,j-1)*umask_wet(i+1,j-1) +                    &
     &           umask(i  ,j  )*umask_wet(i  ,j  ) +                    &
     &           umask(i+1,j  )*umask_wet(i  ,j  )
          IF (dsum.gt.0.0_r8) fakt = 1.0_r8/dsum
          mfu11 = 0.5_r8*(hi(i-1,j-1,liold)*f(i-1,j-1)+                 &
     &                    hi(i  ,j-1,liold)*f(i  ,j-1))*                &
     &                   uie(i  ,j-1,lieol)*                            &
     &                   umask(i  ,j-1)*umask_wet(i  ,j-1)
          mfu21 = 0.5_r8*(hi(i  ,j-1,liold)*f(i  ,j-1)+                 &
     &                    hi(i+1,j-1,liold)*f(i+1,j-1))*                &
     &                   uie(i+1,j-1,lieol)*                            &
     &                   umask(i+1,j-1)*umask_wet(i+1,j-1)
          mfu12 = 0.5_r8*(hi(i-1,j  ,liold)*f(i-1,j  )+                 &
     &                    hi(i  ,j  ,liold)*f(i  ,j  ))*                &
     &                   uie(i  ,j  ,lieol)*                            &
     &                   umask(i  ,j  )*umask_wet(i  ,j  )
          mfu22 = 0.5_r8*(hi(i  ,j  ,liold)*f(i  ,j  )+                 &
     &                    hi(i+1,j  ,liold)*f(i+1,j  ))*                &
     &                   uie(i+1,j  ,lieol)*                            &
     &                   umask(i+1,j  )*umask_wet(i+1,j  )
#  else
          dsum = umask(i,j-1)+umask(i+1,j-1)+umask(i,j)+umask(i+1,j)
          IF (dsum.gt.0.0_r8) fakt = 1.0_r8/dsum
          mfu11 = 0.5_r8*(hi(i-1,j-1,liold)*f(i-1,j-1)+                 &
     &                    hi(i  ,j-1,liold)*f(i  ,j-1))*                &
     &                   uie(i  ,j-1,lieol)*umask(i  ,j-1)
          mfu21 = 0.5_r8*(hi(i  ,j-1,liold)*f(i  ,j-1)+                 &
     &                    hi(i+1,j-1,liold)*f(i+1,j-1))*                &
     &                   uie(i+1,j-1,lieol)*umask(i+1,j-1)
          mfu12 = 0.5_r8*(hi(i-1,j  ,liold)*f(i-1,j  )+                 &
     &                    hi(i  ,j  ,liold)*f(i  ,j  ))*                &
     &                   uie(i  ,j  ,lieol)*umask(i  ,j  )
          mfu22 = 0.5_r8*(hi(i  ,j  ,liold)*f(i  ,j  )+                 &
     &                    hi(i+1,j  ,liold)*f(i+1,j  ))*                &
     &                   uie(i+1,j  ,lieol)*umask(i+1,j  )
#  endif
# elif defined WET_DRY
          dsum = umask_wet(i,j-1)+umask_wet(i+1,j-1)+                   &
     &           umask_wet(i,j  )+umask_wet(i+1,j  )
          IF (dsum.gt.0.0_r8) fakt = 1.0_r8/dsum
          mfu11 = 0.5_r8*(hi(i-1,j-1,liold)*f(i-1,j-1)+                 &
     &                    hi(i  ,j-1,liold)*f(i  ,j-1))*                &
     &                   uie(i  ,j-1,lieol)*umask_wet(i  ,j-1)
          mfu21 = 0.5_r8*(hi(i  ,j-1,liold)*f(i  ,j-1)+                 &
     &                    hi(i+1,j-1,liold)*f(i+1,j-1))*                &
     &                   uie(i+1,j-1,lieol)*umask_wet(i+1,j-1)
          mfu12 = 0.5_r8*(hi(i-1,j  ,liold)*f(i-1,j)+                   &
     &                    hi(i  ,j  ,liold)*f(i  ,j))*                  &
     &                   uie(i  ,j  ,lieol)*umask_wet(i  ,j  )
          mfu22 = 0.5_r8*(hi(i  ,j  ,liold)*f(i  ,j)+                   &
     &                    hi(i+1,j  ,liold)*f(i+1,j))*                  &
     &                   uie(i+1,j  ,lieol)*umask_wet(i+1,j  )
# else
          fakt = 0.25_r8
          mfu11 = 0.5_r8*(hi(i-1,j-1,liold)*f(i-1,j-1)+                 &
     &                    hi(i  ,j-1,liold)*f(i  ,j-1))*                &
     &                   uie(i  ,j-1,lieol)
          mfu21 = 0.5_r8*(hi(i  ,j-1,liold)*f(i  ,j-1)+                 &
     &                    hi(i+1,j-1,liold)*f(i+1,j-1))*                &
     &                   uie(i+1,j-1,lieol)
          mfu12 = 0.5_r8*(hi(i-1,j  ,liold)*f(i-1,j  )+                 &
     &                    hi(i  ,j  ,liold)*f(i  ,j  ))*                &
     &                   uie(i  ,j  ,lieol)
          mfu22 = 0.5_r8*(hi(i  ,j  ,liold)*f(i  ,j  )+                 &
     &                    hi(i+1,j  ,liold)*f(i+1,j  ))*                &
     &                   uie(i+1,j  ,lieol)
# endif
!
! *** coriolis force v
!
          vforce = vforce - fakt*rhoice(ng)*                            &
     &                      (mfu11 + mfu21 + mfu12 + mfu22)
!
! *** stress from ocean current
!
          vforce = vforce + avf*chuy*rho0*vwater(i,j)
!
          alfa = masv + dte(ng)*avf*rho0*chuy
# ifdef ICE_LANDFAST
!
! *** stress on bottom of keels
!
          IF (max(ai(i,j-1,liold),ai(i,j,liold)) > 0.01_r8) THEN
!
!  Don't have landfast ice in nudging band
!
            IF (LnudgeAICLM(ng)) THEN
              cff4 = CLIMA(ng)%AInudgcof(i,j)
            ELSE
              cff4 = 0.0_r8
            END IF
            IF (cff4 == 0.0_r8) THEN
              u_v = MIN(uie(i  ,j,lieol), uie(i  ,j-1,lieol),           &
     &                  uie(i-1,j,lieol), uie(i-1,j-1,lieol))
              speed = SQRT(vie(i,j,lieol)**2 + u_v**2)
              vdrag = (lf_k2(ng)/(speed + lf_u0(ng)))*h_loadv(i,j)
              alfa = alfa + vdrag
            END IF
          END IF
# endif
!
!  Solving the momentum equation for v
!
          vie(i,j,lienw) = (masv*vie(i,j,lieol) + dte(ng)*vforce)/alfa
!
          IF (LnudgeMICLM(ng)) THEN
            cff = 0.5*(CLIMA(ng)%MInudgcof(i,j)+                        &
     &                 CLIMA(ng)%MInudgcof(i,j-1))
            vie(i,j,lienw)=vie(i,j,lienw)+                              &
     &        dte(ng)*cff*(CLIMA(ng)%viclm(i,j)-vie(i,j,lienw))
          END IF
# ifdef ICE_SHOREFAST
          hv = 0.5_r8*(h(i,j-1)+MIN(Zt_avg1(i,j-1),0.0_r8) +            &
     &                 h(i,j  )+MIN(Zt_avg1(i,j  ),0.0_r8))
          masv = 0.5_r8*(hi(i,j,liold)+hi(i,j-1,liold))
          clear = hv-0.9_r8*masv
          clear = MAX(clear,0.0_r8)
          IF (clear.lt.5.0_r8) vie(i,j,lienw) =                         &
     &      MAX(clear-1.0_r8,0.0_r8)/4.0_r8*vie(i,j,lienw)
# endif
# ifdef ICE_LANDFAST_CLM
          vie(i,j,lienw) = 0.5*(fastice_clm(i,j)+fastice_clm(i,j-1))    &
     &                     *vie(i,j,lienw)
# endif
# ifdef MASKING
          vie(i,j,lienw) = vmask(i,j)*vie(i,j,lienw)
# endif
# ifdef ICESHELF
          IF ((zice(i,j-1)+zice(i,j)).ne.0.0_r8) THEN
            vie(i,j,lienw) = 0.0_r8
          END IF
# endif
        END DO
      END DO

      CALL uibc_tile (ng, tile,                                         &
     &                LBi, UBi, LBj, UBj,                               &
     &                IminS, ImaxS, JminS, JmaxS,                       &
     &                lieol, lienw, uie)
      CALL vibc_tile (ng, tile,                                         &
     &                LBi, UBi, LBj, UBj,                               &
     &                IminS, ImaxS, JminS, JmaxS,                       &
     &                lieol, lienw, vie)
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          uie(:,:,lienw))
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          vie(:,:,lienw))
      END IF
# ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    uie(:,:,lienw), vie(:,:,lienw))
# endif
!
! Update ice velocity
!
      IF(ievp(ng).eq.nevp(ng)) THEN
        DO j=Jstr,Jend
          DO i=IstrP,Iend
            ui(i,j,liunw) = uie(i,j,lienw)
          END DO
        END DO
        DO j=JstrP,Jend
          DO i=Istr,Iend
            vi(i,j,liunw) = vie(i,j,lienw)
          END DO
        END DO
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=Jstr,Jend
            ui(1,j,liunw) = uie(1,j,lienw)
          END DO
          DO j=JstrP,Jend
            vi(0,j,liunw) = vie(0,j,lienw)
          END DO
        END IF
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=Jstr,Jend
            ui(Lm(ng)+1,j,liunw) = uie(Lm(ng)+1,j,lienw)
          END DO
          DO j=JstrP,Jend
            vi(Lm(ng)+1,j,liunw) = vie(Lm(ng)+1,j,lienw)
          END DO
        END IF
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=IstrP,Iend
            ui(i,0,liunw) = uie(i,0,lienw)
          END DO
          DO i=Istr,Iend
            vi(i,1,liunw) = vie(i,1,lienw)
          END DO
        END IF
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=IstrP,Iend
            ui(i,Mm(ng)+1,liunw) = uie(i,Mm(ng)+1,lienw)
          END DO
          DO i=Istr,Iend
            vi(i,Mm(ng)+1,liunw) = vie(i,Mm(ng)+1,lienw)
          END DO
        END IF
        IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
          ui(1,0,liunw) = uie(1,0,lienw)
          vi(0,1,liunw) = vie(0,1,lienw)
        END IF
        IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          ui(Lm(ng)+1,0,liunw) = uie(Lm(ng)+1,0,lienw)
          vi(Lm(ng)+1,1,liunw) = vie(Lm(ng)+1,1,lienw)
        END IF
        IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          ui(1,Mm(ng)+1,liunw) = uie(1,Mm(ng)+1,lienw)
          vi(0,Mm(ng)+1,liunw) = vie(0,Mm(ng)+1,lienw)
        END IF
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          ui(Lm(ng)+1,Mm(ng)+1,liunw) = uie(Lm(ng)+1,Mm(ng)+1,lienw)
          vi(Lm(ng)+1,Mm(ng)+1,liunw) = vie(Lm(ng)+1,Mm(ng)+1,lienw)
        END IF

        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_u2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            ui(:,:,liunw))
          CALL exchange_v2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            vi(:,:,liunw))
        END IF
# ifdef DISTRIBUTE
        CALL mp_exchange2d (ng, tile, iNLM, 2,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints,                               &
     &                      EWperiodic(ng), NSperiodic(ng),             &
     &                      ui(:,:,liunw), vi(:,:,liunw))
# endif
      END IF
!
      RETURN
      END SUBROUTINE ice_elastic_tile
#endif
      END MODULE ice_elastic_mod
