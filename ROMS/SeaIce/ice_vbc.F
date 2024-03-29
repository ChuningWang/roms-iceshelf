#include "cppdefs.h"
      MODULE ice_vbc_mod
#ifdef ICE_MODEL
!
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2022 The ROMS/TOMS Group          Chuning Wang   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This module sets the ice-water and ice-air stresses for the ice     !
!    momentum equation.                                                !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: ice_vbc

      CONTAINS
!
!***********************************************************************
      SUBROUTINE ice_vbc (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_grid
      USE mod_forces
      USE mod_ocean
      USE mod_coupling
      USE mod_ice
      USE mod_stepping

      integer, intent(in) :: ng, tile
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 6, __LINE__, __FILE__)
# endif
      CALL ice_vbc_tile (ng, tile,                                      &
     &                   LBi, UBi, LBj, UBj,                            &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   liold(ng), liuol(ng),                          &
     &                   GRID(ng) % z_r,                                &
     &                   GRID(ng) % z_w,                                &
# ifdef WET_DRY
     &                   GRID(ng) % umask_wet,                          &
     &                   GRID(ng) % vmask_wet,                          &
# endif
# ifdef ICESHELF
     &                   GRID(ng) % zice,                               &
# endif
     &                   OCEAN(ng) % rho,                               &
     &                   COUPLING(ng) % Zt_avg1,                        &
     &                   ICE(ng) % ai,                                  &
     &                   ICE(ng) % hi,                                  &
     &                   ICE(ng) % ui,                                  &
     &                   ICE(ng) % vi,                                  &
     &                   ICE(ng) % uwater,                              &
     &                   ICE(ng) % vwater,                              &
     &                   ICE(ng) % spd_iw,                              &
# ifdef ICE_BULK_FLUXES
     &                   FORCES(ng) % sustr_aw,                         &
     &                   FORCES(ng) % svstr_aw,                         &
     &                   FORCES(ng) % taux_ai,                          &
     &                   FORCES(ng) % tauy_ai,                          &
     &                   FORCES(ng) % sustr_iw,                         &
     &                   FORCES(ng) % svstr_iw,                         &
# endif
# ifdef ICE_SHOREFAST
     &                   GRID(ng) % h,                                  &
# elif defined ICE_LANDFAST
     &                   GRID(ng) % h,                                  &
     &                   ICE(ng) % h_loadu,                             &
     &                   ICE(ng) % h_loadv,                             &
# endif
     &                   FORCES(ng) % sustr,                            &
     &                   FORCES(ng) % svstr,                            &
     &                   ICE(ng) % tauaiu,                              &
     &                   ICE(ng) % tauaiv,                              &
     &                   ICE(ng) % sealev,                              &
     &                   ICE(ng) % utau_iw,                             &
     &                   ICE(ng) % chu_iw)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 6, __LINE__, __FILE__)
# endif
      RETURN
      END SUBROUTINE ice_vbc
!
!***********************************************************************
      SUBROUTINE ice_vbc_tile (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         liold, liuol,                            &
     &                         z_r, z_w,                                &
# ifdef WET_DRY
     &                         umask_wet, vmask_wet,                    &
# endif
# ifdef ICESHELF
     &                         zice,                                    &
# endif
     &                         rho,                                     &
     &                         Zt_avg1,                                 &
     &                         ai, hi, ui, vi, uwater, vwater, spd_iw,  &
# ifdef ICE_BULK_FLUXES
     &                         sustr_aw, svstr_aw, taux_ai, tauy_ai,    &
     &                         sustr_iw, svstr_iw,                      &
# endif
# ifdef ICE_SHOREFAST
     &                         h,                                       &
# elif defined ICE_LANDFAST
     &                         h, h_loadu, h_loadv,                     &
# endif
     &                         sustr, svstr, tauaiu, tauaiv, sealev,    &
     &                         utau_iw, chu_iw)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
      USE bc_2d_mod
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: liold, liuol

# ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
#  ifdef WET_DRY
      real(r8), intent(in) :: umask_wet(LBi:,LBj:)
      real(r8), intent(in) :: vmask_wet(LBi:,LBj:)
#  endif
#  ifdef ICESHELF
      real(r8), intent(in) :: zice(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: rho(LBi:,LBj:,:)
      real(r8), intent(in) :: Zt_avg1(LBi:,LBj:)
      real(r8), intent(in) :: ai(LBi:,LBj:,:)
      real(r8), intent(in) :: hi(LBi:,LBj:,:)
      real(r8), intent(in) :: ui(LBi:,LBj:,:)
      real(r8), intent(in) :: vi(LBi:,LBj:,:)
      real(r8), intent(in) :: uwater(LBi:,LBj:)
      real(r8), intent(in) :: vwater(LBi:,LBj:)
      real(r8), intent(in) :: spd_iw(LBi:,LBj:)
#  ifdef ICE_BULK_FLUXES
      real(r8), intent(in) :: sustr_aw(LBi:,LBj:)
      real(r8), intent(in) :: svstr_aw(LBi:,LBj:)
      real(r8), intent(in) :: taux_ai(LBi:,LBj:)
      real(r8), intent(in) :: tauy_ai(LBi:,LBj:)
      real(r8), intent(out) :: sustr_iw(LBi:,LBj:)
      real(r8), intent(out) :: svstr_iw(LBi:,LBj:)
#  endif
#  ifdef ICE_SHOREFAST
      real(r8), intent(in) :: h(LBi:,LBj:)
#  elif defined ICE_LANDFAST
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(out) :: h_loadu(LBi:,LBj:)
      real(r8), intent(out) :: h_loadv(LBi:,LBj:)
#  endif
      real(r8), intent(out) :: sustr(LBi:,LBj:)
      real(r8), intent(out) :: svstr(LBi:,LBj:)
      real(r8), intent(out) :: tauaiu(LBi:,LBj:)
      real(r8), intent(out) :: tauaiv(LBi:,LBj:)
      real(r8), intent(out) :: sealev(LBi:,LBj:)
      real(r8), intent(out) :: utau_iw(LBi:,LBj:)
      real(r8), intent(inout) :: chu_iw(LBi:,LBj:)
# else
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:N(ng))
#  ifdef WET_DRY
      real(r8), intent(in) :: umask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask_wet(LBi:UBi,LBj:UBj)
#  endif
#  ifdef ICESHELF
      real(r8), intent(in) :: zice(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: rho(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: Zt_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: ai(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: hi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: ui(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: vi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: uwater(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vwater(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: spd_iw(LBi:UBi,LBj:UBj)
#  ifdef ICE_BULK_FLUXES
      real(r8), intent(in) :: sustr_aw(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: svstr_aw(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: taux_ai(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: tauy_ai(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: sustr_iw(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: svstr_iw(LBi:UBi,LBj:UBj)
#  endif
#  ifdef ICE_SHOREFAST
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
#  elif defined ICE_LANDFAST
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: h_loadu(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: h_loadv(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(out) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: svstr(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: tauaiu(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: tauaiv(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: sealev(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: utau_iw(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: chu_iw(LBi:UBi,LBj:UBj)
# endif
!
!  Local variable declarations.
!
      integer :: i, j
# ifdef ICE_SHOREFAST
      real(r8) :: clear, hh
# elif defined ICE_LANDFAST
      real(r8) :: h_u, h_v, h_cu, h_cv, h_wu, h_wv
# endif

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: spdiw
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: chuiw
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: utauiw
# ifndef ICE_MK
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Uwind
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Vwind
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: windu
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: windv
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: wind_speed
# endif
      real(r8) :: tauiwu
      real(r8) :: tauiwv
      real(r8) :: aix
      real(r8) :: aiy
      real(r8) :: spd
      real(r8) :: hix
      real(r8) :: hiy
      real(r8) :: chux
      real(r8) :: chuy

      real(r8) :: rhoO
      real(r8) :: dztop
      real(r8) :: thick
      real(r8) :: zdz0
      real(r8) :: z0

      real(r8), parameter :: kappa = 0.4_r8
      real(r8), parameter :: z0ii = 0.02_r8
!
# include "set_bounds.h"
!
!  Input from ocean model / atmosphere data
!
# ifdef ICE_MK
      DO j=Jstr-1,Jend
        DO i=Istr-1,Iend
          rhoO = rho0+rho(i,j,N(ng))
          spd = spd_iw(i,j)
#  ifdef ICE_SHOREFAST
          spd = MAX(spd,0.10_r8)
          clear = h(i,j)+MIN(Zt_avg1(i,j),0.0_r8)-0.9*hi(i,j,liold)
          clear = MAX(clear,0.001_r8)
          IF (clear .lt. 5.0_r8) THEN
            spd = (clear/5.0_r8)*spd
          END IF
#  else
          spd = MAX(spd,0.15_r8)
#  endif
#  ifdef ICE_MOM_BULK
          utauiw(i,j) = spd
          chuiw(i,j) = cdiw(ng)*spd
#  else
          thick = hi(i,j,liold)/MAX(tol,ai(i,j,liold))
          z0 = MIN(MAX(z0ii*thick,0.01_r8),0.1_r8)
          dztop = z_w(i,j,N(ng))-z_r(i,j,N(ng))
          zdz0 = dztop/z0
          if (zdz0 .lt. 3._r8) zdz0 = 3._r8
          utauiw(i,j) = SQRT(chu_iw(i,j)*spd)
          utauiw(i,j) = MAX(utauiw(i,j),1.E-04_r8)
          chuiw(i,j) = kappa*utauiw(i,j)/LOG(zdz0)
#  endif
#  ifdef ICE_SHOREFAST
          hh = h(i,j)+MIN(Zt_avg1(i,j),0.0_r8)
          clear = hh-0.9_r8*hi(i,j,liold)
          clear = MAX(clear,0.0_r8)
          IF (clear.lt.5.0_r8) chuiw(i,j) =                             &
     &       (MAX(clear-1.0_r8,0.0_r8)/4.0_r8)*chuiw(i,j)
#  endif
        END DO
      END DO
!
      DO j=Jstr,Jend
        DO i=IstrP,Iend
          rhoO = 1000._r8 + 0.5_r8*(rho(i,j,N(ng))+rho(i-1,j,N(ng)))
          aix = 0.5_r8*(ai(i,j,liold)+ai(i-1,j,liold))
          chux = 0.5_r8*(chuiw(i,j)+chuiw(i-1,j))
#  ifdef ICE_BULK_FLUXES
          tauaiu(i,j) = 0.5_r8*aix*(taux_ai(i,j)+taux_ai(i-1,j))
#  else
          tauaiu(i,j) = 0.5_r8*aix*rhoice(ng)*(sustr(i,j)+sustr(i-1,j))
#  endif
#  ifdef WET_DRY
          tauaiu(i,j) = tauaiu(i,j)*umask_wet(i,j)
#  endif
#  ifdef ICE_BULK_FLUXES
#   ifdef ICESHELF
          IF ((zice(i,j)*zice(i-1,j)).eq.0.0_r8) THEN
#   endif
          sustr_iw(i,j) = chux*(ui(i,j,liuol)-uwater(i,j))
          sustr(i,j) = aix*sustr_iw(i,j) + (1.0_r8-aix)*sustr_aw(i,j)
#   ifdef WET_DRY
          sustr(i,j) = sustr(i,j)*umask_wet(i,j)
#   endif
#   ifdef ICESHELF
          END IF
#   endif
#  endif
        END DO
      END DO
!
      DO j=JstrP,Jend
        DO i=Istr,Iend
          rhoO = 1000._r8 + 0.5_r8*(rho(i,j,N(ng))+rho(i,j-1,N(ng)))
          aiy = 0.5_r8*(ai(i,j,liold)+ai(i,j-1,liold))
          chuy = 0.5_r8*(chuiw(i,j)+chuiw(i,j-1))
#  ifdef ICE_BULK_FLUXES
          tauaiv(i,j) = 0.5_r8*aiy*(tauy_ai(i,j)+tauy_ai(i,j-1))
#  else
          tauaiv(i,j) = 0.5_r8*aiy*rhoice(ng)*(svstr(i,j)+svstr(i,j-1))
#  endif
#  ifdef WET_DRY
          tauaiv(i,j) = tauaiv(i,j)*vmask_wet(i,j)
#  endif
#  ifdef ICE_BULK_FLUXES
#   ifdef ICESHELF
          IF ((zice(i,j)*zice(i,j-1)).eq.0.0_r8) THEN
#   endif
          svstr_iw(i,j) = chuy*(vi(i,j,liuol)-vwater(i,j))
          svstr(i,j) = aiy*svstr_iw(i,j) + (1.0_r8-aiy)*svstr_aw(i,j)
#   ifdef WET_DRY
          svstr(i,j) = svstr(i,j)*vmask_wet(i,j)
#   endif
#   ifdef ICESHELF
          END IF
#   endif
#  endif
# else
      DO j=Jstr,Jend
        DO i=IstrP,Iend
           windu(i,j) = 0.5_r8*(Uwind(i-1,j)+Uwind(i,j))
        END DO
      END DO
      DO j=JstrP,Jend
        DO i=Istr,Iend
           windv(i,j) = 0.5_r8*(Vwind(i,j-1)+Vwind(i,j))
        END DO
      END DO
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
          spd = spd_iw(i,j)
          spdiw(i,j) = (ai(i,j,liold)+0.02)*rho0*cdiw(ng)*spd/rhoice(ng)
          spdiw(i,j) =                                                  &
     &      MAX(spdiw(i,j),                                             &
     &          (ai(i,j,liold)+0.02_r8)*cdiw(ng)*0.1_r8*rho/rhoice(ng))
           wind_speed(i,j) = 0.5*SQRT((windu(i,j) + windu(i+1,j))**2 +  &
     &                                (windv(i,j) + windv(i,j+1))**2)
#  ifdef ICE_SHOREFAST
          clear = h(i,j)+MIN(Zt_avg1(i,j),0.0_r8)-0.9*hi(i,j,liold)
          clear = MAX(clear,0.001_r8)
          IF (clear .lt. 5.0_r8) THEN
            spdiw(i,j) = (clear/5.0_r8)*spdiw(i,j)
            wind(i,j) = (clear/5.0_r8)*wind(i,j)
          END IF
#  endif
        END DO
      END DO
!
      DO j=Jstr,Jend
        DO i=IstrP,Iend
          rhoO = 1000._r8 + 0.5_r8*(rho(i,j,N(ng))+rho(i-1,j,N(ng)))
          aix = 0.5_r8*(ai(i,j,liold)+ai(i-1,j,liold))
          hix = 0.5_r8*(hi(i,j,liold)+hi(i-1,j,liold))
          spd = 0.5_r8*(wind_speed(i,j)+wind_speed(i-1,j))
          tauaiu(i,j) = aix*rho_air(ng)*                                &
     &      (0.5_r8*cdai(ng)*                                           &
     &       (1.0_r8-                                                   &
     &        COS(2.0_r8*pi*MIN((hix/(aix+0.02_r8)+0.1_r8),0.5_r8))))*  &
     &      spd*windu(i,j)/rhoice(ng)
          tauiwu = 0.5_r8*(spdiw(i,j)+spdiw(i-1,j)) *                   &
     &             (ui(i,j,liuol)-uwater(i,j))*rhoice(ng)/rhoO
          sustr_iw(i,j) = tauiwu
#  ifdef ICE_BULK_FLUXES
#   ifdef ICESHELF
          IF ((zice(i,j)*zice(i-1,j)).eq.0.0_r8) THEN
#   endif
          sustr(i,j) = aix*tauiwu + (1.0_r8-aix)*0.5_r8*                &
     &                              (sustr_aw(i,j)+sustr_aw(i-1,j))
#   ifdef WET_DRY
          sustr(i,j) = sustr(i,j)*umask_wet(i,j)
#   endif
#   ifdef ICESHELF
          END IF
#   endif
#  endif
        END DO
      END DO
!
      DO j=JstrP,Jend
        DO i=Istr,Iend
          rhoO = 1000._r8 + 0.5_r8*(rho(i,j,N(ng))+rho(i,j-1,N(ng)))
          aiy = 0.5_r8*(ai(i,j,liold)+ai(i,j-1,liold))
          hiy = 0.5_r8*(hi(i,j,liold)+hi(i,j-1,liold))
          spd = 0.5_r8*(wind_speed(i,j)+wind_speed(i,j-1))
          tauaiv(i,j) = aiy*rho_air(ng)*                                &
     &      (0.5_r8*cdai(ng)*                                           &
     &       (1.0_r8-                                                   &
     &        COS(2.0_r8*pi*MIN((hiy/(aiy+0.02_r8)+0.1_r8),0.5_r8))))*  &
     &      spd*windv(i,j)/rhoice(ng)
          tauiwv = 0.5_r8*(spdiw(i,j)+spdiw(i,j-1)) *                   &
     &             (vi(i,j,liuol)-vwater(i,j))*rhoice(ng)/rhoO
          svstr_iw(i,j) = tauiwv
#  ifdef ICE_BULK_FLUXES
#   ifdef ICESHELF
          IF ((zice(i,j)*zice(i,j-1)).eq.0.0_r8) THEN
#   endif
          svstr(i,j) = aiy*tauiwv + (1.0_r8-aiy)*0.5_r8*                &
     &                              (svstr_aw(i,j)+svstr_aw(i,j-1))
#   ifdef WET_DRY
          svstr(i,j) = svstr(i,j)*vmask_wet(i,j)
#   endif
#   ifdef ICESHELF
          END IF
#   endif
#  endif
        END DO
      END DO
# endif
        END DO
      END DO
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
           sealev(i,j) = Zt_avg1(i,j)
           chu_iw(i,j) = chuiw(i,j)
           utau_iw(i,j) = utauiw(i,j)
        END DO
      END DO
# ifdef ICE_LANDFAST
!
! Compute the term (h_u - h_cu) * exp(-C(1-A_u)) (at u points)
!
      DO j=Jstr,Jend
        DO i=IstrP,Iend
          aix = MAX(ai(i,j,liold), ai(i-1,j,liold))
          IF (aix > 0.01) THEN
            h_u = MAX(hi(i,j,liold), hi(i-1,j,liold))
            h_wu = MIN((h(i,j)+Zt_avg1(i,j)), (h(i-1,j)+Zt_avg1(i-1,j)))
            h_cu = h_wu * aix / lf_k1(ng)
            h_loadu(i,j) = MAX(0._r8,                                   &
     &                         (h_u-h_cu)*EXP(-astren(ng)*(1.0_r8-aix)))
          ELSE
            h_loadu(i,j) = 0._r8
          END IF
        END DO
      END DO
!
! Compute the term (h_v - h_cv) * exp(-C(1-A_v)) (at v points)
!
      DO j=JstrP,Jend
        DO i=Istr,Iend
          aiy = MAX(ai(i,j,liold), ai(i,j-1,liold))
          IF (aiy > 0.01) THEN
            h_v = MAX(hi(i,j,liold), hi(i,j-1,liold))
            h_wv = MIN((h(i,j)+Zt_avg1(i,j)), (h(i,j-1)+Zt_avg1(i,j-1)))
            h_cv = h_wv * aiy / lf_k1(ng)
            h_loadv(i,j) = MAX(0._r8,                                   &
     &                         (h_v-h_cv)*EXP(-astren(ng)*(1.0_r8-aiy)))
          ELSE
            h_loadv(i,j) = 0._r8
          END IF
        END DO
      END DO
# endif
!
!  Apply boundary conditions.
!
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  sealev)
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  utau_iw)
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  chu_iw)
# ifdef ICE_BULK_FLUXES
      CALL bc_u2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  sustr)
      CALL bc_v2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  svstr)
      CALL bc_u2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  tauaiu)
      CALL bc_v2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  tauaiv)
      CALL bc_u2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  sustr_iw)
      CALL bc_v2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  svstr_iw)
# endif
# ifdef ICE_LANDFAST
      CALL bc_u2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  h_loadu)
      CALL bc_v2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  h_loadv)
# endif
# ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 3,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    sealev, utau_iw, chu_iw)
#  ifdef ICE_BULK_FLUXES
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    sustr, svstr, tauaiu, tauaiv)
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    sustr_iw, svstr_iw)
#  endif
#  ifdef ICE_LANDFAST
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    h_loadu, h_loadv)
#  endif
# endif
!
      RETURN
      END SUBROUTINE ice_vbc_tile
#endif
      END MODULE ice_vbc_mod
