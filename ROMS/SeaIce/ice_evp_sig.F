#include "cppdefs.h"
      MODULE ice_evp_sig_mod
#ifdef ICE_EVP
!
!=================================================== W. Paul Budgell ===
!  Copyright (c) 2002-2022 The ROMS/TOMS Group          Chuning Wang   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!================================================== Hernan G. Arango ===
!                                                                      !
!  This routine time steps the EVP stresses term.                      !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC ice_evp_sig

      CONTAINS

      SUBROUTINE ice_evp_sig (ng, tile)

      USE mod_param
      USE mod_grid
      USE mod_ice
      USE mod_stepping

      integer, intent(in) :: ng, tile
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 94, __LINE__, __FILE__)
# endif
!
      CALL ice_evp_sig_tile (ng, tile,                                  &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
     &                       liold(ng), lieol(ng), lienw(ng),           &
     &                       GRID(ng) % pm,                             &
     &                       GRID(ng) % pn,                             &
     &                       ICE(ng) % uie,                             &
     &                       ICE(ng) % vie,                             &
     &                       ICE(ng) % hi,                              &
     &                       ICE(ng) % pice,                            &
     &                       ICE(ng) % zetai,                           &
     &                       ICE(ng) % eta,                             &
     &                       ICE(ng) % sig11,                           &
     &                       ICE(ng) % sig22,                           &
     &                       ICE(ng) % sig12)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 94, __LINE__, __FILE__)
# endif
      RETURN
      END SUBROUTINE ice_evp_sig
!
!***********************************************************************
      SUBROUTINE ice_evp_sig_tile (ng, tile,                            &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        liold, lieol, lienw,                      &
     &                        pm, pn, uie, vie,                         &
     &                        hi, pice, zetai, eta,                     &
     &                        sig11, sig22, sig12)
!***********************************************************************
!

      USE mod_param
      USE mod_ncparam
      USE mod_scalars
      USE mod_clima
!
      USE exchange_2d_mod, ONLY : exchange_r2d_tile
#ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
#endif
!
      USE i2dbc_mod, ONLY : i2dbc_sig
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: liold, lieol, lienw

# ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: uie(LBi:,LBj:,:)
      real(r8), intent(in) :: vie(LBi:,LBj:,:)
      real(r8), intent(in) :: hi(LBi:,LBj:,:)
      real(r8), intent(in) :: pice(LBi:,LBj:)
      real(r8), intent(in) :: zetai(LBi:,LBj:)
      real(r8), intent(in) :: eta(LBi:,LBj:)
      real(r8), intent(inout) :: sig11(LBi:,LBj:,:)
      real(r8), intent(inout) :: sig22(LBi:,LBj:,:)
      real(r8), intent(inout) :: sig12(LBi:,LBj:,:)
# else
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: uie(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: vie(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: hi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: pice(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: zetai(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: eta(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: sig11(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: sig22(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: sig12(LBi:UBi,LBj:UBj,2)
# endif
!
! Local variable definitions
!
      integer :: i, j

      real(r8) :: e, e0, ep, ee, ees, gamma
      real(r8) :: f1, f2, f3
      real(r8) :: alfa, beta, epx, epy, eps11, eps22, eps12
      real(r8) :: cff

# include "set_bounds.h"

!
! ----------------------------------------------------------------------
!   Initial value for Youngs modulus (between 0 and 1)
! ----------------------------------------------------------------------
!
      e0=0.25_r8
!
!  Stress tensor
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
          eps11 = (uie(i+1,j,lieol)-uie(i,j,lieol))*pm(i,j)
          eps22 = (vie(i,j+1,lieol)-vie(i,j,lieol))*pn(i,j)
          epx = 0.25_r8*( vie(i+1,j+1,lieol)+vie(i+1,j,lieol) -         &
     &                    vie(i-1,j+1,lieol)-vie(i-1,j,lieol) )*pm(i,j)
          epy = 0.25_r8*( uie(i+1,j+1,lieol)+uie(i,j+1,lieol) -         &
     &                    uie(i+1,j-1,lieol)-uie(i,j-1,lieol) )*pn(i,j)
          eps12 = 0.5_r8*(epx + epy)
!
          IF (hi(i,j,liold).gt.0.01_r8) THEN
            e = 2.0_r8*e0*rhoice(ng)*hi(i,j,liold)/(pm(i,j)*dte(ng))**2
            ep = e*pice(i,j)/(4.0_r8*zetai(i,j)+1.0E-8_r8)
            ee = e/(2.0_r8*eta(i,j)+1.0E-8_r8)
            ees= e*(eta(i,j)-zetai(i,j))/                               &
     &             (4.0_r8*eta(i,j)*zetai(i,j)+1.0E-8_r8)
!
            alfa = 1.0_r8/dte(ng) + ee + ees
            beta = ees
            gamma = 1/dte(ng) + ee
            f1 = e*eps11 - ep + sig11(i,j,lieol)/dte(ng)
            f2 = e*eps22 - ep + sig22(i,j,lieol)/dte(ng)
            f3 = e*eps12 + sig12(i,j,lieol)/dte(ng)
            sig11(i,j,lienw) = (alfa*f1 - beta*f2)/(alfa**2 - beta**2)
            sig22(i,j,lienw) = (alfa*f2 - beta*f1)/(alfa**2 - beta**2)
            sig12(i,j,lienw) = f3/gamma
!
          ELSE
!
            sig11(i,j,lienw) = 2.0_r8*eta(i,j)*eps11+                   &
     &                         (zetai(i,j)-eta(i,j))*(eps11+eps22) -    &
     &                         pice(i,j)/2.0_r8
!
            sig22(i,j,lienw) = 2.0_r8*eta(i,j)*eps22+                   &
     &                         (zetai(i,j)-eta(i,j))*(eps11+eps22) -    &
     &                         pice(i,j)/2.0_r8
!
            sig12(i,j,lienw) = 2.0_r8*eta(i,j)*eps12
          END IF
!
          IF (LnudgeSICLM(ng)) THEN
            cff = CLIMA(ng)%SInudgcof(i,j)
            sig11(i,j,lienw) = sig11(i,j,lienw) +                       &
     &                         dtice(ng)*cff*(CLIMA(ng)%sig11clm(i,j) - &
     &                         sig11(i,j,lienw))
            sig22(i,j,lienw) = sig22(i,j,lienw) +                       &
     &                         dtice(ng)*cff*(CLIMA(ng)%sig22clm(i,j) - &
     &                         sig22(i,j,lienw))
            sig12(i,j,lienw) = sig12(i,j,lienw) +                       &
     &                         dtice(ng)*cff*(CLIMA(ng)%sig12clm(i,j) - &
     &                         sig12(i,j,lienw))
          END IF
        END DO
      END DO
!
      CALL i2dbc_sig (ng, tile,                                         &
     &                LBi, UBi, LBj, UBj,                               &
     &                IminS, ImaxS, JminS, JmaxS,                       &
     &                lieol, lienw,                                     &
     &                uie, vie, sig11, sig22, sig12)
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sig11(:,:,lienw))
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sig22(:,:,lienw))
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sig12(:,:,lienw))
      END IF
# ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 3,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    sig11(:,:,lienw), sig22(:,:,lienw),           &
     &                    sig12(:,:,lienw))
# endif
!
      RETURN
      END SUBROUTINE ice_evp_sig_tile
#endif
      END MODULE ice_evp_sig_mod
