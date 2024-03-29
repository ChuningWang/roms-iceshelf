#include "cppdefs.h"
      MODULE ice_evp_mod
#ifdef ICE_EVP
!
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2022 The ROMS/TOMS Group          Chuning Wang   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the parameters for elastic-viscous-plastic    !
!  rheology.                                                           !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC ice_evp

      CONTAINS

      SUBROUTINE ice_evp (ng, tile)

      USE mod_param
      USE mod_grid
      USE mod_ice
      USE mod_stepping
!
      integer, intent(in) :: ng, tile
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 93, __LINE__, __FILE__)
# endif
!
      CALL ice_evp_tile (ng, tile,                                      &
     &                   LBi, UBi, LBj, UBj,                            &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   liold(ng), lieol(ng), liuol(ng),               &
     &                   GRID(ng) % pm,                                 &
     &                   GRID(ng) % pn,                                 &
# ifdef ICE_OUTFLOW_MASK
     &                   GRID(ng) % mask_outflow,                       &
# endif
# ifdef ICE_STRENGTH_QUAD
     &                   ICE(ng) % pstar_grid,                          &
# endif
     &                   ICE(ng) % ai,                                  &
     &                   ICE(ng) % hi,                                  &
     &                   ICE(ng) % ui,                                  &
     &                   ICE(ng) % vi,                                  &
     &                   ICE(ng) % uie,                                 &
     &                   ICE(ng) % vie,                                 &
     &                   ICE(ng) % pice,                                &
     &                   ICE(ng) % zetai,                               &
     &                   ICE(ng) % eta                                  &
     &                   )
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 93, __LINE__, __FILE__)
# endif
      RETURN
      END SUBROUTINE ice_evp
!
!***********************************************************************
      SUBROUTINE ice_evp_tile (ng, tile,                                &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        liold, lieol, liuol,                      &
     &                        pm, pn,                                   &
# ifdef ICE_OUTFLOW_MASK
     &                        mask_outflow,                             &
# endif
#ifdef ICE_STRENGTH_QUAD
     &                        pstar_grid,                               &
#endif
     &                        ai, hi, ui, vi, uie, vie,                 &
     &                        pice, zetai, eta)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
      USE bc_2d_mod, ONLY : bc_r2d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
# endif
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: liold, lieol, liuol
# ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
#  ifdef ICE_OUTFLOW_MASK
      real(r8), intent(in) :: mask_outflow(LBi:,LBj:)
#  endif
#  ifdef ICE_STRENGTH_QUAD
      real(r8), intent(in) :: pstar_grid(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: ai(LBi:,LBj:,:)
      real(r8), intent(in) :: hi(LBi:,LBj:,:)
      real(r8), intent(in) :: ui(LBi:,LBj:,:)
      real(r8), intent(in) :: vi(LBi:,LBj:,:)
      real(r8), intent(inout) :: uie(LBi:,LBj:,:)
      real(r8), intent(inout) :: vie(LBi:,LBj:,:)
      real(r8), intent(out) :: pice(LBi:,LBj:)
      real(r8), intent(out) :: zetai(LBi:,LBj:)
      real(r8), intent(out) :: eta(LBi:,LBj:)
# else
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
#  ifdef ICE_OUTFLOW_MASK
      real(r8), intent(in) :: mask_outflow(LBi:UBi,LBj:UBj)
#  endif
#  ifdef ICE_STRENGTH_QUAD
      real(r8), intent(in) :: pstar_grid(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: ai(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: hi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: ui(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: vi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: uie(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: vie(LBi:UBi,LBj:UBj,2)
      real(r8), intent(out) :: pice(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: zetai(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: eta(LBi:UBi,LBj:UBj)
# endif
!
! Local variable definitions
!
      integer :: i, j

      real(r8) :: eone, etwos, epx, epy, e2r, eps11, eps22, eps12
      real(r8) :: delta, zmax
      real(r8), parameter :: epso = 1.E-12_r8

# include "set_bounds.h"
!
! ----------------------------------------------------------------------
!   Update ice velocity
! ----------------------------------------------------------------------
!
      IF (ievp(ng).eq.1) THEN
        DO j=Jstr,Jend
          DO i=IstrP,Iend
            uie(i,j,1) = ui(i,j,liuol)
            uie(i,j,2) = ui(i,j,liuol)
          END DO
        END DO
        DO j=JstrP,Jend
          DO i=Istr,Iend
             vie(i,j,1) = vi(i,j,liuol)
             vie(i,j,2) = vi(i,j,liuol)
          END DO
        END DO
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=Jstr,Jend
             uie(1,j,1) = ui(1,j,liuol)
             uie(1,j,2) = ui(1,j,liuol)
          END DO
          DO j=JstrP,Jend
             vie(0,j,1) = vi(0,j,liuol)
             vie(0,j,2) = vi(0,j,liuol)
          END DO
        END IF
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=Jstr,Jend
             uie(Lm(ng)+1,j,1) = ui(Lm(ng)+1,j,liuol)
             uie(Lm(ng)+1,j,2) = ui(Lm(ng)+1,j,liuol)
          END DO
          DO j=JstrP,Jend
             vie(Lm(ng)+1,j,1) = vi(Lm(ng)+1,j,liuol)
             vie(Lm(ng)+1,j,2) = vi(Lm(ng)+1,j,liuol)
          END DO
        END IF
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=IstrP,Iend
             uie(i,0,1) = ui(i,0,liuol)
             uie(i,0,2) = ui(i,0,liuol)
          END DO
          DO i=Istr,Iend
             vie(i,1,1) = vi(i,1,liuol)
             vie(i,1,2) = vi(i,1,liuol)
          END DO
        END IF
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=IstrP,Iend
             uie(i,Mm(ng)+1,1) = ui(i,Mm(ng)+1,liuol)
             uie(i,Mm(ng)+1,2) = ui(i,Mm(ng)+1,liuol)
          END DO
          DO i=Istr,Iend
             vie(i,Mm(ng)+1,1) = vi(i,Mm(ng)+1,liuol)
             vie(i,Mm(ng)+1,2) = vi(i,Mm(ng)+1,liuol)
          END DO
        END IF
        IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
          uie(1,0,1) = ui(1,0,liuol)
          uie(1,0,2) = ui(1,0,liuol)
          vie(0,1,1) = vi(0,1,liuol)
          vie(0,1,2) = vi(0,1,liuol)
        END IF
        IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          uie(Lm(ng)+1,0,1) = ui(Lm(ng)+1,0,liuol)
          uie(Lm(ng)+1,0,2) = ui(Lm(ng)+1,0,liuol)
          vie(Lm(ng)+1,1,1) = vi(Lm(ng)+1,1,liuol)
          vie(Lm(ng)+1,1,2) = vi(Lm(ng)+1,1,liuol)
        END IF
        IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          uie(1,Mm(ng)+1,1) = ui(1,Mm(ng)+1,liuol)
          uie(1,Mm(ng)+1,2) = ui(1,Mm(ng)+1,liuol)
          vie(0,Mm(ng)+1,1) = vi(0,Mm(ng)+1,liuol)
          vie(0,Mm(ng)+1,2) = vi(0,Mm(ng)+1,liuol)
        END IF
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          uie(Lm(ng)+1,Mm(ng)+1,1) = ui(Lm(ng)+1,Mm(ng)+1,liuol)
          uie(Lm(ng)+1,Mm(ng)+1,2) = ui(Lm(ng)+1,Mm(ng)+1,liuol)
          vie(Lm(ng)+1,Mm(ng)+1,1) = vi(Lm(ng)+1,Mm(ng)+1,liuol)
          vie(Lm(ng)+1,Mm(ng)+1,2) = vi(Lm(ng)+1,Mm(ng)+1,liuol)
        END IF
# ifdef DISTRIBUTE
        CALL mp_exchange2d (ng, tile, iNLM, 2,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints, EWperiodic(ng),               &
     &                      NSperiodic(ng),                             &
     &                      uie(:,:,1), vie(:,:,1))
        CALL mp_exchange2d (ng, tile, iNLM, 2,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints, EWperiodic(ng),               &
     &                      NSperiodic(ng),                             &
     &                      uie(:,:,2), vie(:,:,2))
# endif
      END IF
!
      e2r = 1.0_r8/(ellip_sq(ng))
!
! ----------------------------------------------------------------------
!   Compute strain rates
! ----------------------------------------------------------------------
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
          eone  = eps11+eps22 
          etwos = (eps11-eps22)**2 + 4.0_r8*eps12*eps12
! 
          delta = ABS(eone**2 + e2r*etwos)
          delta = MAX(SQRT(delta), epso)
# ifdef ICE_STRENGTH_QUAD
          pice(i,j) = pstar_grid(i,j)*hi(i,j,liold)**2*                 &
# else
          pice(i,j) = pstar(ng)*hi(i,j,liold)*                          &
# endif
     &                EXP(-astren(ng)*(1.0_r8-ai(i,j,liold)))
          zetai(i,j) = pice(i,j)/(2.0_r8*delta)
          zmax = 2.5E+8_r8*pice(i,j)
          zetai(i,j) = MIN(zetai(i,j),zetamax(ng))
          zetai(i,j) = MAX(zetai(i,j),zetamin(ng))
          eta(i,j) = e2r*zetai(i,j)
        END DO
      END DO

      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  pice)
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  zetai)
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  eta)
# ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 3,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    pice, zetai, eta)
# endif

      DO j=JstrT,JendT
        DO i=IstrT,IendT
# ifdef ICE_OUTFLOW_MASK
          pice(i,j)=pice(i,j)*mask_outflow(i,j)
          zetai(i,j)=zetai(i,j)*mask_outflow(i,j)
          eta(i,j)=eta(i,j)*mask_outflow(i,j)
# endif
        END DO
      END DO
!
      RETURN
      END SUBROUTINE ice_evp_tile
#endif
      END MODULE ice_evp_mod
