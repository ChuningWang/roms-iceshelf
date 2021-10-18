#include "cppdefs.h"
      MODULE tibc_mod
#ifdef ICE_MODEL
! 
!=======================================================================
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                           Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This subroutine computes lateral boundary conditions for internal   !
!  ice temperature.                                                    !
!                                                                      !
!=======================================================================
!

      implicit none

      PRIVATE
      PUBLIC tibc_tile

      CONTAINS
!
!***********************************************************************
      SUBROUTINE tibc (ng, tile, model)
!***********************************************************************
!
      USE mod_param
      USE mod_ice
      USE mod_stepping
      USE mod_scalars
!
      integer, intent(in) :: ng, tile, model

# include "tile.h"
!
      CALL tibc_tile (ng, tile, model,                                  &
     &                LBi, UBi, LBj, UBj,                               &
     &                liold(ng), linew(ng),                             &
     &                ICE(ng) % ui,                                     &
     &                ICE(ng) % vi,                                     &
     &                ICE(ng) % hi,                                     &
     &                ICE(ng) % ti,                                     &
     &                ICE(ng) % enthalpi)
      RETURN
      END SUBROUTINE tibc

!
!***********************************************************************
      SUBROUTINE tibc_tile (ng, tile, model,                            &
     &                           LBi, UBi, LBj, UBj,                    &
     &                           liold, linew,                          &
     &                           ui, vi, hi, ti, enthalpi)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
      USE mod_scalars
      USE mod_boundary
      USE mod_grid

      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: liold, linew

# ifdef ASSUMED_SHAPE
      real(r8), intent(in)    :: ui(LBi:,LBj:,:)
      real(r8), intent(in)    :: vi(LBi:,LBj:,:)
      real(r8), intent(in)    :: hi(LBi:,LBj:,:)
      real(r8), intent(inout) :: ti(LBi:,LBj:,:)
      real(r8), intent(inout) :: enthalpi(LBi:,LBj:,:)
# else
      real(r8), intent(in)    :: ui(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in)    :: vi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in)    :: hi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: ti(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: enthalpi(LBi:UBi,LBj:UBj,2)
# endif

!
!  Local variable declarations.
!
      integer :: i, j, know

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Set time-indices
!-----------------------------------------------------------------------
!
        know=liold
!
!-----------------------------------------------------------------------
!  Lateral boundary conditions at the western edge.
!-----------------------------------------------------------------------
!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
!
!  Western edge, clamped boundary condition.
!
        IF (LBC(iwest,isTice,ng)%clamped) THEN
          DO j=Jstr,Jend
            IF (LBC_apply(ng)%west(j)) THEN
              enthalpi(Istr-1,j,linew)=BOUNDARY(ng)%hi_west(j)*         &
     &                                 BOUNDARY(ng)%ti_west(j)
              ti(Istr-1,j,linew)=BOUNDARY(ng)%ti_west(j)
# ifdef MASKING
              enthalpi(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)*        &
     &                                 GRID(ng)%rmask(Istr-1,j)
              ti(Istr-1,j,linew)=ti(Istr-1,j,linew)*                    &
     &                           GRID(ng)%rmask(Istr-1,j)
# endif
# ifdef WET_DRY
              enthalpi(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)*        &
     &                                 GRID(ng)%rmask_wet(Istr-1,j)
              ti(Istr-1,j,linew)=ti(Istr-1,j,linew)*                    &
     &                           GRID(ng)%rmask_wet(Istr-1,j)
# endif
            END IF
          END DO
!
!  Western edge, clamped on inflow, gradient on outflow.
!
        ELSE IF (LBC(iwest,isTice,ng)%mixed) THEN
          DO j=Jstr,Jend
            IF (LBC_apply(ng)%west(j)) THEN
              IF (ui(Istr,j,linew).ge.0._r8) THEN
                enthalpi(Istr-1,j,linew)=BOUNDARY(ng)%hi_west(j)*       &
     &                                   BOUNDARY(ng)%ti_west(j)
                ti(Istr-1,j,linew)=BOUNDARY(ng)%ti_west(j)
              ELSE
                enthalpi(Istr-1,j,linew)=enthalpi(Istr,j,linew)
                ti(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)/            &
     &                             MAX(hi(Istr-1,j,linew),1.0E-6_r8)
              END IF
# ifdef MASKING
              enthalpi(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)*        &
     &                                 GRID(ng)%rmask(Istr-1,j)
              ti(Istr-1,j,linew)=ti(Istr-1,j,linew)*                    &
     &                           GRID(ng)%rmask(Istr-1,j)
# endif
# ifdef WET_DRY
              enthalpi(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)*        &
     &                                 GRID(ng)%rmask_wet(Istr-1,j)
              ti(Istr-1,j,linew)=ti(Istr-1,j,linew)*                    &
     &                           GRID(ng)%rmask_wet(Istr-1,j)
# endif
            END IF
          END DO
!
!  Western edge, gradient boundary condition.
!
        ELSE IF (LBC(iwest,isTice,ng)%gradient) THEN
          DO j=Jstr,Jend
            IF (LBC_apply(ng)%west(j)) THEN
              enthalpi(Istr-1,j,linew)=enthalpi(Istr,j,linew)
# ifdef MASKING
              enthalpi(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)*        &
     &                                 GRID(ng)%rmask(Istr-1,j)
# endif
# ifdef WET_DRY
              enthalpi(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)*        &
     &                                 GRID(ng)%rmask_wet(Istr-1,j)
# endif
              ti(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)/              &
     &                           MAX(hi(Istr-1,j,linew),1.0E-6_r8)
            END IF
          END DO
!
!  Western edge, closed boundary condition.
!
        ELSE IF (LBC(iwest,isTice,ng)%closed) THEN
          DO j=Jstr,Jend
            IF (LBC_apply(ng)%west(j)) THEN
              enthalpi(Istr-1,j,linew)=enthalpi(Istr,j,linew)
# ifdef MASKING
              enthalpi(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)*        &
     &                                 GRID(ng)%rmask(Istr-1,j)
# endif
# ifdef WET_DRY
              enthalpi(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)*        &
     &                                 GRID(ng)%rmask_wet(Istr-1,j)
# endif
              ti(Istr-1,j,linew)=enthalpi(Istr-1,j,linew)/              &
     &                           MAX(hi(Istr-1,j,linew),1.0E-6_r8)
            END IF
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Lateral boundary conditions at the eastern edge.
!-----------------------------------------------------------------------
!
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
!
!  Eastern edge, clamped boundary condition.
!
        IF (LBC(ieast,isTice,ng)%clamped) THEN
          DO j=Jstr,Jend
            IF (LBC_apply(ng)%east(j)) THEN
              enthalpi(Iend+1,j,linew)=BOUNDARY(ng)%hi_east(j)*         &
     &                                 BOUNDARY(ng)%ti_east(j)
              ti(Iend+1,j,linew)=BOUNDARY(ng)%ti_east(j)
# ifdef MASKING
              enthalpi(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)*        &
     &                                 GRID(ng)%rmask(Iend+1,j)
              ti(Iend+1,j,linew)=ti(Iend+1,j,linew)*                    &
     &                           GRID(ng)%rmask(Iend+1,j)
# endif
# ifdef WET_DRY
              enthalpi(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)*        &
     &                                 GRID(ng)%rmask_wet(Iend+1,j)
              ti(Iend+1,j,linew)=ti(Iend+1,j,linew)*                    &
     &                           GRID(ng)%rmask_wet(Iend+1,j)
# endif
            END IF
          END DO
!
!  Eastern edge, clamped on inflow, gradient on outflow.
!
        ELSE IF (LBC(ieast,isTice,ng)%mixed) THEN
          DO j=Jstr,Jend
            IF (LBC_apply(ng)%east(j)) THEN
              IF (ui(Iend+1,j,linew).le.0._r8) THEN
                enthalpi(Iend+1,j,linew)=BOUNDARY(ng)%hi_east(j)*       &
     &                                   BOUNDARY(ng)%ti_east(j)
                ti(Iend+1,j,linew)=BOUNDARY(ng)%ti_east(j)
              ELSE
                enthalpi(Iend+1,j,linew)=enthalpi(Iend,j,linew)
                ti(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)/            &
     &                             MAX(hi(Iend+1,j,linew),1.0E-6_r8)
              END IF
# ifdef MASKING
              enthalpi(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)*        &
     &                                 GRID(ng)%rmask(Iend+1,j)
              ti(Iend+1,j,linew)=ti(Iend+1,j,linew)*                    &
     &                           GRID(ng)%rmask(Iend+1,j)
# endif
# ifdef WET_DRY
              enthalpi(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)*        &
     &                                 GRID(ng)%rmask_wet(Iend+1,j)
              ti(Iend+1,j,linew)=ti(Iend+1,j,linew)*                    &
     &                           GRID(ng)%rmask_wet(Iend+1,j)
# endif
            END IF
          END DO
!
!  Eastern edge, gradient boundary condition.
!
        ELSE IF (LBC(ieast,isTice,ng)%gradient) THEN
          DO j=Jstr,Jend
            IF (LBC_apply(ng)%east(j)) THEN
              enthalpi(Iend+1,j,linew)=enthalpi(Iend,j,linew)
# ifdef MASKING
              enthalpi(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)*        &
     &                                 GRID(ng)%rmask(Iend+1,j)
# endif
# ifdef WET_DRY
              enthalpi(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)*        &
     &                                 GRID(ng)%rmask_wet(Iend+1,j)
# endif
              ti(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)/              &
     &                           MAX(hi(Iend+1,j,linew),1.0E-6_r8)
            END IF
          END DO
!
!  Eastern edge, closed boundary condition.
!
        ELSE IF (LBC(ieast,isTice,ng)%closed) THEN
          DO j=Jstr,Jend
            IF (LBC_apply(ng)%east(j)) THEN
              enthalpi(Iend+1,j,linew)=enthalpi(Iend,j,linew)
# ifdef MASKING
              enthalpi(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)*        &
     &                                 GRID(ng)%rmask(Iend+1,j)
# endif
# ifdef WET_DRY
              enthalpi(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)*        &
     &                                 GRID(ng)%rmask_wet(Iend+1,j)
# endif
              ti(Iend+1,j,linew)=enthalpi(Iend+1,j,linew)/              &
     &                           MAX(hi(Iend+1,j,linew),1.0E-6_r8)
            END IF
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Lateral boundary conditions at the southern edge.
!-----------------------------------------------------------------------
!
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
!
!  Southern edge, clamped boundary condition.
!
        IF (LBC(isouth,isTice,ng)%clamped) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%south(i)) THEN
              enthalpi(i,Jstr-1,linew)=BOUNDARY(ng)%hi_south(i)*        &
     &                                 BOUNDARY(ng)%ti_south(i)
              ti(i,Jstr-1,linew)=BOUNDARY(ng)%ti_south(i)
# ifdef MASKING
              enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)*        &
     &                                 GRID(ng)%rmask(i,Jstr-1)
              ti(i,Jstr-1,linew)=ti(i,Jstr-1,linew)*                    &
     &                           GRID(ng)%rmask(i,Jstr-1)
# endif
# ifdef WET_DRY
              enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)*        &
     &                                 GRID(ng)%rmask_wet(i,Jstr-1)
              ti(i,Jstr-1,linew)=ti(i,Jstr-1,linew)*                    &
     &                           GRID(ng)%rmask_wet(i,Jstr-1)
# endif
            END IF
          END DO
!
!  Southern edge, clamped on inflow, gradient on outflow.
!
        ELSE IF (LBC(isouth,isTice,ng)%mixed) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%south(i)) THEN
              IF (vi(i,Jstr,linew).ge.0._r8) THEN
                enthalpi(i,Jstr-1,linew)=BOUNDARY(ng)%hi_south(i)*      &
     &                                   BOUNDARY(ng)%ti_south(i)
                ti(i,Jstr-1,linew)=BOUNDARY(ng)%ti_south(i)
              ELSE
                enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr,linew)
                ti(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)/            &
     &                             MAX(hi(i,Jstr-1,linew),1.0E-6_r8)
              END IF
# ifdef MASKING
              enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)*        &
     &                                 GRID(ng)%rmask(i,Jstr-1)
              ti(i,Jstr-1,linew)=ti(i,Jstr-1,linew)*                    &
     &                           GRID(ng)%rmask(i,Jstr-1)
# endif
# ifdef WET_DRY
              enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)*        &
     &                                 GRID(ng)%rmask_wet(i,Jstr-1)
              ti(i,Jstr-1,linew)=ti(i,Jstr-1,linew)*                    &
     &                           GRID(ng)%rmask_wet(i,Jstr-1)
# endif
            END IF
          END DO
!
!  Southern edge, gradient boundary condition.
!
        ELSE IF (LBC(isouth,isTice,ng)%gradient) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%south(i)) THEN
              enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr,linew)
# ifdef MASKING
              enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)*        &
     &                                 GRID(ng)%rmask(i,Jstr-1)
# endif
# ifdef WET_DRY
              enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)*        &
     &                                 GRID(ng)%rmask_wet(i,Jstr-1)
# endif
              ti(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)/              &
     &                           MAX(hi(i,Jstr-1,linew),1.0E-6_r8)
            END IF
          END DO
!
!  Southern edge, closed boundary condition.
!
        ELSE IF (LBC(isouth,isTice,ng)%closed) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%south(i)) THEN
              enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr,linew)
# ifdef MASKING
              enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)*        &
     &                                 GRID(ng)%rmask(i,Jstr-1)
# endif
# ifdef WET_DRY
              enthalpi(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)*        &
     &                                 GRID(ng)%rmask_wet(i,Jstr-1)
# endif
              ti(i,Jstr-1,linew)=enthalpi(i,Jstr-1,linew)/              &
     &                           MAX(hi(i,Jstr-1,linew),1.0E-6_r8)
            END IF
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Lateral boundary conditions at the northern edge.
!-----------------------------------------------------------------------
!
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
!
!  Northern edge, clamped boundary condition.
!
        IF (LBC(inorth,isTice,ng)%clamped) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%north(i)) THEN
              enthalpi(i,Jend+1,linew)=BOUNDARY(ng)%hi_north(i)*        &
     &                                 BOUNDARY(ng)%ti_north(i)
              ti(i,Jend+1,linew)=BOUNDARY(ng)%ti_north(i)
# ifdef MASKING
              enthalpi(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)*        &
     &                                 GRID(ng)%rmask(i,Jend+1)
              ti(i,Jend+1,linew)=ti(i,Jend+1,linew)*                    &
     &                           GRID(ng)%rmask(i,Jend+1)
# endif
# ifdef WET_DRY
              enthalpi(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)*        &
     &                                 GRID(ng)%rmask_wet(i,Jend+1)
              ti(i,Jend+1,linew)=ti(i,Jend+1,linew)*                    &
     &                           GRID(ng)%rmask_wet(i,Jend+1)
# endif
            END IF
          END DO
!
!  Northern edge, clamped on inflow, gradient on outflow.
!
        ELSE IF (LBC(inorth,isTice,ng)%mixed) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%north(i)) THEN
              IF (vi(i,Jend+1,linew).le.0._r8) THEN
                enthalpi(i,Jend+1,linew)=BOUNDARY(ng)%hi_north(i)*      &
     &                                   BOUNDARY(ng)%ti_north(i)
                ti(i,Jend+1,linew)=BOUNDARY(ng)%ti_north(i)
              ELSE
                enthalpi(i,Jend+1,linew)=enthalpi(i,Jend,linew)
                ti(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)/            &
     &                             MAX(hi(i,Jend+1,linew),1.0E-6_r8)
              END IF
# ifdef MASKING
              enthalpi(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)*        &
     &                                 GRID(ng)%rmask(i,Jend+1)
              ti(i,Jend+1,linew)=ti(i,Jend+1,linew)*                    &
     &                           GRID(ng)%rmask(i,Jend+1)
# endif
# ifdef WET_DRY
              enthalpi(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)*        &
     &                                 GRID(ng)%rmask_wet(i,Jend+1)
              ti(i,Jend+1,linew)=ti(i,Jend+1,linew)*                    &
     &                           GRID(ng)%rmask_wet(i,Jend+1)
# endif
            END IF
          END DO
!
!  Northern edge, gradient boundary condition.
!
        ELSE IF (LBC(inorth,isTice,ng)%gradient) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%north(i)) THEN
              enthalpi(i,Jend+1,linew)=enthalpi(i,Jend,linew)
# ifdef MASKING
              enthalpi(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)*        &
     &                                 GRID(ng)%rmask(i,Jend+1)
# endif
# ifdef WET_DRY
              enthalpi(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)*        &
     &                                 GRID(ng)%rmask_wet(i,Jend+1)
# endif
              ti(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)/              &
     &                           MAX(hi(i,Jend+1,linew),1.0E-6_r8)
            END IF
          END DO
!
!  Northern edge, closed boundary condition.
!
        ELSE IF (LBC(inorth,isTice,ng)%closed) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%north(i)) THEN
              enthalpi(i,Jend+1,linew)=enthalpi(i,Jend,linew)
# ifdef MASKING
              enthalpi(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)*        &
     &                                 GRID(ng)%rmask(i,Jend+1)
# endif
# ifdef WET_DRY
              enthalpi(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)*        &
     &                                 GRID(ng)%rmask_wet(i,Jend+1)
# endif
              ti(i,Jend+1,linew)=enthalpi(i,Jend+1,linew)/              &
     &                           MAX(hi(i,Jend+1,linew),1.0E-6_r8)
            END IF
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Boundary corners.
!-----------------------------------------------------------------------
!
      IF (.not.(EWperiodic(ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
          IF (LBC_apply(ng)%south(Istr-1).and.                          &
     &        LBC_apply(ng)%west (Jstr-1)) THEN
            enthalpi(Istr-1,Jstr-1,linew)=                              &
     &        0.5_r8*(enthalpi(Istr  ,Jstr-1,linew)+                    &
     &                enthalpi(Istr-1,Jstr  ,linew))
            ti(Istr-1,Jstr-1,linew)=                                    &
     &        0.5_r8*(ti(Istr  ,Jstr-1,linew)+                          &
     &                ti(Istr-1,Jstr  ,linew))
          END IF
        END IF
        IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          IF (LBC_apply(ng)%south(Iend+1).and.                          &
     &        LBC_apply(ng)%east (Jstr-1)) THEN
            enthalpi(Iend+1,Jstr-1,linew)=                              &
     &        0.5_r8*(enthalpi(Iend+1,Jstr  ,linew)+                    &
     &                enthalpi(Iend  ,Jstr-1,linew))
            ti(Iend+1,Jstr-1,linew)=                                    &
     &        0.5_r8*(ti(Iend+1,Jstr  ,linew)+                          &
     &                ti(Iend  ,Jstr-1,linew))
          END IF
        END IF
        IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          IF (LBC_apply(ng)%north(Istr-1).and.                          &
     &        LBC_apply(ng)%west (Jend+1)) THEN
            enthalpi(Istr-1,Jend+1,linew)=                              &
     &        0.5_r8*(enthalpi(Istr-1,Jend  ,linew)+                    &
     &                enthalpi(Istr  ,Jend+1,linew))
            ti(Istr-1,Jend+1,linew)=                                    &
     &        0.5_r8*(ti(Istr-1,Jend  ,linew)+                          &
     &                ti(Istr  ,Jend+1,linew))
          END IF
        END IF
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          IF (LBC_apply(ng)%north(Iend+1).and.                          &
     &        LBC_apply(ng)%east (Jend+1)) THEN
            enthalpi(Iend+1,Jend+1,linew)=                              &
     &        0.5_r8*(enthalpi(Iend+1,Jend  ,linew)+                    &
     &                enthalpi(Iend  ,Jend+1,linew))
            ti(Iend+1,Jend+1,linew)=                                    &
     &        0.5_r8*(ti(Iend+1,Jend  ,linew)+                          &
     &                ti(Iend  ,Jend+1,linew))
          END IF
        END IF
      END IF
      RETURN
      END SUBROUTINE tibc_tile
#endif

      END MODULE tibc_mod
