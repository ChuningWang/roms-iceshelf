#include "cppdefs.h"
      MODULE vibc_mod
#ifdef ICE_MODEL
! 
!=======================================================================
!  Copyright (c) 2002-2022 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license               Chuning Wang   !
!    See License_ROMS.txt                           Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This subroutine computes lateral boundary conditions for V-velocity.!
!                                                                      !
!=======================================================================
!

      implicit none

      PRIVATE
      PUBLIC  :: vibc_tile

      CONTAINS
!
!***********************************************************************
      SUBROUTINE vibc (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_ice
      USE mod_stepping

      integer, intent(in) :: ng, tile
!
#include "tile.h"
!
      CALL vibc_tile (ng, tile,                                         &
     &                LBi, UBi, LBj, UBj,                               &
     &                IminS, ImaxS, JminS, JmaxS,                       &
     &                liuol(ng), liunw(ng),                             &
     &                ICE(ng) % vi)
      RETURN
      END SUBROUTINE vibc
!
!***********************************************************************
      SUBROUTINE vibc_tile (ng, tile,                                   &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
     &                      liuol, liunw,                               &
     &                      vi)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
      USE mod_boundary
      USE mod_grid
      USE mod_scalars

      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: liuol, liunw

# ifdef ASSUMED_SHAPE
      real(r8), intent(inout) :: vi(LBi:,LBj:,:)
# else
      real(r8), intent(inout) :: vi(LBi:UBi,LBj:UBj,2)
# endif
!
!  Local variable declarations.
!
      integer :: i, Jmax, Jmin, j
      real(r8) :: Ce, Cx
      real(r8) :: cff, dVde, dVdt, dVdx, tau

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: grad
!
#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Lateral boundary conditions at the western edge.
!-----------------------------------------------------------------------
!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
!
!  Western edge, implicit upstream radiation condition.
!
        IF (LBC(iwest,isVice,ng)%radiation) THEN
          DO j=JstrV-1,Jend
            grad(Istr-1,j)=vi(Istr-1,j+1,liuol)-vi(Istr-1,j  ,liuol)
            grad(Istr  ,j)=vi(Istr  ,j+1,liuol)-vi(Istr  ,j  ,liuol)
          END DO
          DO j=JstrV,Jend
            IF (LBC_apply(ng)%west(j)) THEN
              dVdt=vi(Istr,j,liuol)-vi(Istr  ,j,liunw)
              dVdx=vi(Istr,j,liunw)-vi(Istr+1,j,liunw)

              IF (LBC(iwest,isVice,ng)%nudging) THEN
                IF ((dVdt*dVdx).lt.0.0_r8) THEN
                  tau=M2obc_in (ng,iwest)
                ELSE
                  tau=M2obc_out(ng,iwest)
                END IF
# ifdef IMPLICIT_NUDGING
                IF (tau.gt.0.0_r8) tau=1.0_r8/tau
# else
                tau=tau*dt(ng)
# endif
              END IF

              IF ((dVdt*dVdx).lt.0.0_r8) dVdt=0.0_r8
              IF ((dVdt*(grad(Istr,j-1)+                                &
     &                   grad(Istr,j  ))).gt.0.0_r8) THEN
                dVde=grad(Istr,j-1)
              ELSE
                dVde=grad(Istr,j  )
              END IF
              cff=MAX(dVdx*dVdx+dVde*dVde,tol)
              Cx=dVdt*dVdx
# ifdef RADIATION_2D
              Ce=MIN(cff,MAX(dVdt*dVde,-cff))
# else
              Ce=0.0_r8
# endif
              vi(Istr-1,j,liunw)=(cff*vi(Istr-1,j,liuol)+               &
     &                            Cx *vi(Istr  ,j,liunw)-               &
     &                            MAX(Ce,0.0_r8)*grad(Istr-1,j-1)-      &
     &                            MIN(Ce,0.0_r8)*grad(Istr-1,j  )) /    &
     &                           (cff+Cx)

              IF (LBC(iwest,isVice,ng)%nudging) THEN
#ifdef IMPLICIT_NUDGING
                phi=dt(ng)/(tau+dt(ng))
                vi(Istr-1,j,liunw)=(1.0_r8-phi)*vi(Istr-1,j,liunw)+     &
     &                             phi*BOUNDARY(ng)%vi_west(j)
#else
                vi(Istr-1,j,liunw)=vi(Istr-1,j,liunw)+                  &
     &                             tau*(BOUNDARY(ng)%vi_west(j)-        &
     &                                  vi(Istr-1,j,liuol))
#endif
              END IF
# ifdef MASKING
              vi(Istr-1,j,liunw)=vi(Istr-1,j,liunw)*                    &
     &                           GRID(ng)%vmask(Istr-1,j)
# endif
            END IF
          END DO
!
!  Western edge, clamped boundary condition.
!
        ELSE IF (LBC(iwest,isVice,ng)%clamped) THEN
          DO j=JstrV,Jend
            IF (LBC_apply(ng)%west(j)) THEN
              vi(Istr-1,j,liunw)=BOUNDARY(ng)%vi_west(j)
# ifdef MASKING
              vi(Istr-1,j,liunw)=vi(Istr-1,j,liunw)*                    &
     &                           GRID(ng)%vmask(Istr-1,j)
# endif
            END IF
          END DO
!
!  Western edge, gradient boundary condition.
!
        ELSE IF (LBC(iwest,isVice,ng)%gradient) THEN
          DO j=JstrV,Jend
            IF (LBC_apply(ng)%west(j)) THEN
              vi(Istr-1,j,liunw)=vi(Istr,j,liunw)
# ifdef MASKING
              vi(Istr-1,j,liunw)=vi(Istr-1,j,liunw)*                    &
     &                           GRID(ng)%vmask(Istr-1,j)
# endif
            END IF
          END DO
!
!  Western edge, gradient boundary condition.
!
        ELSE IF (LBC(iwest,isVice,ng)%mixed) THEN
          DO j=JstrV,Jend
            IF (LBC_apply(ng)%west(j)) THEN
              vi(Istr-1,j,liunw)=vi(Istr,j,liunw)
# ifdef MASKING
              vi(Istr-1,j,liunw)=vi(Istr-1,j,liunw)*                    &
     &                           GRID(ng)%rmask(Istr-1,j)
# endif
            END IF
          END DO
!
!  Western edge, closed boundary condition: free slip (gamma2=1)  or
!                                           no   slip (gamma2=-1).
!
        ELSE IF (LBC(iwest,isVice,ng)%closed) THEN
          IF (NSperiodic(ng)) THEN
            Jmin=JstrV
            Jmax=Jend
          ELSE
            Jmin=Jstr
            Jmax=JendR
          END IF
          DO j=Jmin,Jmax
            IF (LBC_apply(ng)%west(j)) THEN
              vi(Istr-1,j,liunw)=gamma2(ng)*vi(Istr-1,j,liunw)
# ifdef MASKING
              vi(Istr-1,j,liunw)=vi(Istr-1,j,liunw)*                    &
     &                           GRID(ng)%vmask(Istr-1,j)
# endif
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
!  Eastern edge, implicit upstream radiation condition.
!
        IF (LBC(ieast,isVice,ng)%radiation) THEN
          DO j=JstrV-1,Jend
            grad(Iend  ,j)=vi(Iend  ,j+1,liuol)-vi(Iend  ,j  ,liuol)
            grad(Iend+1,j)=vi(Iend+1,j+1,liuol)-vi(Iend+1,j  ,liuol)
          END DO
          DO j=JstrV,Jend
            IF (LBC_apply(ng)%east(j)) THEN
              dVdt=vi(Iend,j,liuol)-vi(Iend  ,j,liunw)
              dVdx=vi(Iend,j,liunw)-vi(Iend-1,j,liunw)

              IF (LBC(ieast,isVice,ng)%nudging) THEN
                IF ((dVdt*dVdx).lt.0.0_r8) THEN
                  tau=M2obc_in (ng,ieast)
                ELSE
                  tau=M2obc_out(ng,ieast)
                END IF
# ifdef IMPLICIT_NUDGING
                IF (tau.gt.0.0_r8) tau=1.0_r8/tau
# else
                tau=tau*dt(ng)
# endif
              END IF

              IF ((dVdt*dVdx).lt.0.0_r8) dVdt=0.0_r8
              IF ((dVdt*(grad(Iend,j-1)+                                &
     &                   grad(Iend,j  ))).gt.0.0_r8) THEN
                dVde=grad(Iend,j-1)
              ELSE
                dVde=grad(Iend,j  )
              END IF
              cff=MAX(dVdx*dVdx+dVde*dVde,tol)
              Cx=dVdt*dVdx
# ifdef RADIATION_2D
              Ce=MIN(cff,MAX(dVdt*dVde,-cff))
# else
              Ce=0.0_r8
# endif
              vi(Iend+1,j,liunw)=(cff*vi(Iend+1,j,liuol)+               &
     &                            Cx *vi(Iend  ,j,liunw)-               &
     &                            MAX(Ce,0.0_r8)*grad(Iend+1,j-1)-      &
     &                            MIN(Ce,0.0_r8)*grad(Iend+1,j  )) /    &
     &                           (cff+Cx)

              IF (LBC(ieast,isVice,ng)%nudging) THEN
# ifdef IMPLICIT_NUDGING
                phi=dt(ng)/(tau+dt(ng))
                vi(Iend+1,j,liunw)=(1.0_r8-phi)*vi(Iend+1,j,liunw)+     &
     &                             phi*BOUNDARY(ng)%vi_west(j)
# else
                vi(Iend+1,j,liunw)=vi(Iend+1,j,liunw)+                  &
     &                             tau*(BOUNDARY(ng)%vi_east(j)-        &
     &                                  vi(Iend+1,j,liuol))
# endif
              END IF
# ifdef MASKING
              vi(Iend+1,j,liunw)=vi(Iend+1,j,liunw)*                    &
     &                           GRID(ng)%vmask(Iend+1,j)
# endif
            END IF
          END DO
!
!  Eastern edge, clamped boundary condition.
!
        ELSE IF (LBC(ieast,isVice,ng)%clamped) THEN
          DO j=JstrV,Jend
            IF (LBC_apply(ng)%east(j)) THEN
              vi(Iend+1,j,liunw)=BOUNDARY(ng)%vi_east(j)
# ifdef MASKING
              vi(Iend+1,j,liunw)=vi(Iend+1,j,liunw)*                    &
     &                           GRID(ng)%vmask(Iend+1,j)
# endif
            END IF
          END DO
!
!  Eastern edge, gradient boundary condition.
!
        ELSE IF (LBC(ieast,isVice,ng)%gradient) THEN
          DO j=JstrV,Jend
            IF (LBC_apply(ng)%east(j)) THEN
              vi(Iend+1,j,liunw)=vi(Iend,j,liunw)
# ifdef MASKING
              vi(Iend+1,j,liunw)=vi(Iend+1,j,liunw)*                    &
     &                           GRID(ng)%vmask(Iend+1,j)
# endif
            END IF
          END DO
!
!  Eastern edge, gradient boundary condition.
!
        ELSE IF (LBC(ieast,isVice,ng)%mixed) THEN
          DO j=JstrV,Jend
            IF (LBC_apply(ng)%east(j)) THEN
              vi(Iend+1,j,liunw)=vi(Iend,j,liunw)
# ifdef MASKING
              vi(Iend+1,j,liunw)=vi(Iend+1,j,liunw)*                    &
     &                           GRID(ng)%rmask(Iend+1,j)
# endif
            END IF
          END DO
!
!  Eastern edge, closed boundary condition: free slip (gamma2=1)  or
!                                           no   slip (gamma2=-1).
!
        ELSE IF (LBC(ieast,isVice,ng)%closed) THEN
          IF (NSperiodic(ng)) THEN
            Jmin=JstrV
            Jmax=Jend
          ELSE
            Jmin=Jstr
            Jmax=JendR
          END IF
          DO j=Jmin,Jmax
            IF (LBC_apply(ng)%east(j)) THEN
              vi(Iend+1,j,liunw)=gamma2(ng)*vi(Iend,j,liunw)
# ifdef MASKING
              vi(Iend+1,j,liunw)=vi(Iend+1,j,liunw)*                    &
     &                           GRID(ng)%vmask(Iend+1,j)
# endif
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
!  Southern edge, implicit upstream radiation condition.
!
        IF (LBC(isouth,isVice,ng)%radiation) THEN
          DO i=Istr,Iend+1
            grad(i,Jstr  )=vi(i  ,Jstr  ,liuol)-vi(i-1,Jstr  ,liuol)
            grad(i,Jstr+1)=vi(i  ,Jstr+1,liuol)-vi(i-1,Jstr+1,liuol)
          END DO
          DO i=Istr,Iend
            IF (LBC_apply(ng)%south(i)) THEN
              dVdt=vi(i,Jstr+1,liuol)-vi(i,Jstr+1,liunw)
              dVde=vi(i,Jstr+1,liunw)-vi(i,Jstr+2,liunw)

              IF (LBC(isouth,isVice,ng)%nudging) THEN
                IF ((dVdt*dVde).lt.0.0_r8) THEN
                  tau=M2obc_in (ng,isouth)
                ELSE
                  tau=M2obc_out(ng,isouth)
                END IF
# ifdef IMPLICIT_NUDGING
                IF (tau.gt.0.0_r8) tau=1.0_r8/tau
# else
                tau=tau*dt(ng)
# endif
              END IF

              IF ((dVdt*dVde).lt.0.0_r8) dVdt=0.0_r8
              IF ((dVdt*(grad(i  ,Jstr+1)+                              &
     &                   grad(i+1,Jstr+1))).gt.0.0_r8) THEN
                dVdx=grad(i  ,Jstr+1)
              ELSE
                dVdx=grad(i+1,Jstr+1)
              END IF
              cff=MAX(dVdx*dVdx+dVde*dVde,tol)
# ifdef RADIATION_2D
              Cx=MIN(cff,MAX(dVdt*dVdx,-cff))
# else
              Cx=0.0_r8
# endif
              Ce=dVdt*dVde
              vi(i,Jstr,liunw)=(cff*vi(i,Jstr  ,liuol)+                 &
     &                          Ce *vi(i,Jstr+1,liunw)-                 &
     &                          MAX(Cx,0.0_r8)*grad(i  ,Jstr)-          &
     &                          MIN(Cx,0.0_r8)*grad(i+1,Jstr)) /        &
     &                         (cff+Ce)

              IF (LBC(isouth,isVice,ng)%nudging) THEN
# ifdef IMPLICIT_NUDGING
                phi=dt(ng)/(tau+dt(ng))
                vi(i,Jstr,liunw)=(1.0_r8-phi)*vi(i,Jstr,liunw)+         &
     &                           phi*BOUNDARY(ng)%vi_south(i)
# else
                vi(i,Jstr,liunw)=vi(i,Jstr,liunw)+                      &
     &                           tau*(BOUNDARY(ng)%vi_south(i)-         &
     &                                vi(i,Jstr,liuol))
# endif
              END IF
# ifdef MASKING
              vi(i,Jstr,liunw)=vi(i,Jstr,liunw)*                        &
     &                         GRID(ng)%vmask(i,Jstr)
# endif
            END IF
          END DO
!
!  Southern edge, clamped boundary condition.
!
        ELSE   IF (LBC(isouth,isVice,ng)%clamped) THEN
          DO   i=Istr,Iend
              IF (LBC_apply(ng)%south(i)) THEN
                vi(i,Jstr,liunw)=BOUNDARY(ng)%vi_south(i)
# ifdef MASKING
                vi(i,Jstr,liunw)=vi(i,Jstr,liunw)*                      &
     &                           GRID(ng)%vmask(i,Jstr)
# endif
            END IF
          END DO
!
!  Southern edge, gradient boundary condition.
!
        ELSE IF (LBC(isouth,isVice,ng)%gradient) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%south(i)) THEN
              vi(i,Jstr,liunw)=vi(i,Jstr+1,liunw)
# ifdef MASKING
              vi(i,Jstr,liunw)=vi(i,Jstr,liunw)*                        &
     &                         GRID(ng)%vmask(i,Jstr)
# endif
            END IF
          END DO
!
!  Southern edge, clamped on inflow, gradient on outflow.
!
        ELSE IF (LBC(isouth,isVice,ng)%mixed) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%south(i)) THEN
              IF (vi(i,Jstr+1,liunw).ge.0._r8) THEN
                vi(i,Jstr,liunw)=BOUNDARY(ng)%vi_south(i)
              ELSE
                vi(i,Jstr,liunw)=vi(i,Jstr+1,liunw)
              END IF
# ifdef MASKING
              vi(i,Jstr,liunw)=vi(i,Jstr,liunw)*                        &
     &                         GRID(ng)%rmask(i,Jstr)
# endif
            END IF
          END DO
!
!  Southern edge, closed boundary condition.
!
        ELSE IF (LBC(isouth,isVice,ng)%closed) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%south(i)) THEN
              vi(i,Jstr,liunw)=0.0_r8
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
!  Northern edge, implicit upstream radiation condition.
!
        IF (LBC(inorth,isVice,ng)%radiation) THEN
          DO i=Istr,Iend+1
            grad(i,Jend  )=vi(i  ,Jend  ,liuol)-vi(i-1,Jend  ,liuol)
            grad(i,Jend+1)=vi(i  ,Jend+1,liuol)-vi(i-1,Jend+1,liuol)
          END DO
          DO i=Istr,Iend
            IF (LBC_apply(ng)%north(i)) THEN
              dVdt=vi(i,Jend,liuol)-vi(i,Jend  ,liunw)
              dVde=vi(i,Jend,liunw)-vi(i,Jend-1,liunw)

              IF (LBC(inorth,isVice,ng)%nudging) THEN
                IF ((dVdt*dVde).lt.0.0_r8) THEN
                  tau=M2obc_in (ng,inorth)
                ELSE
                  tau=M2obc_out(ng,inorth)
                END IF
# ifdef IMPLICIT_NUDGING
                IF (tau.gt.0.0_r8) tau=1.0_r8/tau
# else
                tau=tau*dt(ng)
# endif
              END IF
              IF ((dVdt*dVde).lt.0.0_r8) dVdt=0.0_r8
              IF ((dVdt*(grad(i  ,Jend)+                                &
     &                   grad(i+1,Jend))).gt.0.0_r8) THEN
                dVdx=grad(i  ,Jend)
              ELSE
                dVdx=grad(i+1,Jend)
              END IF
              cff=MAX(dVdx*dVdx+dVde*dVde,tol)
# ifdef RADIATION_2D
              Cx=MIN(cff,MAX(dVdt*dVdx,-cff))
# else
              Cx=0.0_r8
# endif
              Ce=dVdt*dVde
              vi(i,Jend+1,liunw)=(cff*vi(i,Jend+1,liuol)+               &
     &                            Ce *vi(i,Jend  ,liunw)-               &
     &                            MAX(Cx,0.0_r8)*grad(i  ,Jend+1)-      &
     &                            MIN(Cx,0.0_r8)*grad(i+1,Jend+1)) /    &
     &                           (cff+Ce)

              IF (LBC(inorth,isVice,ng)%nudging) THEN
# ifdef IMPLICIT_NUDGING
                phi=dt(ng)/(tau+dt(ng))
                vi(i,Jend+1,liunw)=(1.0_r8-phi)*vi(i,Jend+1,liunw)+     &
     &                             phi*BOUNDARY(ng)%vi_north(i)
# else
                vi(i,Jend+1,liunw)=vi(i,Jend+1,liunw)+                  &
     &                             tau*(BOUNDARY(ng)%vi_north(i)-       &
     &                                  vi(i,Jend+1,liuol))
# endif
              END IF
# ifdef MASKING
              vi(i,Jend+1,liunw)=vi(i,Jend+1,liunw)*                    &
     &                           GRID(ng)%vmask(i,Jend+1)
# endif
            END IF
          END DO
!
!  Northern edge, clamped boundary condition.
!
        ELSE IF (LBC(inorth,isVice,ng)%clamped) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%north(i)) THEN
              vi(i,Jend+1,liunw)=BOUNDARY(ng)%vi_north(i)
# ifdef MASKING
              vi(i,Jend+1,liunw)=vi(i,Jend+1,liunw)*                    &
     &                           GRID(ng)%vmask(i,Jend+1)
# endif
            END IF
          END DO
!
!  Northern edge, gradient boundary condition.
!
        ELSE IF (LBC(inorth,isVice,ng)%gradient) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%north(i)) THEN
              vi(i,Jend+1,liunw)=vi(i,Jend,liunw)
# ifdef MASKING
              vi(i,Jend+1,liunw)=vi(i,Jend+1,liunw)*                    &
     &                           GRID(ng)%vmask(i,Jend+1)
# endif
            END IF
          END DO
!
!  Northern edge, clamped on inflow, gradient on outflow.
!
        ELSE IF (LBC(inorth,isVice,ng)%mixed) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%north(i)) THEN
              IF (vi(i,Jend,liunw).le.0._r8) THEN
                vi(i,Jend+1,liunw)=BOUNDARY(ng)%vi_north(i)
              ELSE
                vi(i,Jend+1,liunw)=vi(i,Jend,liunw)
              END IF
# ifdef MASKING
              vi(i,Jend+1,liunw)=vi(i,Jend+1,liunw)*                    &
     &                           GRID(ng)%rmask(i,Jend+1)
# endif
            END IF
          END DO
!
!  Northern edge, closed boundary condition.
!
        ELSE IF (LBC(inorth,isVice,ng)%closed) THEN
          DO i=Istr,Iend
            IF (LBC_apply(ng)%east(j)) THEN
              vi(i,Jend+1,liunw)=0.0_r8
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
     &        LBC_apply(ng)%west (Jstr  )) THEN
            vi(Istr-1,Jstr  ,liunw)=0.5_r8*(vi(Istr  ,Jstr  ,liunw)+    &
     &                                      vi(Istr-1,Jstr+1,liunw))
          END IF
        END IF
        IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          IF (LBC_apply(ng)%south(Iend+1).and.                          &
     &        LBC_apply(ng)%east (Jstr  )) THEN
            vi(Iend+1,Jstr  ,liunw)=0.5_r8*(vi(Iend  ,Jstr  ,liunw)+    &
     &                                      vi(Iend+1,Jstr+1,liunw))
          END IF
        END IF
        IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          IF (LBC_apply(ng)%north(Istr-1).and.                          &
     &        LBC_apply(ng)%west (Jend+1)) THEN
            vi(Istr-1,Jend+1,liunw)=0.5_r8*(vi(Istr-1,Jend  ,liunw)+    &
     &                                      vi(Istr  ,Jend+1,liunw))
          END IF
        END IF
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          IF (LBC_apply(ng)%north(Iend+1).and.                          &
     &        LBC_apply(ng)%east (Jend+1)) THEN
            vi(Iend+1,Jend+1,liunw)=0.5_r8*(vi(Iend+1,Jend  ,liunw)+    &
     &                                      vi(Iend  ,Jend+1,liunw))
          END IF
        END IF
      END IF

      RETURN
      END SUBROUTINE vibc_tile
#endif
      END MODULE vibc_mod
