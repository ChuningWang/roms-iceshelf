!
!***********************************************************************
      SUBROUTINE ice_thermo (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_grid
      USE mod_ocean
      USE mod_ice
      USE mod_forces
      USE mod_stepping
#ifdef ICE_SHALLOW_LIMIT
      USE mod_coupling
#endif

      integer, intent(in) :: ng, tile
!
#include "tile.h"
!
#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 92, __LINE__, __FILE__)
#endif
!
!-----------------------------------------------------------------------
!  Calculate ice thermodynamics.
!-----------------------------------------------------------------------
!
      CALL ice_thermo_tile (ng, tile,                                   &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
     &                      nrhs(ng), liold(ng), linew(ng),             &
#ifdef MASKING
     &                      GRID(ng) % rmask,                           &
#endif
#ifdef WET_DRY
     &                      GRID(ng) % rmask_wet,                       &
#endif
#ifdef ICESHELF
     &                      GRID(ng) % zice,                            &
#endif
#ifdef ICE_SHALLOW_LIMIT
     &                      GRID(ng) % h,                               &
     &                      COUPLING(ng) % Zt_avg1,                     &
#endif
     &                      GRID(ng) % z_r,                             &
     &                      GRID(ng) % z_w,                             &
     &                      OCEAN(ng) % t,                              &
     &                      ICE(ng) % ui,                               &
     &                      ICE(ng) % vi,                               &
     &                      ICE(ng) % wfr,                              &
     &                      ICE(ng) % ai,                               &
     &                      ICE(ng) % hi,                               &
     &                      ICE(ng) % hsn,                              &
     &                      ICE(ng) % ageice,                           &
     &                      ICE(ng) % tis,                              &
     &                      ICE(ng) % ti,                               &
     &                      ICE(ng) % t2,                               &
     &                      ICE(ng) % enthalpi,                         &
     &                      ICE(ng) % hage,                             &
     &                      ICE(ng) % coef_ice_heat,                    &
     &                      ICE(ng) % rhs_ice_heat,                     &
     &                      ICE(ng) % s0mk,                             &
     &                      ICE(ng) % t0mk,                             &
     &                      ICE(ng) % wai,                              &
     &                      ICE(ng) % wao,                              &
     &                      ICE(ng) % wio,                              &
     &                      ICE(ng) % wro,                              &
     &                      ICE(ng) % iomflx,                           &
     &                      FORCES(ng) % sustr,                         &
     &                      FORCES(ng) % svstr,                         &
     &                      FORCES(ng) % wsf,                           &
     &                      FORCES(ng) % qswo,                          &
     &                      FORCES(ng) % qai,                           &
     &                      FORCES(ng) % qao,                           &
     &                      FORCES(ng) % qio,                           &
     &                      FORCES(ng) % qi2,                           &
#ifdef BULK_FLUXES
     &                      FORCES(ng) % rain,                          &
# ifdef ICE_SNOWFALL
     &                      FORCES(ng) % snow,                          &
# endif
#endif
#ifdef ICE_DIAGS
     &                      FORCES(ng) % wsni,                          &
     &                      FORCES(ng) % sao,                           &
     &                      FORCES(ng) % sio,                           &
#endif
     &                      FORCES(ng) % stflx)
#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 92, __LINE__, __FILE__)
#endif
      RETURN
      END SUBROUTINE ice_thermo
!
!***********************************************************************
      SUBROUTINE ice_thermo_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
     &                            nrhs, liold, linew,                   &
#ifdef MASKING
     &                            rmask,                                &
#endif
#ifdef WET_DRY
     &                            rmask_wet,                            &
#endif
#ifdef ICESHELF
     &                            zice,                                 &
#endif
#ifdef ICE_SHALLOW_LIMIT
     &                            h, Zt_avg1,                           &
#endif
     &                            z_r, z_w, t,                          &
     &                            ui, vi,                               &
     &                            wfr,                                  &
     &                            ai, hi, hsn, ageice,                  &
     &                            tis, ti, t2,                          &
     &                            enthalpi, hage,                       &
     &                            coef_ice_heat, rhs_ice_heat,          &
     &                            s0mk, t0mk,                           &
     &                            wai, wao, wio, wro, iomflx,           &
     &                            sustr, svstr,                         &
     &                            wsf, qswo,                            &
     &                            qai, qao, qio, qi2,                   &
#ifdef BULK_FLUXES
     &                            rain,                                 &
# ifdef ICE_SNOWFALL
     &                            snow,                                 &
# endif
#endif
#ifdef ICE_DIAGS
     &                            wsni, sao, sio,                       &
#endif
     &                            stflx)
!***********************************************************************
!
!-----------------------------------------------------------------------
!
!  Original comment:
!      beregner varmefluxer og produskjonsrater
!      og oppdaterer tis (t3 i mellor et.al.)
!
!  means compute heat fluxes and ice production rates:
!
!      wai(i,j)=-(qai(i,j) -qi2(i,j)) / (hfus1(i,j)*rho0)
!
!  and up date the internal ice temperature (t3 in Mellor et all).
!
!        tis(i,j)=tis(i,j)+del
!
!        the following global arrays are calculated:
!        (description is given below)
!
!        apond
!        hpond
!        ageice
!        qai
!        qio
!        qi2
!        wsm
!        wai
!        wro
!        ti
!        t2
!        tis
!        hfus1
!        coa
!
!        parameters:
!
!         inp from atmosphere model:
!             wind_speed(im,jm)  -  abs(wind_10_meter)
!             Tair(im,jm)        -  atmos. temperature
!             rh(im,jm)          -  atmosphere specific humidity
!             snow(im,jm)        -  snow fall rate
!
!
!         inp from ocean model:
!
!             t0mk(im,jm)        -  sea surface temperature
!             rmask(im,jm)       -  pointer (land/ocean)
!
!             dtice              -  time step
!                                    dtice=float(isplitc)*dti
!
!       global variables transfered by module:
!
!            ----  needs to be initiated elswhere ----
!
!   (rads)   sw_flux(i,j)        -  incoming short wave radiation
!   (rads)   lwrad(i,j)          -  incoming long wave radiation
!
!            qi2(i,j)            -  heat flux in ice
!            hi(i,j,linew)       -  ice mass (divided by area)
!            ai(i,j,linew)       -  ice concentration
!            hsn(i,j,linew)      -  mass snow (pr. area) ai*s_thick
!            ti(i,j,linew)       -  temperature in middle of ice
!                                   (t1 in Mellor ...)
!            enthalpi(i,j,linew) -  scaled perturbation ice heat content
!            tis(i,j)            -  temperature at snow/atmos. interface
!                                   (t3 in Mellor ...)
!            brnfr(i,j)          -  brine fraction
!            wsm(i,j)            -  snow melting rate
!            wai(i,j)            -  melt rate at atmos./ice
!            apond(i,j,linew)    -  melt water fraction
!            hpond(i,j,linew)    -  melt water depth
!            ageice(i,j,linew)   -  ice age
!
!
!            ----  initiated in this routine ----
!
!            qai(i,j)            -  heat flux atmosphere/ice
!                                   (positive from ice to atm.)
!            qio(i,j)            -  heat flux ice/ocean
!                                   (positive from ocean)
!            hfus1(i,j)          -  heat of fusion (L_o or L_3)
!            wro(i,j)            -  production rate of surface runoff
!            t2(i,j)             -  temperature at ice/snow interface
!
!-----------------------------------------------------------------------
!
      USE mod_param
      USE mod_ncparam
      USE mod_scalars
      USE mod_clima
!
      USE bc_2d_mod, ONLY : bc_r2d_tile
      USE i2dbc_mod, ONLY : i2dbc_ice, i2dbc_tice
!
      USE exchange_2d_mod, ONLY : exchange_r2d_tile
#ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
#endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nrhs, liold, linew
#ifdef ASSUMED_SHAPE
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
# endif
# ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:,LBj:)
# endif
# ifdef ICESHELF
      real(r8), intent(in) :: zice(LBi:,LBj:)
# endif
# ifdef ICE_SHALLOW_LIMIT
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: Zt_avg1(LBi:,LBj:)
# endif
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: t(LBi:,LBj:,:,:,:)
      real(r8), intent(in) :: ui(LBi:,LBj:,:)
      real(r8), intent(in) :: vi(LBi:,LBj:,:)
      real(r8), intent(in) :: wfr(LBi:,LBj:)
      real(r8), intent(inout) :: ai(LBi:,LBj:,:)
      real(r8), intent(inout) :: hi(LBi:,LBj:,:)
      real(r8), intent(inout) :: hsn(LBi:,LBj:,:)
      real(r8), intent(inout) :: ageice(LBi:,LBj:,:)
      real(r8), intent(inout) :: tis(LBi:,LBj:)
      real(r8), intent(inout) :: ti(LBi:,LBj:,:)
      real(r8), intent(inout) :: t2(LBi:,LBj:)
      real(r8), intent(inout) :: enthalpi(LBi:,LBj:,:)
      real(r8), intent(inout) :: hage(LBi:,LBj:,:)
      real(r8), intent(inout) :: coef_ice_heat(LBi:,LBj:)
      real(r8), intent(inout) :: rhs_ice_heat(LBi:,LBj:)
      real(r8), intent(inout) :: s0mk(LBi:,LBj:)
      real(r8), intent(inout) :: t0mk(LBi:,LBj:)
      real(r8), intent(inout) :: wai(LBi:,LBj:)
      real(r8), intent(inout) :: wao(LBi:,LBj:)
      real(r8), intent(inout) :: wio(LBi:,LBj:)
      real(r8), intent(inout) :: wro(LBi:,LBj:)
      real(r8), intent(out) :: iomflx(LBi:,LBj:)
!
      real(r8), intent(in) :: sustr(LBi:,LBj:)
      real(r8), intent(in) :: svstr(LBi:,LBj:)
      real(r8), intent(in) :: wsf(LBi:,LBj:)
      real(r8), intent(in) :: qswo(LBi:,LBj:)
      real(r8), intent(in) :: qai(LBi:,LBj:)
      real(r8), intent(in) :: qao(LBi:,LBj:)
      real(r8), intent(out) :: qio(LBi:,LBj:)
      real(r8), intent(out) :: qi2(LBi:,LBj:)
# ifdef BULK_FLUXES
      real(r8), intent(in) :: rain(LBi:,LBj:)
#  ifdef ICE_SNOWFALL
      real(r8), intent(in) :: snow(LBi:,LBj:)
#  endif
# endif
# ifdef ICE_DIAGS
      real(r8), intent(inout) :: wsni(LBi:,LBj:)
      real(r8), intent(out) :: sao(LBi:,LBj:)
      real(r8), intent(out) :: sio(LBi:,LBj:)
# endif
      real(r8), intent(inout) :: stflx(LBi:,LBj:,:)
#else
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
# endif
# ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:UBi,LBj:UBj)
# endif
# ifdef ICESHELF
      real(r8), intent(in) :: zice(LBi:UBi,LBj:UBj)
# endif
# ifdef ICE_SHALLOW_LIMIT
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Zt_avg1(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:N(ng))
      real(r8), intent(in) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
      real(r8), intent(in) :: ui(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: vi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: wfr(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: ai(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: hi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: hsn(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: ageice(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: tis(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: ti(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: t2(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: enthalpi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: hage(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: coef_ice_heat(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: rhs_ice_heat(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: s0mk(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: t0mk(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: wai(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: wao(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: wio(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: wro(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: iomflx(LBi:UBi,LBj:UBj)
!
      real(r8), intent(in) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: svstr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: wsf(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: qswo(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: qai(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: qao(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: qio(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: qi2(LBi:UBi,LBj:UBj)
# ifdef BULK_FLUXES
      real(r8), intent(in) :: rain(LBi:UBi,LBj:UBj)
#  ifdef ICE_SNOWFALL
      real(r8), intent(in) :: snow(LBi:UBi,LBj:UBj)
#  endif
# endif
# ifdef ICE_DIAGS
      real(r8), intent(inout) :: wsni(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: sao(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: sio(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(inout) :: stflx(LBi:UBi,LBj:UBj,NT(ng))
#endif
!
! Local variable definitions
!
      real(r8), parameter :: eps = 1.E-4_r8
      real(r8), parameter :: prt = 13._r8
      real(r8), parameter :: prs = 2432._r8
      real(r8), parameter :: tpr = 0.85_r8
      real(r8), parameter :: nu = 1.8E-6_r8
      real(r8), parameter :: z0ii = 0.02_r8
      real(r8), parameter :: kappa = 0.4_r8
      real(r8), parameter :: alphic = 2.034_r8          ! [W m-1 K-1]
      real(r8), parameter :: alphsn = 0.31_r8           ! [W m-1 K-1]
      real(r8), parameter :: hfus = 3.347E+5_r8         ! [J kg-1]
      real(r8), parameter :: cpi = 2093.0_r8            ! [J kg-1 K-1]
      real(r8), parameter :: cpw = 3990.0_r8            ! [J kg-1 K-1]
      real(r8), parameter :: rhocpr = 0.2439634E-6_r8   ! [m s2 K kg-1]
      real(r8), parameter :: ykf = 3.14_r8
!
      integer :: i, j
!
      real(r8) :: tfrz, cot, ai_tmp
      real(r8) :: hicehinv  ! 1./(0.5*i_thick)
      real(r8) :: corfac, z0, zdz0, rno
      real(r8) :: termt, terms
      real(r8) :: xtot, phi
      real(r8) :: rhowr
      real(r8) :: cff
!
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: b2d
!
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: alph
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: ws
!
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: temp_top
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: salt_top
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: sice
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: brnfr
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: hfus1
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: wsm
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: utau
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: dztop
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: i_thick
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: s_thick
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: coa
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: cht
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: chs
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: ai_old
!
#ifdef ICE_SHALLOW_LIMIT
      real(r8) :: hh, clear
#endif
      real(r8) :: fac_sf
#ifdef ICE_CONVSNOW
      real(r8) :: hstar
#endif
!
#include "set_bounds.h"
!
!  Pass in sea surface states
!
      DO j = Jstr,Jend
        DO i = Istr,Iend
          temp_top(i,j) = t(i,j,N(ng),nrhs,itemp)
          salt_top(i,j) = MAX(0.0_r8,t(i,j,N(ng),nrhs,isalt))
          dztop(i,j)    = z_w(i,j,N(ng))-z_r(i,j,N(ng))
!
          utau(i,j) = SQRT(SQRT(                                        &
     &                (0.5_r8*(sustr(i,j)+sustr(i+1,j  )))**2 +         &
     &                (0.5_r8*(svstr(i,j)+svstr(i  ,j+1)))**2))
          utau(i,j) = MAX(utau(i,j),0.0001_r8)
        END DO
      END DO
!
!-----------------------------------------------------------------------
!   Get incoming long and shortwave radiation
!-----------------------------------------------------------------------
!
! *** all rho's 0n 1026 kg m-3. cp's on 4.186e+6 j m-3 c-1.
! *** sigma=5.67e-8 w m-2 k-4 : sigma=sigma*epsilon. m&u used 5.78e-8
! *** compute sw ,lw & back radiation (see p&w 1979)
!
!-----------------------------------------------------------------------
!  calculate those parts of the energy balance which do not depend
!  on the surface temperature.
!-----------------------------------------------------------------------
!
! *** ignore snow effects except change albi to albsn value
!
!     beregner sno- og is-tykkelse
!     (compute snow and ice thicknesses)
!
      DO j = Jstr,Jend
        DO i = Istr,Iend
!
!  Ice salinity and ice/snow thicknesses
!
          sice(i,j) = MIN(sice_ref,salt_top(i,j))
          i_thick(i,j) = MAX(0.05_r8,                                   &
     &                       hi(i,j,linew)/MAX(ai(i,j,linew),eps))
          s_thick(i,j) = hsn(i,j,linew)/MAX(ai(i,j,linew),eps)
!
!  Store old ai
!
          ai_old(i,j) = ai(i,j,linew)
!
!  Brine fraction
!
          brnfr(i,j) = frln*sice(i,j)/MIN(ti(i,j,linew),-eps)
          brnfr(i,j) = MAX(0.0_r8,MIN(0.2_r8,brnfr(i,j)))
!
!  alph - thermal conductivity of ice
!
          alph(i,j) = alphic*(1._r8-1.2_r8*brnfr(i,j))
          corfac    = 1._r8/(0.5_r8*(1._r8 +                            &
     &                               EXP(-(hi(i,j,linew)/1._r8)**2)))
          alph(i,j) = alph(i,j)*corfac
          coa(i,j)  = 2.0_r8*alph(i,j)*s_thick(i,j) /                   &
     &                (alphsn*(i_thick(i,j)+eps))
        END DO
      END DO
!
! Compute ice thermodynamic variables specify snow fall rate and snow
!   thickness compute net ice atmos. surface heat transfer zero if
!   temperature is below freezing.
!
!     Solve for temperature at the top of the ice layer
!
      DO j = Jstr,Jend
        DO i = Istr,Iend
!
!  Gradient coefficient for heat conductivity part
!
          b2d(i,j)           = 2.0_r8*alph(i,j) /                       &
     &                         ((i_thick(i,j)+eps)*(1._r8+coa(i,j)))
          coef_ice_heat(i,j) = coef_ice_heat(i,j) + b2d(i,j)
!
          IF (ai(i,j,linew) .gt. min_a(ng)) THEN
!
!  Downward conductivity term, assuming the ocean at the freezing point
!
            rhs_ice_heat(i,j) = rhs_ice_heat(i,j) +                     &
     &                          b2d(i,j)*ti(i,j,linew)
            tis(i,j) = rhs_ice_heat(i,j)/coef_ice_heat(i,j)
            tis(i,j) = MAX(tis(i,j),-45._r8)
          ELSE
            tis(i,j) = 0.0_r8
          END IF
        END DO
      END DO

      DO j = Jstr,Jend
        DO i = Istr,Iend
!
!  Calculate interior ice temp and heat fluxes
!
          IF (ai(i,j,linew) .gt. min_a(ng)) THEN
            cot = cpi - frln*sice(i,j)*hfus/(ti(i,j,linew)-eps)**2
            ti(i,j,linew) = ti(i,j,linew) +                             &
     & dtice(ng)*2._r8*alph(i,j)/(rhoice(ng)*i_thick(i,j)**2*cot)*      &
     & (t0mk(i,j) +                                                     &
     &  (tis(i,j) - (2._r8+coa(i,j))*ti(i,j,linew))/(1._r8+coa(i,j)))
!
            ti(i,j,linew) = MAX(ti(i,j,linew),-35._r8)
            ti(i,j,linew) = MIN(ti(i,j,linew),-eps   )
          ELSE
            ti(i,j,linew) = -2.0_r8
          END IF
        END DO
      END DO

      DO j = Jstr,Jend
        DO i = Istr,Iend
          IF (ai(i,j,linew) .gt. min_a(ng)) THEN
            t2(i,j) = (tis(i,j)+coa(i,j)*ti(i,j,linew)) /               &
     &                (1._r8+coa(i,j))
            hicehinv = 2._r8/i_thick(i,j)
!
!  Compute net heat flux from ice interior to snow
!    Mellor and Kantha Eq (9b)
!
            qi2(i,j) = alph(i,j)*(ti(i,j,linew)-t2(i,j)      )*hicehinv
!
!  Compute net heat flux from ocean to ice interior
!    Mellor and Kantha Eq (11)
!
            qio(i,j) = alph(i,j)*(t0mk(i,j)    -ti(i,j,linew))*hicehinv
          END IF
        END DO
      END DO
!
!  For open water ice fluxes set to zero
!
      DO j = Jstr,Jend
        DO i = Istr,Iend
          IF (ai(i,j,linew) .le. min_a(ng)) THEN
            tis(i,j)      = 0.0_r8
            t2(i,j)       = 0.0_r8
            ti(i,j,linew) = -2.0_r8
            qio(i,j)       = 0._r8
            qi2(i,j)       = 0._r8
            hsn(i,j,linew) = 0._r8
          END IF
#ifdef MASKING
          tis(i,j)      = tis(i,j)*rmask(i,j)
          t2(i,j)       = t2(i,j)*rmask(i,j)
          ti(i,j,linew) = ti(i,j,linew)*rmask(i,j)
#endif
#ifdef WET_DRY
          tis(i,j)      = tis(i,j)*rmask_wet(i,j)
          t2(i,j)       = t2(i,j)*rmask_wet(i,j)
          ti(i,j,linew) = ti(i,j,linew)*rmask_wet(i,j)
#endif
#ifdef ICESHELF
          IF (zice(i,j).ne.0.0_r8) THEN
            tis(i,j)      = 0.0_r8
            t2(i,j)       = 0.0_r8
            ti(i,j,linew) = -2.0_r8
          END IF
#endif
        END DO
      END DO
!
      DO j = Jstr,Jend
        DO i = Istr,Iend
!
!  Sea ice freezing point temperature
!
          tfrz = frln*sice(i,j)
!
!  Initialize variables
!
          wsm(i,j) = 0._r8
          wai(i,j) = 0._r8
          wro(i,j) = 0._r8
          IF (ai(i,j,linew) .gt. min_a(ng)) THEN
!
!  Melt ice or freeze surface water in the fall if there is no snow
!
            IF (hsn(i,j,linew) .le. 0.0_r8) THEN
              IF (tis(i,j) .gt. tfrz) THEN
!
!  Ice warmer than freezing point
!
                tis(i,j) = tfrz
                t2(i,j)  = tfrz
!
                hfus1(i,j) = hfus*(1._r8-brnfr(i,j)) + tis(i,j)*cpw -   &
     &            ((1._r8-brnfr(i,j))*cpi+brnfr(i,j)*cpw)*ti(i,j,linew)
!
!  Re-compute net heat flux from ice interior to snow
!    Mellor and Kantha Eq (9b)
!
                qi2(i,j) = b2d(i,j)*(ti(i,j,linew) - tis(i,j))
!
!  Compute ice melt rat - wai is positive for melt
!
                wai(i,j) = MAX(0.0_r8,(qi2(i,j) - qai(i,j))) /          &
     &                     (hfus1(i,j)*rho0)
!
!  Compute production rate for melt water (melting rate)
!
                wsm(i,j) = wsf(i,j)
              END IF
            ELSE
              IF (tis(i,j) .gt. 0.0_r8) THEN
!
!  There is snow cover to melt, and ice temperature warmer than fresh
!    water freezing point
!
                tis(i,j) = 0._r8
                qi2(i,j) = b2d(i,j)*(ti(i,j,linew) - tis(i,j))
                t2(i,j)  = (tis(i,j)+coa(i,j)*ti(i,j,linew)) /          &
     &                     (1._r8+coa(i,j))
!
!  Compute snow melting rate
!    Note snowfall are considered fully melted in this scenario
!
                wsm(i,j) = MAX(0.0_r8,(qi2(i,j) - qai(i,j)) /           &
     &                                (hfus*rhosnow_dry(ng))) + wsf(i,j)
              END IF
            END IF
!
!  Compute snow thickness
!
#ifdef ICE_NO_SNOW
            hsn(i,j,linew) = 0.0_r8
#else
            hsn(i,j,linew) = hsn(i,j,linew) +                           &
     &                       dtice(ng)*ai(i,j,linew)*(wsf(i,j)-wsm(i,j))
            hsn(i,j,linew) = MAX(0.0_r8,hsn(i,j,linew))
#endif
          END IF
          wro(i,j) = wai(i,j) + wsm(i,j)
        END DO
      END DO
!
      rhowr = 1.0_r8/rhow
!
      DO j = Jstr,Jend
        DO i = Istr,Iend
!
!  Yaglom and Kader formulation for z0t and z0s -- WPB
!
          z0 = max(z0ii*i_thick(i,j),0.01_r8)
          z0 = min(z0,0.1_r8)
!
          zdz0 = dztop(i,j)/z0
          zdz0 = MAX(zdz0,3._r8)
!
          rno = utau(i,j)*z0/nu
          termt = ykf*sqrt(rno)*prt**0.666667_r8
          terms = ykf*sqrt(rno)*prs**0.666667_r8
          cht(i,j) = utau(i,j)/(tpr*log(zdz0)/kappa+termt)
          chs(i,j) = utau(i,j)/(tpr*log(zdz0)/kappa+terms)
        END DO
      END DO
!
      DO j = Jstr,Jend
        DO i = Istr,Iend
!
!  Compute freezing rate, at ocean surface (wao) and ice base (wio)
!
          tfrz = frln*salt_top(i,j)
!
!  Initialize variables
!
          wao(i,j) = 0._r8
          wio(i,j) = 0._r8
!
          hfus1(i,j) = hfus*(1.0_r8-brnfr(i,j)) + t0mk(i,j)*cpw -       &
     &      ((1.0_r8-brnfr(i,j))*cpi+brnfr(i,j)*cpw)*ti(i,j,linew)
!
!  This should only happen if the ice frazil subroutine is disabled
!
!         IF (temp_top(i,j) .le. tfrz)                                  &
!    &      wao(i,j) = qao(i,j)/(hfus1(i,j)*rho0)
          IF (temp_top(i,j) .le. tfrz)                                  &
     &      temp_top(i,j) = tfrz
          IF (ai(i,j,linew) .le. min_a(ng) .or.                         &
     &        hi(i,j,linew) .le. min_h(ng)) THEN
            s0mk(i,j) = salt_top(i,j)
            t0mk(i,j) = temp_top(i,j)
            xtot = (1._r8-ai(i,j,linew))*wao(i,j)
          ELSE
!
!  MK89 version
!
            wio(i,j) = (qio(i,j)/rho0 +                                 &
     &                  cpw*cht(i,j)*(t0mk(i,j) - temp_top(i,j))) /     &
     &                 hfus1(i,j)
            xtot = (1._r8-ai(i,j,linew))*wao(i,j) +                     &
     &                    ai(i,j,linew) *wio(i,j)
!
            s0mk(i,j) =                                                 &
     &        (chs(i,j)*salt_top(i,j) +                                 &
     &         (       ai(i,j,linew) *wro(i,j) - xtot)*sice(i,j)) /     &
     &        (chs(i,j)               +                                 &
     &                 ai(i,j,linew) *wro(i,j) - xtot -                 &
     &          (1._r8-ai(i,j,linew))*stflx(i,j,isalt))
            s0mk(i,j) = MIN(MAX(s0mk(i,j), 0._r8), 40._r8)
            t0mk(i,j) = frln*s0mk(i,j)
          END IF
!
!  Modify heat/salt fluxes by accounting ice fluxes
!    Note that stflx follows the ROMS heat flux convention, which is in
!    unit of degC/m2, not watt/m2
!
#ifdef ICESHELF
          IF (zice(i,j).eq.0.0_r8) THEN
#endif
            fac_sf = 1.0_r8
#ifdef ICE_SHALLOW_LIMIT
!
!  Limit ice growth in shallow water
!
            hh = h(i,j)+Zt_avg1(i,j)
            clear = MAX(0.0_r8, hh-0.9_r8*hi(i,j,liold))
            fac_sf = MIN(1.0_r8, MAX(0.0_r8, clear-0.5_r8))
#endif
!
!  Modify heat flux
!
            IF (ai(i,j,linew).le.min_a(ng)) THEN
              stflx(i,j,itemp) = qao(i,j)
            ELSE
              stflx(i,j,itemp) =                                        &
     &           (1.0_r8-ai(i,j,linew))* qao(i,j)              +        &
     &          (        ai(i,j,linew) *(qio(i,j) - qswo(i,j)) -        &
     &           xtot*hfus1(i,j))*fac_sf
            END IF
!
!  Change heat flux back to ROMS convention
!
            stflx(i,j,itemp) = -rhocpr*stflx(i,j,itemp)
#ifdef MASKING
            stflx(i,j,itemp) = stflx(i,j,itemp)*rmask(i,j)
#endif
#ifdef WET_DRY
            stflx(i,j,itemp) = stflx(i,j,itemp)*rmask_wet(i,j)
#endif
!
!  Modify salt flux
!
#ifdef ICE_DIAGS
            sao(i,j) = stflx(i,j,isalt)
            sio(i,j) = (xtot-ai(i,j,linew)*wro(i,j))*                   &
     &                 (salt_top(i,j)-sice(i,j))*rhowr*fac_sf
            stflx(i,j,isalt) = sao(i,j) + sio(i,j)
#else
            stflx(i,j,isalt) = stflx(i,j,isalt) +                       &
     &        (xtot-ai(i,j,linew)*wro(i,j))*(salt_top(i,j)-sice(i,j))*  &
     &        rhowr*fac_sf
#endif
#ifdef BULK_FLUXES
!
!  Assume 100% drainage of rain and unfrozen snow
!
# ifdef ICE_SNOWFALL
            IF (snow(i,j).gt.0._r8 .and. wsf(i,j).eq.0._r8) THEN
              stflx(i,j,isalt) = stflx(i,j,isalt) -                     &
     &          ai(i,j,linew)*(rain(i,j)+snow(i,j))*salt_top(i,j)*rhowr
            ELSE
              stflx(i,j,isalt) = stflx(i,j,isalt) -                     &
     &          ai(i,j,linew)* rain(i,j)           *salt_top(i,j)*rhowr
            END IF
# else
            IF (rain(i,j).gt.0._r8 .and. wsf(i,j).eq.0._r8) THEN
              stflx(i,j,isalt) = stflx(i,j,isalt) -                     &
     &          ai(i,j,linew)* rain(i,j)           *salt_top(i,j)*rhowr
            END IF
# endif
#endif
!
!  Net ice production rate, positive for growth
!
            iomflx(i,j) = xtot - ai(i,j,linew)*wro(i,j) + wfr(i,j)
#ifdef MASKING
            stflx(i,j,isalt) = stflx(i,j,isalt)*rmask(i,j)
            iomflx(i,j) = iomflx(i,j)*rmask(i,j)
#endif
#ifdef WET_DRY
            stflx(i,j,isalt) = stflx(i,j,isalt)*rmask_wet(i,j)
            iomflx(i,j) = iomflx(i,j)*rmask_wet(i,j)
#endif
#ifdef ICESHELF
          ELSE
            iomflx(i,j) = 0.0_r8
# ifdef ICE_DIAGS
            sio(i,j) = 0.0_r8
# endif
          END IF
#endif
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Determine coverage/thicknes of sea ice
!-----------------------------------------------------------------------
!
      DO j = Jstr,Jend
        DO i = Istr,Iend
          IF (wao(i,j) .lt. 0.0_r8 ) THEN
            phi = 0.5_r8
          ELSE
            phi = 4._r8
          END IF
          hi(i,j,linew) = hi(i,j,linew) + dtice(ng)*                    &
     &      (        ai(i,j,linew) *(wio(i,j)-wai(i,j)) +               &
     &       (1.0_r8-ai(i,j,linew))* wao(i,j)           +               &
     &                               wfr(i,j))
!
          ai_tmp = ai(i,j,linew)
          ai(i,j,linew) = ai(i,j,linew) +                               &
     &                    dtice(ng)*(1.0_r8-ai(i,j,linew))*             &
     &                    (phi*wao(i,j)+4._r8*wfr(i,j))
          ai(i,j,linew) = MIN(ai(i,j,linew), max_a(ng))
#ifndef ICE_NO_SNOW
!
!  Adjust snow volume when ice melting out from under it
!
          IF (ai(i,j,linew) .lt. ai_tmp)                                &
     &      hsn(i,j,linew) = hsn(i,j,linew) * ai(i,j,linew) /           &
     &                       MAX(ai_tmp, eps)

# ifdef ICE_CONVSNOW
!
!  If snow base is below sea level, then raise the snow base to sea
!  level by converting some snow to ice (N.B. hstar is also weighted by
!  ai like hsn and hi)
!
          hstar = hsn(i,j,linew) -                                      &
     &            (rho0 - rhoice(ng))*hi(i,j,linew)/rhosnow_dry(ng)
          IF (hstar .gt. 0.0_r8) THEN
#  ifdef ICE_DIAGS
            wsni(i,j) = hstar*sice_ref/dtice(ng)
#  endif
            hsn(i,j,linew) = hsn(i,j,linew) - hstar*rhoice(ng)/rho0
            hi(i,j,linew) = hi(i,j,linew) + hstar*rhosnow_dry(ng)/rho0
!
! Add salt to ice (negative salt flux)
!
            stflx(i,j,isalt) = stflx(i,j,isalt) -                       &
     &                         hstar*sice_ref/dtice(ng)
          END IF
# endif
#endif
          IF (LnudgeAICLM(ng)) THEN
            cff = CLIMA(ng)%AInudgcof(i,j)
            ai(i,j,linew) = ai(i,j,linew)+                              &
     &        dtice(ng)*cff*(CLIMA(ng)%aiclm(i,j)  - ai(i,j,linew) )
            hi(i,j,linew) = hi(i,j,linew)+                              &
     &        dtice(ng)*cff*(CLIMA(ng)%hiclm(i,j)  - hi(i,j,linew) )
            hsn(i,j,linew) = hsn(i,j,linew)+                            &
     &        dtice(ng)*cff*(CLIMA(ng)%hsnclm(i,j) - hsn(i,j,linew))
          END IF
#ifdef MASKING
          ai(i,j,linew) = ai(i,j,linew)*rmask(i,j)
          hi(i,j,linew) = hi(i,j,linew)*rmask(i,j)
#endif
#ifdef ICESHELF
          IF (zice(i,j).ne.0.0_r8) THEN
            ai(i,j,linew) = 0.0_r8
            hi(i,j,linew) = 0.0_r8
# ifdef ICE_DIAGS
            wsni(i,j) = 0.0_r8
#  endif
          END IF
#endif
!
!  Determine age of the sea ice
!
!  Case 1 - new ice
!
          IF (ageice(i,j,linew).le.0.0_r8   .and.                       &
     &            hi(i,j,linew).gt.min_h(ng)) THEN
            ageice(i,j,linew) = dtice(ng)/86400._r8
!
!  Case 2 - existing ice gets older
!
          ELSEIF(ageice(i,j,linew).gt.0.0_r8   .and.                    &
     &               hi(i,j,linew).gt.min_h(ng)) THEN
            ageice(i,j,linew) = ageice(i,j,linew)+dtice(ng)/86400._r8
!
!  Case 3 - all ice in cell melted or is open water and stays open water
!
          ELSE
            ageice(i,j,linew) = 0.0_r8
          END IF
          hage(i,j,linew) = hi(i,j,linew)*ageice(i,j,linew)
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Limit variable values.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
          ai(i,j,linew)  = MAX(MIN(ai(i,j,linew),max_a(ng)),  0.0_r8)
          hi(i,j,linew)  = MAX(hi(i,j,linew),                 0.0_r8)
          hsn(i,j,linew) = MAX(hsn(i,j,linew),                0.0_r8)
          ti(i,j,linew)  = MAX(ti(i,j,linew),               -70.0_r8)
          IF (hi(i,j,linew)     .le. 0.0_r8) ai(i,j,linew)     = 0.0_r8
          IF (ai(i,j,linew)     .le. 0.0_r8) hi(i,j,linew)     = 0.0_r8
          IF (ageice(i,j,linew) .le. 0.0_r8) ageice(i,j,linew) = 0.0_r8
          IF (hage(i,j,linew)   .le. 0.0_r8) hage(i,j,linew)   = 0.0_r8
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Exchange boundary information.
!-----------------------------------------------------------------------
!
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  tis)
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  coef_ice_heat)
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  rhs_ice_heat)
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  stflx(:,:,isalt))
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  stflx(:,:,itemp))
!
      CALL i2dbc_ice  (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 liold, linew,                                    &
     &                 ui, vi, ai, hi, hsn)
!
      CALL i2dbc_tice (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 liold, linew,                                    &
     &                 ui, vi, hi, ti, enthalpi)
!
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  ageice(:,:,linew))
      CALL bc_r2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  hage(:,:,linew))
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          ai(:,:,linew))
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          hi(:,:,linew))
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          hsn(:,:,linew))
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          ti(:,:,linew))
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          enthalpi(:,:,linew))
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          ageice(:,:,linew))
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          hage(:,:,linew))
      END IF
#ifdef DISTRIBUTE
!
      CALL mp_exchange2d (ng, tile, iNLM, 3,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    ai(:,:,linew), hi(:,:,linew), hsn(:,:,linew))
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    ti(:,:,linew), enthalpi(:,:,linew),           &
     &                    ageice(:,:,linew), hage(:,:,linew))
#endif

      RETURN
      END SUBROUTINE ice_thermo_tile
