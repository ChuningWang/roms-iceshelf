#include "cppdefs.h"
      MODULE ccsm_flux_mod
#if defined BULK_FLUXES && defined CCSM_FLUXES
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the bulk parameterization of surface wind     !
!  stress and surface net heat fluxes.                                 !
!                                                                      !
!  References:                                                         !
!                                                                      !
!       CCSM's flux_mod.F90 by B. Kauffman                             !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: ccsm_flux

      CONTAINS
!
!***********************************************************************
      SUBROUTINE ccsm_flux (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid
      USE mod_mixing
      USE mod_ocean
      USE mod_stepping
# ifdef ICE_MODEL
      USE mod_ice
# endif
# ifdef EVAP_SHALLOW
      USE mod_coupling
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 17, __LINE__, __FILE__)
# endif
      CALL ccsm_flux_tile (ng, tile,                                    &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     IminS, ImaxS, JminS, JmaxS,                  &
     &                     nrhs(ng),                                    &
# ifdef ICE_MODEL
     &                     liold(ng),                                   &
     &                     linew(ng),                                   &
# endif
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
     &                     GRID(ng) % umask,                            &
     &                     GRID(ng) % vmask,                            &
# endif
# ifdef WET_DRY
     &                     GRID(ng) % rmask_wet,                        &
     &                     GRID(ng) % umask_wet,                        &
     &                     GRID(ng) % vmask_wet,                        &
# endif
     &                     OCEAN(ng) % t,                               &
# ifdef WIND_MINUS_CURRENT
     &                     OCEAN(ng) % u,                               &
     &                     OCEAN(ng) % v,                               &
# endif
     &                     FORCES(ng) % Hair,                           &
     &                     FORCES(ng) % Pair,                           &
     &                     FORCES(ng) % Tair,                           &
     &                     FORCES(ng) % Uwind,                          &
     &                     FORCES(ng) % Vwind,                          &
# ifdef CLOUDS
     &                     FORCES(ng) % cloud,                          &
# endif
# ifdef COARE_TAYLOR_YELLAND
     &                     FORCES(ng) % Hwave,                          &
# endif
# if defined COARE_TAYLOR_YELLAND || defined COARE_OOST
     &                     FORCES(ng) % Pwave_top,                      &
# endif
# if !defined DEEPWATER_WAVES      && \
     (defined COARE_TAYLOR_YELLAND || defined COARE_OOST)
     &                     FORCES(ng) % Lwave,                          &
# endif
# ifdef EVAP_SHALLOW
     &                     GRID(ng) % h,                                &
     &                     COUPLING(ng) % Zt_avg1,                      &
# endif
     &                     FORCES(ng) % rain,                           &
     &                     FORCES(ng) % lhflx,                          &
     &                     FORCES(ng) % lrflx,                          &
     &                     FORCES(ng) % shflx,                          &
     &                     FORCES(ng) % srflx,                          &
# ifdef ICE_MODEL
     &                     ICE(ng) % ai,                                &
     &                     ICE(ng) % hi,                                &
     &                     ICE(ng) % rfaci,                             &
     &                     FORCES(ng) % sustr_aw,                       &
     &                     FORCES(ng) % svstr_aw,                       &
     &                     FORCES(ng) % qao_n,                          &
     &                     FORCES(ng) % tau_aix_n,                      &
     &                     FORCES(ng) % tau_aiy_n,                      &
#  ifdef ICE_THERMO
     &                     ICE(ng) % hsn,                               &
     &                     ICE(ng) % tis,                               &
#   ifdef ICE_SNOWFALL
     &                     FORCES(ng) % snow,                           &
#   endif
     &                     FORCES(ng) % qai_n,                          &
     &                     FORCES(ng) % sr_in_i,                        &
     &                     FORCES(ng) % sr_th_i,                        &
     &                     FORCES(ng) % qswi_n,                         &
     &                     ICE(ng) % coef_ice_heat,                     &
     &                     ICE(ng) % rhs_ice_heat,                      &
     &                     FORCES(ng) % snow_n,                         &
#  endif
#  ifdef ICE_ALBEDO
#   ifdef ICE_CCSM_SHORTWAVE
     &                     FORCES(ng) % frswp,                          &
#   endif
     &                     FORCES(ng) % alb,                            &
     &                     FORCES(ng) % alb_i,                          &
#   ifdef ICE_ALB_DIRDIFF
     &                     FORCES(ng) % cawdir,                         &
#   endif
#  endif
#  ifdef ICE_DIAGS
     &                     FORCES(ng) % lr_dn,                          &
     &                     FORCES(ng) % sr_dn,                          &
     &                     FORCES(ng) % lat_i,                          &
     &                     FORCES(ng) % sen_i,                          &
     &                     FORCES(ng) % lr_up_i,                        &
     &                     FORCES(ng) % sr_up_i,                        &
     &                     FORCES(ng) % ssflx_l,                        &
#  endif
# endif
# ifdef EMINUSP
     &                     FORCES(ng) % evap,                           &
# endif
     &                     FORCES(ng) % stflux,                         &
     &                     FORCES(ng) % sustr,                          &
     &                     FORCES(ng) % svstr)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 17, __LINE__, __FILE__)
# endif
!
      RETURN
      END SUBROUTINE ccsm_flux
!
!***********************************************************************
      SUBROUTINE ccsm_flux_tile (ng, tile,                              &
     &                           LBi, UBi, LBj, UBj,                    &
     &                           IminS, ImaxS, JminS, JmaxS,            &
     &                           nrhs,                                  &
# ifdef ICE_MODEL
     &                           liold, linew,                          &
# endif
# ifdef MASKING
     &                           rmask, umask, vmask,                   &
# endif
# ifdef WET_DRY
     &                           rmask_wet, umask_wet, vmask_wet,       &
# endif
     &                           t,                                     &
# ifdef WIND_MINUS_CURRENT
     &                           u, v,                                  &
# endif
     &                           Hair, Pair, Tair, Uwind, Vwind,        &
# ifdef CLOUDS
     &                           cloud,                                 &
# endif
# ifdef COARE_TAYLOR_YELLAND
     &                           Hwave,                                 &
# endif
# if defined COARE_TAYLOR_YELLAND || defined COARE_OOST
     &                           Pwave_top,                             &
# endif
# if !defined DEEPWATER_WAVES      && \
     (defined COARE_TAYLOR_YELLAND || defined COARE_OOST)
     &                           Lwave,                                 &
# endif
# ifdef EVAP_SHALLOW
     &                           h, Zt_avg1,                            &
# endif
     &                           rain, lhflx, lrflx, shflx, srflx,      &
# ifdef ICE_MODEL
     &                           ai, hi, rfaci, sustr_aw, svstr_aw,     &
     &                           qao_n, tau_aix_n, tau_aiy_n,           &
#  ifdef ICE_THERMO
     &                           hsn, tis,                              &
#   ifdef ICE_SNOWFALL
     &                           snow,                                  &
#   endif
     &                           qai_n, sr_in_i, sr_th_i, qswi_n,       &
     &                           coef_ice_heat, rhs_ice_heat, snow_n,   &
#  endif
#  ifdef ICE_ALBEDO
#   ifdef ICE_CCSM_SHORTWAVE
     &                           frswp,                                 &
#   endif
     &                           alb,                                   &
     &                           alb_i,                                 &
#   ifdef ICE_ALB_DIRDIFF
     &                           cawdir,                                &
#   endif
#  endif
#  ifdef ICE_DIAGS
     &                           lr_dn, sr_dn, lat_i, sen_i,            &
     &                           lr_up_i, sr_up_i, ssflx_l,             &
#  endif
# endif
# ifdef EMINUSP
     &                           evap,                                  &
# endif
     &                           stflux, sustr, svstr)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
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
      integer, intent(in) :: nrhs
# ifdef ICE_MODEL
      integer, intent(in) :: liold
      integer, intent(in) :: linew
# endif
!
# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:,LBj:)
      real(r8), intent(in) :: umask_wet(LBi:,LBj:)
      real(r8), intent(in) :: vmask_wet(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: t(LBi:,LBj:,:,:,:)
#  ifdef WIND_MINUS_CURRENT
      real(r8), intent(in) :: u(LBi:,LBj:,:,:)
      real(r8), intent(in) :: v(LBi:,LBj:,:,:)
#  endif
      real(r8), intent(in) :: Hair(LBi:,LBj:)
      real(r8), intent(in) :: Pair(LBi:,LBj:)
      real(r8), intent(in) :: Tair(LBi:,LBj:)
#  ifdef WIND_MINUS_CURRENT
      real(r8), intent(inout) :: Uwind(LBi:,LBj:)
      real(r8), intent(inout) :: Vwind(LBi:,LBj:)
#  else
      real(r8), intent(in) :: Uwind(LBi:,LBj:)
      real(r8), intent(in) :: Vwind(LBi:,LBj:)
#  endif
#  ifdef CLOUDS
      real(r8), intent(in) :: cloud(LBi:,LBj:)
#  endif
#  ifdef COARE_TAYLOR_YELLAND
      real(r8), intent(in) :: Hwave(LBi:,LBj:)
#  endif
#  if defined COARE_TAYLOR_YELLAND || defined COARE_OOST
      real(r8), intent(in) :: Pwave_top(LBi:,LBj:)
#  endif
#  if !defined DEEPWATER_WAVES      && \
     (defined COARE_TAYLOR_YELLAND || defined COARE_OOST)
      real(r8), intent(in) :: Lwave(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: rain(LBi:,LBj:)
#  ifdef EVAP_SHALLOW
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: Zt_avg1(LBi:,LBj:)
#  endif

      real(r8), intent(inout) :: lhflx(LBi:,LBj:)
      real(r8), intent(inout) :: lrflx(LBi:,LBj:)
      real(r8), intent(inout) :: shflx(LBi:,LBj:)
      real(r8), intent(inout) :: srflx(LBi:,LBj:)
      real(r8), intent(inout) :: stflux(LBi:,LBj:,:)

#  ifdef EMINUSP
      real(r8), intent(out) :: evap(LBi:,LBj:)
#  endif
      real(r8), intent(out) :: sustr(LBi:,LBj:)
      real(r8), intent(out) :: svstr(LBi:,LBj:)

#  ifdef ICE_MODEL
      real(r8), intent(in) :: ai(LBi:,LBj:,:)
      real(r8), intent(in) :: hi(LBi:,LBj:,:)
      real(r8), intent(out) :: rfaci(LBi:,LBj:)
      real(r8), intent(out) :: sustr_aw(LBi:,LBj:)
      real(r8), intent(out) :: svstr_aw(LBi:,LBj:)
      real(r8), intent(out) :: qao_n(LBi:,LBj:)
      real(r8), intent(out) :: tau_aix_n(LBi:,LBj:)
      real(r8), intent(out) :: tau_aiy_n(LBi:,LBj:)

#   ifdef ICE_THERMO
      real(r8), intent(in) :: hsn(LBi:,LBj:,:)
      real(r8), intent(in) :: tis(LBi:,LBj:)
#    ifdef ICE_SNOWFALL
      real(r8), intent(in) :: snow(LBi:,LBj:)
#    endif
      real(r8), intent(out) :: qai_n(LBi:,LBj:)
      real(r8), intent(out) :: sr_in_i(LBi:,LBj:)
      real(r8), intent(out) :: sr_th_i(LBi:,LBj:)
      real(r8), intent(out) :: qswi_n(LBi:,LBj:)
      real(r8), intent(out) :: coef_ice_heat(LBi:,LBj:)
      real(r8), intent(out) :: rhs_ice_heat(LBi:,LBj:)
      real(r8), intent(out) :: snow_n(LBi:,LBj:)
#   endif

#   ifdef ICE_ALBEDO
#    ifdef ICE_CCSM_SHORTWAVE
      real(r8), intent(in) :: frswp(LBi:,LBj:)
#    endif
      real(r8), intent(inout) :: alb(LBi:,LBj:)
      real(r8), intent(inout) :: alb_i(LBi:,LBj:)
#    ifdef ICE_ALB_DIRDIFF
      real(r8), intent(inout) :: cawdir(LBi:,LBj:)
#    endif
#   endif

#   ifdef ICE_DIAGS
      real(r8), intent(out) :: lr_dn(LBi:,LBj:)
      real(r8), intent(out) :: sr_dn(LBi:,LBj:)
      real(r8), intent(out) :: lat_i(LBi:,LBj:)
      real(r8), intent(out) :: sen_i(LBi:,LBj:)
      real(r8), intent(out) :: lr_up_i(LBi:,LBj:)
      real(r8), intent(out) :: sr_up_i(LBi:,LBj:)
      real(r8), intent(out) :: ssflx_l(LBi:,LBj:)
#   endif
#  endif

# else
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask_wet(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#  ifdef WIND_MINUS_CURRENT
      real(r8), intent(in) :: u(LBi:UBi,LBj:UBj,N(ng),3)
      real(r8), intent(in) :: v(LBi:UBi,LBj:UBj,N(ng),3)
#  endif
      real(r8), intent(in) :: Hair(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Pair(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Tair(LBi:UBi,LBj:UBj)
#  ifdef WIND_MINUS_CURRENT
      real(r8), intent(inout) :: Uwind(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: Vwind(LBi:UBi,LBj:UBj)
#  else
      real(r8), intent(in) :: Uwind(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Vwind(LBi:UBi,LBj:UBj)
#  endif
#  ifdef CLOUDS
      real(r8), intent(in) :: cloud(LBi:UBi,LBj:UBj)
#  endif
#  ifdef COARE_TAYLOR_YELLAND
      real(r8), intent(in) :: Hwave(LBi:UBi,LBj:UBj)
#  endif
#  if defined COARE_TAYLOR_YELLAND || defined COARE_OOST
      real(r8), intent(in) :: Pwave_top(LBi:UBi,LBj:UBj)
#  endif
#  if !defined DEEPWATER_WAVES      && \
     (defined COARE_TAYLOR_YELLAND || defined COARE_OOST)
      real(r8), intent(in) :: Lwave(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: rain(LBi:UBi,LBj:UBj)
#  ifdef EVAP_SHALLOW
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Zt_avg1(LBi:UBi,LBj:UBj)
#  endif

      real(r8), intent(inout) :: lhflx(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: lrflx(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: shflx(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: srflx(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: stflux(LBi:UBi,LBj:UBj,NT(ng))

#  ifdef EMINUSP
      real(r8), intent(out) :: evap(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(out) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: svstr(LBi:UBi,LBj:UBj)

#  ifdef ICE_MODEL
      real(r8), intent(in) :: ai(LBi:,LBj:,:)
      real(r8), intent(in) :: hi(LBi:,LBj:,:)
      real(r8), intent(out) :: rfaci(LBi:,LBj:)
      real(r8), intent(out) :: sustr_aw(LBi:,LBj:)
      real(r8), intent(out) :: svstr_aw(LBi:,LBj:)
      real(r8), intent(out) :: qao_n(LBi:,LBj:)
      real(r8), intent(out) :: tau_aix_n(LBi:,LBj:)
      real(r8), intent(out) :: tau_aiy_n(LBi:,LBj:)

#   ifdef ICE_THERMO
      real(r8), intent(in) :: hsn(LBi:,LBj:,:)
      real(r8), intent(in) :: tis(LBi:,LBj:)
#    ifdef ICE_SNOWFALL
      real(r8), intent(in) :: snow(LBi:,LBj:)
#    endif
      real(r8), intent(out) :: qai_n(LBi:,LBj:)
      real(r8), intent(out) :: sr_in_i(LBi:,LBj:)
      real(r8), intent(out) :: sr_th_i(LBi:,LBj:)
      real(r8), intent(out) :: qswi_n(LBi:,LBj:)
      real(r8), intent(out) :: coef_ice_heat(LBi:,LBj:)
      real(r8), intent(out) :: rhs_ice_heat(LBi:,LBj:)
      real(r8), intent(out) :: snow_n(LBi:,LBj:)
#   endif

#   ifdef ICE_ALBEDO
#    ifdef ICE_CCSM_SHORTWAVE
      real(r8), intent(in) :: frswp(LBi:,LBj:)
#    endif
      real(r8), intent(inout) :: alb(LBi:,LBj:)
      real(r8), intent(inout) :: alb_i(LBi:,LBj:)
#     ifdef ICE_ALB_DIRDIFF
      real(r8), intent(inout) :: cawdir(LBi:,LBj:)
#    endif
#   endif

#   ifdef ICE_DIAGS
      real(r8), intent(out) :: lr_dn(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: sr_dn(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: lat_i(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: sen_i(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: lr_up_i(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: sr_up_i(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: ssflx_l(LBi:UBi,LBj:UBj)
#   endif
#  endif

# endif
!
!  Local variable declarations.
!
      integer :: i, j, listp
      integer :: month

      real(r8), parameter :: eps = 1.0E-20_r8
      real(r8), parameter :: r3 = 1.0_r8/3.0_r8
      real(r8), parameter :: Ch = 1.75e-3
      real(r8), parameter :: Ce = 1.75e-3
!
!  Ratio of molecular weight of water to dry air
!
      real(r8), parameter :: epsilon = 0.622_r8

      real(r8) :: Hscale, Hscale_inv
      real(r8) :: RH, cff1, cff2
      real(r8) :: cff
# ifdef EVAP_SHALLOW
      real(r8), parameter :: coef_shallow = 5.28_r8
!     real(r8), parameter :: coef_shallow = 4.69_r8
      real(r8) :: cffe
# endif
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Uair
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Vair
      real(r8) :: fsnow

! ------------------------------------------------------------------
! !DESCRIPTION:
!     wrapper to atm/ocn flux calculation
!
! !REMARKS:
!     All data must be on the ocean domain (note: a domain includes a
!     particular decomposition).
!
! !REVISION HISTORY:
!     2002-Jun-10 - B. Kauffman - first version
!
! ------------------------------------------------------------------
!
! !IROUTINE: srfflx_ao -- internal atm/ocn flux calculation
!
! !DESCRIPTION:
!
!     Internal atm/ocn flux calculation
!
! !REVISION HISTORY:
!     2002-Jun-10 - B. Kauffman - brought in from cpl5.
!     2003-Apr-02 - B. Kauffman - taux & tauy now utilize ocn velocity
!     2003-Apr-02 - B. Kauffman - tref,qref,duu10n mods as in Bill Large
!
!INTERFACE: ------------------------------------------------------------
!
!SUBROUTINE srfflx_ao_tile(blk_ZX, Uwind, Vwind, Tair, Hair, rhoAir,    &
!    &                     u, v, t, rmask, shflx, lhflx, lwup,          &
!    &                     evap, taux, tauy, tref, qref, duu10n)
!
! INPUT/OUTPUT PARAMETERS:
!
!--- input arguments --------------------------------
! integer(IN) ,intent(in) :: rmask(imax) ! ocn domain mask 0
! real(R8), intent(in) :: blk_ZX (imax)  ! atm level height  (m)
! real(R8), intent(in) :: Uwind (imax)   ! atm u wind        (m/s)
! real(R8), intent(in) :: Vwind (imax)   ! atm v wind        (m/s)
! real(R8), intent(in) :: Tair (imax)    ! atm potential T   (K)
! real(R8), intent(in) :: Hair (imax)    ! atm rel humidity  (frac)
! real(R8), intent(in) :: rhoAir (imax)  ! atm air density   (kg/m^3)
! real(R8), intent(in) :: u (imax)       ! ocn u-velocity    (m/s)
! real(R8), intent(in) :: v (imax)       ! ocn v-velocity    (m/s)
! real(R8), intent(in) :: t (imax)       ! ocn temperature   (K)
!
!--- output arguments -------------------------------
! real(R8), intent(out) :: shflx (imax)  ! heat flux: sensible    (W/m^2)
! real(R8), intent(out) :: lhflx (imax)  ! heat flux: latent      (W/m^2)
! real(R8), intent(out) :: lwup (imax)   ! heat flux: lw upward   (W/m^2)
! real(R8), intent(out) :: evap (imax)   ! water flux: evap  ((kg/s)/m^2)
! real(R8), intent(out) :: taux (imax)   ! surface stress, zonal      (N)
! real(R8), intent(out) :: tauy (imax)   ! surface stress, maridional (N)
! real(R8), intent(out) :: tref (imax)   ! diag:  2m ref height T     (K)
! real(R8), intent(out) :: qref (imax)   ! diag:  2m ref humidity (kg/kg)
! real(R8), intent(out) :: duu10n (imax) ! diag: 10m wspeed sqrd (m/s)^2
!
!EOP
!
!--- local constants --------------------------------
!
      real(R8),parameter :: umin  =  0.5  ! minimum wind speed   (m/s)
      real(R8),parameter :: zref  = 10.0  ! ref height           (m)
      real(R8),parameter :: ztref =  2.0  ! ref height for air T (m)
!
!--- local variables --------------------------------
!
      real(r8) :: lwup    ! upward longwave radiation
      real(r8) :: vmag    ! surface wind magnitude   (m/s)
      real(r8) :: thvbot  ! virtual temperature      (K)
      real(r8) :: ssq     ! sea surface humidity     (kg/kg)
      real(r8) :: Tseak   ! SST in Kelvins           (K)
      real(r8) :: delt    ! potential T difference   (K)
      real(r8) :: delq    ! humidity difference      (kg/kg)
      real(r8) :: stable  ! stability factor
      real(r8) :: rdn     ! sqrt of neutral exchange coeff (momentum)
      real(r8) :: rhn     ! sqrt of neutral exchange coeff (heat)
      real(r8) :: ren     ! sqrt of neutral exchange coeff (water)
      real(r8) :: rd      ! sqrt of exchange coefficient (momentum)
      real(r8) :: re      ! sqrt of exchange coefficient (water)
      real(r8) :: ustar   ! ustar
      real(r8) :: qstar   ! qstar
      real(r8) :: tstar   ! tstar
      real(r8) :: hol     ! H (at blk_ZX) over L
      real(r8) :: xsq     ! ?
      real(r8) :: xqq     ! ?
      real(r8) :: psimh   ! stability function at blk_ZX (momentum)
      real(r8) :: psixh   ! stability function at blk_ZX (heat/water)
      real(r8) :: psix2   ! stability function at ztref ref height
      real(r8) :: alz     ! ln(blk_ZX/zref)
      real(r8) :: u10n    ! 10m neutral wind
      real(r8) :: tau     ! stress at blk_ZX
      real(r8) :: cpair   ! specific heat of moist air
      real(r8) :: fac     ! vertical interpolation factor
      real(r8) :: Hlv     ! Latent heat of vaporization at surface
      real(r8) :: Qair    ! Specific humidity [kg/kg]
!
      real(r8), dimension(IminS:ImaxS) :: rhoAir
      real(r8), dimension(IminS:ImaxS) :: TairK
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: LHeat
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: SHeat
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Taux
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Tauy
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: u10n_tmp
!
# ifdef ICE_MODEL
      real(r8) :: transw, transi, rtransi
      real(r8) :: ice_thick
      real(r8) :: snow_thick
!
! Visual fraction of total shortwave (Aumont et al., 2015)
!
      real(r8), parameter :: fv=0.43_r8
#  ifdef ICE_THERMO
      real(r8), parameter :: Io_frac = 0.17_r8   ! from MU
      real(r8) :: qswi, qlwi, qlh_i, qsh_i
      real(r8) :: le_i, dq_i,fqlat1, sfc_temp, slp, Qsati
      real(r8) :: vap_p_i
#  endif
#  ifdef ICE_ALB_DIRDIFF
      real(r8), parameter :: albw_d=0.065_r8
      real(r8) :: cawdif
#  endif
#  ifdef ICE_CCSM_SHORTWAVE
!
! Vis extnctn coef in ice (1/m); CESM2/CICE5.1 default,
! DuVivier et al., 2018 (cf. 0.73 in Briegleb et al., 2004)
!
      real(r8), parameter :: kappav = 1.4_r8
#  endif
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Taux_Ice
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Tauy_Ice
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: i0frac
#  ifdef ICE_THERMO
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Coef_Ice
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: RHS_Ice
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Qai
#  endif
# endif

!--- local functions --------------------------------
      real(r8)    :: qsat    ! function: satur humididty of air (kg/m^3)
      real(r8)    :: cdn     ! function: neutral drag coeff at 10m
      real(r8)    :: psimhu  ! function: unstable part of psimh
      real(r8)    :: psixhu  ! function: unstable part of psimx
      real(r8)    :: tsk     ! water temperature (K)
      real(r8)    :: tak     ! air temperature (K)
      real(r8)    :: Tk      ! dummy arg ~ air temperature (K)
      real(r8)    :: Umps    ! dummy arg ~ wind velocity (m/s)
      real(r8)    :: xd      ! dummy arg ~ ?

!
! Stefan-Boltzmann constant ~ W/m^2/K^4
!
      real(r8), parameter :: shr_const_stebol  = 5.67e-8_r8
!
! Boltzmann's constant ~ J/K/molecule
!
      real(r8), parameter :: shr_const_boltz   = 1.38065e-23_r8
!
! Avogadro's number ~ molecules/kmole
!
      real(r8), parameter :: shr_const_avogad  = 6.02214e26_r8
!
! Universal gas constant ~ J/K/kmole
!
      real(r8), parameter :: shr_const_rgas =                           &
     &         shr_const_avogad*shr_const_boltz
!
! molecular weight dry air ~ kg/kmole
!
      real(r8), parameter :: shr_const_mwdair  = 28.966_r8
!
! molecular weight water vapor
!
      real(r8), parameter :: shr_const_mwwv = 18.016_r8
!
! Water vapor gas constant ~ J/K/kg
!
      real(r8), parameter :: shr_const_rwv =                            &
     &         shr_const_rgas/shr_const_mwwv
!
! Dry air gas constant     ~ J/K/kg
!
      real(r8), parameter :: shr_const_rdair   =                        &
     &         shr_const_rgas/shr_const_mwdair
      real(r8), parameter :: shr_const_zvir    =                        &
     &         (shr_const_rwv/shr_const_rdair)-1.0_r8
!
! Von Karman constant
!
      real(r8), parameter :: shr_const_karman  = 0.4_r8
!
! specific heat of dry air   ~ J/kg/K
!
      real(r8), parameter :: shr_const_cpdair  = 1.00464e3_r8
!
! specific heat of water vap ~ J/kg/K
!
      real(r8), parameter :: shr_const_cpwv    = 1.810e3_R8
!
! CPWV/CPDAIR - 1.0
!
      real(r8), parameter :: shr_const_cpvir   =                        &
     &         (shr_const_cpwv/shr_const_cpdair)-1.0_r8
!
! latent heat of evaporation ~ J/kg
!
      real(r8), parameter :: shr_const_latvap  = 2.501e6_r8


      qsat(Tk)   = 640380.0 / EXP(5107.4/Tk)
      cdn(Umps)  = 0.0027 / Umps + 0.000142 + 0.0000764 * Umps
      psimhu(xd) = LOG((1.0+xd*(2.0+xd))*(1.0+xd*xd)/8.0) -             &
     &             2.0*ATAN(xd) + 1.571
      psixhu(xd) = 2.0 * LOG((1.0 + xd*xd)/2.0)

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
! PURPOSE:
!   computes atm/ocn surface fluxes
!
! NOTES:
!   o all fluxes are positive downward
!   o net heat flux = net sw + lw up + lw down + shflx + lhflx
!   o here, tstar = <WT>/U*, and qstar = <WQ>/U*.
!   o wind speeds should all be above a minimum speed (eg. 1.0 m/s)
!
! ASSUMPTIONS:
!   o Neutral 10m drag coeff: cdn = .0027/U10 + .000142 + .0000764 U10
!   o Neutral 10m stanton number: ctn = .0327 sqrt(cdn), unstable
!                                 ctn = .0180 sqrt(cdn), stable
!   o Neutral 10m dalton number:  cen = .0346 sqrt(cdn)
!   o The saturation humidity of air at T(K): qsat(T)  (kg/m^3)
!-----------------------------------------------------------------------
!
# if defined ICE_MODEL
      IF (PerfectRST(ng) .and. iic(ng).eq.ntstart(ng)) THEN
        listp=liold
      ELSE
        listp=linew
      END IF
# endif

# ifdef WIND_MINUS_CURRENT
!
!  Modify near-surface (2m or 10m) effective winds by subtracting the
!  ocean surface current (J Wilkin). See:
!
!  Bye, J.A.T. and J.-O. Wolff, 1999: Atmosphere-ocean momentum exchange
!  in general circulation models. J. Phys. Oceanogr. 29, 671-691.
!
      DO j=Jstr-1,JendR
        DO i=Istr-1,IendR
          Uair(i,j)=Uwind(i,j)-                                         &
     &              0.5_r8*(u(i,j,N(ng),nrhs)+u(i+1,j,N(ng),nrhs))
          Vair(i,j)=Vwind(i,j)-                                         &
     &              0.5_r8*(v(i,j,N(ng),nrhs)+v(i,j+1,N(ng),nrhs))
        END DO
      END DO
# else
!
!  Load wind components to local arrays.
!
      DO j=Jstr-1,JendR
        DO i=Istr-1,IendR
          Uair(i,j)=Uwind(i,j)
          Vair(i,j)=Vwind(i,j)
        END DO
      END DO
# endif

      Hscale=rho0*Cp
      Hscale_inv=1.0_r8/Hscale
!
!  Note that this loop needs to be cleaned of all global arrays for
!  OpenMP.
!
      DO j=JstrR,JendR
        DO i=IstrR,IendR
!
!  Compute specific humidities (kg/kg).
!
          IF (Hair(i,j).lt.2.0_r8) THEN  !RH fraction
            cff  = (1.0007_r8+3.46E-6_r8*Pair(i,j))*6.1121_r8*          &
     &             EXP(17.502_r8*Tair(i,j)/(240.97_r8+Tair(i,j)))
            cff  = cff*Hair(i,j)
            Qair = 0.62197_r8*(cff/(Pair(i,j)-0.378_r8*cff))
          ELSE  !RH input was actually specific humidity in g/kg
            Qair=Hair(i,j)/1000.0_r8  !Spec Hum (kg/kg)
          END IF
!
!--- compute some needed quantities ---
!
          vmag     = MAX(umin, SQRT( (Uair(i,j)**2 + Vair(i,j)**2)) )
          TairK(i) = Tair(i,j) + 273.16_r8
          TseaK    = t(i,j,N(ng),nrhs,itemp) + 273.16_r8
          thvbot   = TairK(i) * (1.0 + shr_const_zvir * Qair)
!
!  Virtual temp (K)
!
          rhoAir(i) = Pair(i,j)*100.0_r8 /                              &
     &                (blk_Rgas*TairK(i)*(1.0_r8+0.61_r8*Qair))
!
!  Moist air density (kg/m3).
!
          ssq    = 0.98_r8 * qsat(TseaK) / rhoAir(i)
!
!  Sea surf hum (kg/kg)
!
          delt   = Tair(i,j) - t(i,j,N(ng),nrhs,itemp)
!
!  Pot temp diff (K)
!
          delq   = Qair - ssq
!
!  Spec hum dif (kg/kg)
!
          alz    = LOG(blk_ZW(ng)/zref)
          cpair  = shr_const_cpdair*(1.0 + shr_const_cpvir*ssq)
!
!  first estimate of Z/L and ustar, tstar and qstar
!
!--- neutral coefficients, z/L = 0.0 ---
!
          stable = 0.5_r8 + SIGN(0.5_r8 , delt)
          rdn    = SQRT(cdn(vmag))
          rhn    = (1.0_r8-stable) * 0.0327_r8 + stable * 0.018_r8
          ren    = 0.0346_r8
!
!--- ustar, tstar, qstar ---
!
          ustar = rdn * vmag
          tstar = rhn * delt
          qstar = ren * delq
!
!--- compute stability & evaluate all stability functions ---
!
          hol    = shr_const_karman*g*blk_ZT(ng)*                       &
     &             (tstar/thvbot+qstar/(1.0_r8/shr_const_zvir+Qair)) /  &
     &             ustar**2
          hol    = SIGN( MIN(ABS(hol),10.0_r8), hol )
          stable = 0.5_r8 + SIGN(0.5_r8 , hol)
          xsq    = MAX(SQRT(ABS(1.0_r8 - 16.0_r8*hol)) , 1.0_r8)
          xqq    = SQRT(xsq)
          psimh  = -5.0_r8*hol*stable + (1.0_r8-stable)*psimhu(xqq)
          psixh  = -5.0_r8*hol*stable + (1.0_r8-stable)*psixhu(xqq)
!
!--- shift wind speed using old coefficient ---
!
          rd   = rdn / (1.0_r8 + rdn/shr_const_karman*(alz-psimh))
          u10n = vmag * rd / rdn
!
!--- update transfer coeffs at 10m and neutral stability ---
!
          rdn = SQRT(cdn(u10n))
          ren = 0.0346_r8
          rhn = (1.0_r8-stable)*0.0327_r8 + stable * 0.018_r8
!
!--- shift all coeffs to measurement height and stability ---
!
          rd = rdn / (1.0_r8 + rdn/shr_const_karman*(alz-psimh))
          rh = rhn / (1.0_r8 + rhn/shr_const_karman*(alz-psixh))
          re = ren / (1.0_r8 + ren/shr_const_karman*(alz-psixh))
!
!--- update ustar, tstar, qstar using updated, shifted coeffs --
!
          ustar = rd * vmag
          tstar = rh * delt
          qstar = re * delq
!
!------------------------------------------------------------
! iterate to converge on Z/L, ustar, tstar and qstar
!------------------------------------------------------------
!
!--- compute stability & evaluate all stability functions ---
!
          hol    = shr_const_karman*g*blk_ZQ(ng)*                       &
     &             (tstar/thvbot+qstar/(1.0/shr_const_zvir+Qair)) /     &
     &             ustar**2
          hol    = SIGN( MIN(ABS(hol),10.0_r8), hol )
          stable = 0.5_r8 + SIGN(0.5_r8 , hol)
          xsq    = MAX(SQRT(ABS(1.0_r8 - 16.0_r8*hol)) , 1.0_r8)
          xqq    = SQRT(xsq)
          psimh  = -5.0_r8*hol*stable + (1.0_r8-stable)*psimhu(xqq)
          psixh  = -5.0_r8*hol*stable + (1.0_r8-stable)*psixhu(xqq)
!
!--- shift wind speed using old coeffs ---
!
          rd   = rdn / (1.0_r8 + rdn/shr_const_karman*(alz-psimh))
          u10n = vmag * rd/rdn
!
!--- update transfer coeffs at 10m and neutral stability ---
!
          rdn = SQRT(cdn(u10n))
          ren = 0.0346_r8
          rhn = (1.0_r8 - stable)*0.0327_r8 + stable * 0.018_r8
!
!--- shift all coeffs to measurement height and stability ---
!
          rd = rdn / (1.0_r8 + rdn/shr_const_karman*(alz-psimh))
          rh = rhn / (1.0_r8 + rhn/shr_const_karman*(alz-psixh))
          re = ren / (1.0_r8 + ren/shr_const_karman*(alz-psixh))
!
!--- update ustar, tstar, qstar using updated, shifted coeffs ---
!
          ustar = rd * vmag
          tstar = rh * delt
          qstar = re * delq
!
!--- RD: save u10n to u10n_tmp of dim IminS:ImaxS,JminS:JmaxS
! otherwise there is an array overflow !!!
!
          u10n_tmp(i,j) = u10n
!
!------------------------------------------------------------
! compute the fluxes
!------------------------------------------------------------
!
          tau = rhoAir(i) * ustar * ustar
!
!--- momentum flux ---
!
          Taux(i,j) = tau * Uair(i,j) / vmag
          Tauy(i,j) = tau * Vair(i,j) / vmag

# ifdef ICE_MODEL
#  ifdef ICE_THERMO
          qswi_n(i,j) = srflx(i,j) * Hscale
#  endif
!
!  Ice model bulk flux formula
!
!  Sea ice momentum flux.
!
          Taux_Ice(i,j) = rhoAir(i)*cdai(ng)*Uwind(i,j)*vmag
          Tauy_Ice(i,j) = rhoAir(i)*cdai(ng)*Vwind(i,j)*vmag
#  ifdef ICE_THERMO
!
!  Correct ocean net short-wave radiation for ice cover and convert to
!  kinematic units
!
          Coef_Ice(i,j) = 0._r8
          RHS_Ice(i,j)  = 0._r8
!
!  Sensible heat flux over ice
!
          IF (ai(i,j,listp).gt.min_a(ng)) THEN
            sfc_temp    = tis(i,j) + 273.16_r8
          ELSE
            sfc_temp    = t(i,j,N(ng),nrhs,itemp) + 273.16_r8
          END IF
          qsh_i         = rhoAir(i)*blk_Cpa*Ch*vmag*                    &
     &                    (TairK(i)+0.0098*blk_ZT(ng)-sfc_temp)
          Coef_Ice(i,j) = Coef_Ice(i,j) +                               &
     &                    rhoAir(i)*blk_Cpa*Ch*vmag
          RHS_Ice(i,j)  = RHS_Ice(i,j) +                                &
     &                    rhoAir(i)*blk_Cpa*Ch*vmag*                    &
     &                    (TairK(i)+0.0098*blk_ZT(ng))
!
!  Latent heat of sublimation
!
          le_i = 2.834E+6_r8
!
!  Qsati is saturation specific humidity over the ice from
!  Parkinson and Washington (1979)
!
!  Convert back to Pascal from millibars
!
          slp    = Pair(i,j)*100._r8
          cff    = 611._r8*10._r8**(9.5_r8*(sfc_temp-273.16_r8) /       &
     &                              (sfc_temp-7.66_r8))
          Qsati  = epsilon*cff/(slp-(1._r8-epsilon)*cff)
          dq_i   = Qair-Qsati
          fqlat1 = rhoAir(i)*Ce*vmag
          qlh_i  = fqlat1*le_i*dq_i
!
#   ifdef ICE_DIAGS
          sen_i(i,j) = qsh_i
          lat_i(i,j) = qlh_i
#   endif
          RHS_Ice(i,j) = RHS_Ice(i,j) + qlh_i
!
!  Short-wave radiation to ice
!
#   ifdef ICE_ALBEDO
!
!  Total absorbed shortwave (W/m2 ice)
!
#    ifdef ICE_DIAGS
          sr_up_i(i,j) = alb_i(i,j)*qswi_n(i,j)
#    endif
          qswi = (1.0_r8-alb_i(i,j))*qswi_n(i,j)
#   else
          qswi = qswi_n(i,j)
#   endif

#   ifdef ICE_CCSM_SHORTWAVE
!
!  Note: ICE_ALB_CSIM option is activated if ICE_CCSM_SHORTWAVE is (see
!        globaldefs.h, albedo.F)
!
          i0frac(i,j) = frswp(i,j)
#   else
!
!  Assume only visual part penetrates (Briegleb et al., 2004)
!
#    ifdef ICE_ALBEDO
          i0frac(i,j) = (1.0_r8-alb_i(i,j))*fv
#    else
          i0frac(i,j) = fv
#    endif
#   endif

#   ifdef ICE_I_O
          IF (ai(i,j,listp) .gt. min_a(ng)) THEN
#    ifdef ICE_CCSM_SHORTWAVE
!
!  Modified PWA 14/03/2019 to account for ICE_CCSM_SHORTWAVE option
!
!  Penetrating SW = frswp*SW (W/m2 ice)
!
            sr_th_i(i,j) = qswi*i0frac(i,j)
#    else
!
!  Limit shortwave radiation to be used in computing penetrative
!  radiation by presence of ice and biology
!
            ice_thick  = hi(i,j,listp)  / (ai(i,j,listp)+0.001_r8)
            snow_thick = hsn(i,j,listp) / (ai(i,j,listp)+0.001_r8)
            IF (ice_thick.le.0.1_r8) THEN
              cff1 = ice_thick*5._r8
            ELSE
              cff1 = 0.1_r8*5._r8+(ice_thick-0.1_r8)*1._r8
            END IF
            cff1=cff1+snow_thick*20._r8
            sr_th_i(i,j) = qswi*exp(-cff1)
!
!  A even older formula to compute shortwave going through ice. Not
!  Recommended.
!
!           IF (hsn(i,j,listp) .lt. 1.e-3_r8) THEN
!             sr_th_i(i,j) = qswi * Io_frac
!           ELSE
!             sr_th_i(i,j) = 0._r8
!           END IF
#    endif
!
!  Update sr_in_i and qswi
!
            sr_in_i(i,j) = qswi - sr_th_i(i,j)
            qswi = sr_in_i(i,j)
          END IF
#   endif
!
!  Already in W/m**2
!
          RHS_Ice(i,j) = RHS_Ice(i,j) + qswi
!
!  Net long-wave radiation from ice
!
          cff     = Qair/(1.0_r8+Qair)
          vap_p_i = slp*cff/(0.62197 + cff)
#   ifdef LONGWAVE
          qlwi = emmiss*StefBo*(TairK(i)**4)*                           &
     &           (0.39_r8-0.005_r8*SQRT(vap_p_i))*                      &
     &           (1.0_r8-0.65_r8*cloud(i,j)) -                          &
     &           4._r8*StefBo*emmiss*TairK(i)**3*(sfc_temp-TairK(i))
!
          Coef_Ice(i,j) = Coef_Ice(i,j) +                               &
     &                    4._r8*StefBo*emmiss*TairK(i)**3
          RHS_Ice(i,j)  = RHS_Ice(i,j) -                                &
     &                    emmiss*StefBo*(TairK(i)**4)*                  &
     &                    ((0.39_r8-0.005_r8*SQRT(vap_p_i))*            &
     &                     (1.0_r8-0.65_r8*cloud(i,j)) - 4.0_r8)
#   elif defined LONGWAVE_OUT
          qlwi          = lrflx(i,j)*Hscale - emmiss*StefBo*sfc_temp**4
          Coef_Ice(i,j) = Coef_Ice(i,j) +                               &
     &                    4._r8*StefBo*emmiss*sfc_temp**3
          RHS_Ice(i,j)  = RHS_Ice(i,j) + lrflx(i,j)*Hscale +            &
     &                    3._r8*StefBo*emmiss*sfc_temp**4
#   else
          qlwi = 0.0_r8
#   endif
#   ifdef ICE_DIAGS
          lr_up_i(i,j) = emmiss*StefBo*sfc_temp**4
#   endif
!
!  Correct for Kelvin to Celsius in RHS
!
          RHS_Ice(i,j) = RHS_Ice(i,j) - Coef_Ice(i,j)*273.16_r8
!
!  Net heat flux from ice to atmosphere
!
          Qai(i,j) = - qsh_i - qlh_i - qswi - qlwi
#  endif
!
!--- heat flux ---
!
          SHeat(i,j) = cpair * tau * tstar / ustar
          LHeat(i,j) = shr_const_latvap * tau * qstar / ustar
          SHeat(i,j) = SHeat(i,j)*(1-ai(i,j,listp)) +                   &
     &                 qsh_i     *   ai(i,j,listp)
          LHeat(i,j) = LHeat(i,j)*(1-ai(i,j,listp)) +                   &
     &                 qlh_i     *   ai(i,j,listp)
# else
          SHeat(i,j) = cpair * tau * tstar / ustar
          LHeat(i,j) = shr_const_latvap * tau * qstar / ustar
# endif
        END DO
      END DO

      cff=1.0_r8/rhow
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          shflx (i,j) = SHeat(i,j)
          lhflx (i,j) = LHeat(i,j)
          tsk         = t(i,j,N(ng),nrhs,itemp) + 273.16_r8
          lwup        = -shr_const_stebol * tsk**4

# ifdef ICE_MODEL
#  ifdef ICE_DIAGS
          lr_dn(i,j) = lrflx(i,j)*Hscale
          sr_dn(i,j) = srflx(i,j)*Hscale
#  endif
!
!  Modified PWA 25/09/2019 to allow correction of srflx for ice/snow
!  albedo. Note that qswi is corrected separately for ice albedo above
!
          IF (ai(i,j,listp) .gt. min_a(ng)) THEN
            ice_thick  = hi(i,j,listp)  / (ai(i,j,listp)+0.001_r8)
            snow_thick = hsn(i,j,listp) / (ai(i,j,listp)+0.001_r8)
!
!  Note: This is necessary because hi,hsn are average thickness over the
!        full cell
!
#  ifdef ICE_CCSM_SHORTWAVE
!
!  Note: The attenuation due to snow is here accounted for in
!        i0frac=frswp, which uses asnow (see albedo.F)
!
            cff1 = kappav*ice_thick
#  else
            IF (ice_thick.le.0.1_r8) THEN
              cff1 = ice_thick*5._r8
            ELSE
              cff1 = 0.1_r8*5._r8+(ice_thick-0.1_r8)*1._r8
            END IF
            cff1 = cff1+snow_thick*20._r8
#  endif
#  ifdef ICE_ALBEDO
#   ifdef ICE_ALB_DIRDIFF
            cawdif  = 1._r8-albw_d
            transw  = cawdir(i,j)*(1.0_r8-cloud(i,j))+cawdif*cloud(i,j)
            transi  = i0frac(i,j)*EXP(-cff1)
            rtransi = transi/transw
            srflx(i,j) = srflx(i,j)*((1.0_r8-ai(i,j,listp))*transw +    &
     &                               ai(i,j,listp)         *transi)
#   else
            transw  = 1.0_r8-alb(i,j)
            transi  = i0frac(i,j)*EXP(-cff1)
            rtransi = transi/transw
            srflx(i,j) = srflx(i,j)*((1.0_r8-ai(i,j,listp))*transw +    &
     &                   ai(i,j,listp)*transi)
#   endif
#  else
            rtransi = i0frac(i,j)*EXP(-cff1)
            srflx(i,j) = srflx(i,j)*((1.0_r8-ai(i,j,listp)) +           &
     &                               ai(i,j,listp)*rtransi)
#  endif
!
!  Correction factor for lmd_r1 (near IR fraction of total SW)
!
            rfaci(i,j) = (1.0_r8-ai(i,j,listp)) /                       &
     &        ((1.0_r8-ai(i,j,listp)) + ai(i,j,listp)*MAX(eps,rtransi))
          ELSE
#  ifdef ICE_ALBEDO
#   ifdef ICE_ALB_DIRDIFF
            cawdif = 1._r8-albw_d
            transw = cawdir(i,j)*(1.0_r8-cloud(i,j))+cawdif*cloud(i,j)
            srflx(i,j) = srflx(i,j)*transw
#   else
            transw = 1.0_r8-alb(i,j)
            srflx(i,j) = srflx(i,j)*transw
#   endif
#  endif
            rfaci(i,j) = 1.0_r8
          END IF
!
!  End modification PWA 25/09/2019
!
# endif

# if defined LONGWAVE
          tsk = t(i,j,N(ng),nrhs,itemp) + 273.16_r8
          tak = Tair(i,j) + 273.16_r8

          cff=(0.7859_r8+0.03477_r8*Tair(i,j))/                         &
     &        (1.0_r8+0.00412_r8*Tair(i,j))
          cff=10.0_r8**cff   ! saturation vapor pressure (hPa or mbar)
          cff=cff*Hair(i,j)  ! water vapor pressure (hPa or mbar)

          cff2=tak*tak*tak
          cff1=cff2*tak
          lrflx(i,j) = -shr_const_stebol*                               &
     &                 (cff1*(0.39_r8-0.05_r8*SQRT(cff))  *             &
     &                       (1.0_r8-0.6823_r8*cloud(i,j)*cloud(i,j))+  &
     &                  cff2*4.0_r8*(tsk-tak))
          lrflx(i,j) = lrflx(i,j)*Hscale_inv
# elif defined LONGWAVE_OUT
          lrflx(i,j) = lwup*Hscale_inv + lrflx(i,j)
# endif
          lhflx(i,j) = lhflx(i,j)*Hscale_inv
          shflx(i,j) = shflx(i,j)*Hscale_inv
          stflux(i,j,itemp) = (srflx(i,j)+lrflx(i,j)+                   &
     &                         lhflx(i,j)+shflx(i,j))
# ifdef EMINUSP
!
!  Compute latent heat of vaporization (J/kg) at sea surface, Hlv.
!
          Hlv = (2.501_r8-0.00237_r8*t(i,j,N(ng),nrhs,itemp))*1.0e+6_r8
          evap(i,j) = -LHeat(i,j)/Hlv
#  ifdef ICE_MODEL
          evap(i,j) = (1.0_r8-ai(i,j,listp))*evap(i,j)
#  endif
#  ifdef EVAP_SHALLOW
          cffe = 1._r8/(h(i,j)+Zt_avg1(i,j))
          cffe = MIN(cffe,0.2_r8)
          IF (evap(i,j).gt.0._r8) THEN
            evap(i,j) = (1.0_r8+coef_shallow*cffe)*evap(i,j)
          END IF
#  endif
# endif
# ifdef WET_DRY
#  ifdef EMINUSP
          evap(i,j)=evap(i,j)*rmask_wet(i,j)
#  endif
# endif
!
!--- water flux ---
!     evap(i,j) = lhflx(i,j)/shr_const_latvap
!
!------------------------------------------------------------
! compute diagnositcs: 2m ref T & Q, 10m wind speed squared
!------------------------------------------------------------
!     hol       = hol*ztref/blk_ZQ(ng)
!     xsq       = max( 1.0, sqrt(abs(1.0-16.0*hol)) )
!     xqq       = sqrt(xsq)
!     psix2     = -5.0*hol*stable + (1.0-stable)*psixhu(xqq)
!     fac       = (rh/shr_const_karman) * (alz + al2 - psixh + psix2 )
!     tref(i,j) = TairK - delt*fac
!     tref(i,j) = tref(i,j) - 0.01*ztref -- pot temp to temp correction
!     fac       = (re/shr_const_karman) * (alz + al2 - psixh + psix2 )
!     qref(i,j) = Qair(i,j) - delq*fac
!
!     duu10n(i,j) = u10n*u10n ! 10m wind speed squared
!
!     real(r8) :: al2     ! ln(zref/ztref)
!     al2 = LOG(zref/ztref)
!

# ifdef MASKING
          stflux(i,j,itemp)=stflux(i,j,itemp)*rmask(i,j)
# endif
# ifdef WET_DRY
          stflux(i,j,itemp)=stflux(i,j,itemp)*rmask_wet(i,j)
# endif

# ifdef EMINUSP
#  ifdef MASKING
          evap(i,j)=evap(i,j)*rmask(i,j)
#  endif
#  ifdef WET_DRY
          evap(i,j)=evap(i,j)*rmask_wet(i,j)
#  endif
#  ifdef ICE_SNOWFALL
          stflux(i,j,isalt)=cff*(evap(i,j)-(rain(i,j) + snow(i,j)))
#  else
          stflux(i,j,isalt)=cff*(evap(i,j)-rain(i,j))
#  endif
#  ifdef MASKING
          stflux(i,j,isalt)=stflux(i,j,isalt)*rmask(i,j)
#  endif
#  ifdef WET_DRY
          stflux(i,j,isalt)=stflux(i,j,isalt)*rmask_wet(i,j)
#  endif
#  ifdef ICE_DIAGS
          ssflx_l(i,j)=(1.0_r8-ai(i,j,listp))*stflux(i,j,isalt)
#  endif
# endif
# ifdef ICE_MODEL
          qao_n(i,j) = -stflux(i,j,itemp)*Hscale
          qai_n(i,j) = Qai(i,j)
          coef_ice_heat(i,j) = Coef_Ice(i,j)
          rhs_ice_heat(i,j)  = RHS_Ice(i,j)
!
!  Net precipitation - evaporation in m/s
!
          IF (Tair(i,j).ge.0.0) THEN
            snow_n(i,j) = 0.0_r8
          ELSE
#  ifdef ICE_SNOWFALL
            snow_n(i,j) = snow(i,j)/rhosnow_dry(ng)
#  else
            snow_n(i,j) = rain(i,j)/rhosnow_dry(ng)
#  endif
          END IF
# endif  /* ICE_MODEL */
        END DO
      END DO
!
!  Compute kinematic, surface wind stress (m2/s2).
!
      Hscale = 1._r8/rho0
      DO j=JstrR,JendR
        DO i=Istr,IendR
          sustr(i,j)=Taux(i,j)*Hscale
# ifdef WET_DRY
          sustr(i,j)=sustr(i,j)*umask_wet(i,j)
# elif defined MASKING
          sustr(i,j)=sustr(i,j)*umask(i,j)
# endif
# ifdef ICE_MODEL
          sustr_aw(i,j)=sustr(i,j)
          tau_aix_n(i,j)=Taux_Ice(i,j)
# endif
        END DO
      END DO
      DO j=Jstr,JendR
        DO i=IstrR,IendR
          svstr(i,j)=Tauy(i,j)*Hscale
# ifdef WET_DRY
          svstr(i,j)=svstr(i,j)*vmask_wet(i,j)
# elif defined MASKING
          svstr(i,j)=svstr(i,j)*vmask(i,j)
# endif
# ifdef ICE_MODEL
          svstr_aw(i,j)=svstr(i,j)
          tau_aiy_n(i,j)=Tauy_Ice(i,j)
# endif
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Exchange boundary data.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          lrflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          lhflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          shflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          stflux(:,:,itemp))
# ifdef EMINUSP
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          evap)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          stflux(:,:,isalt))
# endif
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sustr)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          svstr)
# ifdef ICE_MODEL
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          srflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          qao_n)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          tau_aix_n)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          tau_aiy_n)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          rfaci)
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sustr_aw)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          svstr_aw)
#  ifdef ICE_THERMO
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          qai_n)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sr_in_i)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          coef_ice_heat)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          rhs_ice_heat)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          snow_n)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sr_th_i)
#  endif
#  ifdef ICE_DIAGS
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          lr_dn)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sr_dn)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          lat_i)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sen_i)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          lr_up_i)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sr_up_i)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          ssflx_l)
#  endif
# endif
      END IF

# ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    lrflx, lhflx, shflx,                          &
     &                    stflux(:,:,itemp))
#  ifdef EMINUSP
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    evap,                                         &
     &                    stflux(:,:,isalt))
#  endif
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    sustr, svstr)
#  ifdef ICE_MODEL
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    srflx, qao_n, tau_aix_n, tau_aiy_n )
      CALL mp_exchange2d (ng, tile, iNLM, 3,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    rfaci, sustr_aw, svstr_aw)
#   ifdef ICE_THERMO
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    qai_n, coef_ice_heat, rhs_ice_heat, snow_n )
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    sr_in_i, sr_th_i)
#   endif
#   ifdef ICE_DIAGS
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    lr_dn, sr_dn, lat_i, sen_i)
      CALL mp_exchange2d (ng, tile, iNLM, 3,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    lr_up_i, sr_up_i, ssflx_l)
#   endif
#  endif
# endif

      RETURN
      END SUBROUTINE ccsm_flux_tile
#endif

      END module ccsm_flux_mod
