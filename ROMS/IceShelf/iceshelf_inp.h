      SUBROUTINE read_IcePar (model, inp, out, Lwrite)
!
!git $Id$
!=======================================================================
!                                                                      !
!  This routine reads in iceshelf model parameters.                    !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_ncparam
      USE mod_scalars
      USE mod_iceshelfvar
!
      USE inp_decode_mod
!
      implicit none
!
!  Imported variable declarations
!
      logical, intent(in) :: Lwrite
      integer, intent(in) :: model, inp, out
!
!  Local variable declarations.
!
      integer :: Npts, Nval
      integer :: iTrcStr, iTrcEnd
      integer :: i, ifield, igrid, itracer, itrc, ng, nline, status

      logical, dimension(Ngrids) :: Lbed
      logical, dimension(Ngrids) :: Lbottom
      logical, dimension(NCS,Ngrids) :: Lmud
      logical, dimension(NNS,Ngrids) :: Lsand

      real(r8), dimension(Ngrids) :: Rbed
      real(r8), dimension(NCS,Ngrids) :: Rmud
      real(r8), dimension(NNS,Ngrids) :: Rsand

      real(dp), dimension(nRval) :: Rval

      character (len=40 ) :: KeyWord
      character (len=256) :: line
      character (len=256), dimension(nCval) :: Cval
!
!-----------------------------------------------------------------------
!  Initialize.
!-----------------------------------------------------------------------
!
      igrid=1                            ! nested grid counter
      itracer=0                          ! LBC tracer counter
      iTrcStr=1                          ! first LBC tracer to process
      iTrcEnd=NST                        ! last  LBC tracer to process
      nline=0                            ! LBC multi-line counter
!
!-----------------------------------------------------------------------
!  Read in cohesive and non-cohesive model parameters.
!-----------------------------------------------------------------------
!
      DO WHILE (.TRUE.)
        READ (inp,'(a)',ERR=10,END=20) line
        status=decode_line(line, KeyWord, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          SELECT CASE (TRIM(KeyWord))
#ifdef ICESHELF
            CASE ('Hout(idismr)')
              IF (idismr.eq.0) THEN
                IF (Master) WRITE (out,280) 'idismr'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idismr,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idisTstar)')
              IF (idisTstar.eq.0) THEN
                IF (Master) WRITE (out,280) 'idisTstar'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idisTstar,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idisUstar)')
              IF (idisUstar.eq.0) THEN
                IF (Master) WRITE (out,280) 'idisUstar'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idisUstar,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idisTb)')
              IF (idisTb.eq.0) THEN
                IF (Master) WRITE (out,280) 'idisTb'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idisTb,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idisSb)')
              IF (idisSb.eq.0) THEN
                IF (Master) WRITE (out,280) 'idisSb'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idisSb,1:Ngrids)=Lswitch(1:Ngrids)
# if defined ICESHELF_MORPH
            CASE ('Hout(idDraft)')
              IF (iddraft.eq.0) THEN
                IF (Master) WRITE (out,280) 'iddraft'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(iddraft,1:Ngrids)=Lswitch(1:Ngrids)
# endif
#endif
#if defined SURFACE_OVERFLUX_FIX
            CASE ('Hout(idssflxSOF)')
              IF (idssflxSOF.eq.0) THEN
                IF (Master) WRITE (out,280) 'idssflxSOF'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idssflxSOF,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idshflxSOF)')
              IF (idshflxSOF.eq.0) THEN
                IF (Master) WRITE (out,280) 'idshflxSOF'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idshflxSOF,1:Ngrids)=Lswitch(1:Ngrids)
#endif
          END SELECT
        END IF
      END DO
  10  IF (Master) WRITE (out,40) line
      exit_flag=4
      RETURN
  20  CONTINUE
!
!-----------------------------------------------------------------------
!  Report input parameters.
!-----------------------------------------------------------------------
!
      IF (Master.and.Lwrite) THEN
        DO ng=1,Ngrids
          IF (Lsediment(ng)) THEN
            WRITE (out,50) ng
            WRITE (out,60)
            DO itrc=1,NST
              WRITE (out,70) itrc, Sd50(itrc,ng), Csed(itrc,ng),        &
     &                       Srho(itrc,ng), Wsed(itrc,ng),              &
     &                       Erate(itrc,ng), poros(itrc,ng)
            END DO
            WRITE (out,80)
            DO itrc=1,NST
              i=idsed(itrc)
              WRITE (out,70) itrc, tau_ce(itrc,ng), tau_cd(itrc,ng),    &
     &                       nl_tnu2(i,ng), nl_tnu4(i,ng),              &
     &                       Akt_bak(i,ng), Tnudg(i,ng)
            END DO
            WRITE (out,90)
            DO itrc=1,NST
              WRITE (out,70) itrc,  morph_fac(itrc,ng)
            END DO
            WRITE (out,100) newlayer_thick(ng)
            WRITE (out,110) minlayer_thick(ng)
            WRITE (out,120) bedload_coeff(ng)
#ifdef MIXED_BED
            WRITE (out,130) transC(ng)
            WRITE (out,140) transN(ng)
#endif
            DO itrc=1,NST
              i=idsed(itrc)
              IF (LtracerSponge(i,ng)) THEN
                WRITE (out,150) LtracerSponge(i,ng), 'LtracerSponge',   &
     &              i, 'Turning ON  sponge on tracer ', i,              &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,150) LtracerSponge(i,ng), 'LtracerSponge',   &
     &              i, 'Turning OFF sponge on tracer ', i,              &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NST
              i=idsed(itrc)
              IF (LtracerSrc(i,ng)) THEN
                WRITE (out,150) LtracerSrc(i,ng), 'LtracerSrc', i,      &
     &              'Turning ON  point sources/Sink on tracer ', i,     &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,150) LtracerSrc(i,ng), 'LtracerSrc', i,      &
     &              'Turning OFF point sources/Sink on tracer ', i,     &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NST
              i=idsed(itrc)
              IF (LtracerCLM(i,ng)) THEN
                WRITE (out,150) LtracerCLM(i,ng), 'LtracerCLM', i,      &
     &              'Turning ON  processing of climatology tracer ', i, &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,150) LtracerCLM(i,ng), 'LtracerCLM', i,      &
     &              'Turning OFF processing of climatology tracer ', i, &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NST
              i=idsed(itrc)
              IF (LnudgeTCLM(i,ng)) THEN
                WRITE (out,150) LnudgeTCLM(i,ng), 'LnudgeTCLM', i,      &
     &              'Turning ON  nudging of climatology tracer ', i,    &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,150) LnudgeTCLM(i,ng), 'LnudgeTCLM', i,      &
     &              'Turning OFF nudging of climatology tracer ', i,    &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            IF ((nHIS(ng).gt.0).and.ANY(Hout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NST
                i=idTvar(idsed(itrc))
                IF (Hout(i,ng)) WRITE (out,160) Hout(i,ng),             &
     &              'Hout(idTvar)',                                     &
     &              'Write out sediment', itrc, TRIM(Vname(1,i))
              END DO
              DO itrc=1,NST
                i=idfrac(itrc)
                IF (Hout(i,ng)) WRITE (out,160) Hout(i,ng),             &
     &              'Hout(idfrac)',                                     &
     &              'Write out bed fraction, sediment ', itrc,          &
     &              TRIM(Vname(1,i))
              END DO
              DO itrc=1,NST
                i=idBmas(itrc)
                IF (Hout(i,ng)) WRITE (out,160) Hout(i,ng),             &
     &              'Hout(idfrac)',                                     &
     &              'Write out mass, sediment ', itrc,                  &
     &              TRIM(Vname(1,i))
              END DO
#ifdef BEDLOAD
              DO itrc=1,NST
                i=idUbld(itrc)
                IF (Hout(i,ng)) WRITE (out,160) Hout(i,ng),             &
     &              'Hout(idUbld)',                                     &
     &              'Write out bed load at U-points, sediment ', itrc,  &
     &              TRIM(Vname(1,i))
                i=idVbld(itrc)
                IF (Hout(i,ng)) WRITE (out,160) Hout(i,ng),             &
     &              'Hout(idVbld)',                                     &
     &              'Write out bed load at V-points, sediment ', itrc,  &
     &              TRIM(Vname(1,i))
              END DO
#endif
              DO itrc=1,MBEDP
                i=idSbed(itrc)
                IF (Hout(i,ng)) WRITE (out,160) Hout(i,ng),             &
     &              'Hout(idSbed)',                                     &
     &              'Write out BED property ', itrc, TRIM(Vname(1,i))
              END DO
              DO itrc=1,MBOTP
                i=idBott(itrc)
                IF (Hout(i,ng)) WRITE (out,160) Hout(i,ng),             &
     &             'Hout(idBott)',                                      &
     &             'Write out BOTTOM property ', itrc, TRIM(Vname(1,i))
              END DO
            END IF
            IF ((nQCK(ng).gt.0).and.ANY(Qout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NST
                i=idTvar(idsed(itrc))
                IF (Qout(i,ng)) WRITE (out,160) Qout(i,ng),             &
     &              'Qout(idTvar)',                                     &
     &              'Write out sediment', itrc, TRIM(Vname(1,i))
              END DO
              DO itrc=1,NST
                i=idsurT(idsed(itrc))
                IF (Qout(i,ng)) WRITE (out,160) Qout(i,ng),             &
     &              'Qout(idTvar)',                                     &
     &              'Write out surface sediment', itrc, TRIM(Vname(1,i))
              END DO
              DO itrc=1,NST
                i=idfrac(itrc)
                IF (Qout(i,ng)) WRITE (out,160) Qout(i,ng),             &
     &              'Qout(idfrac)',                                     &
     &              'Write out bed fraction, sediment ', itrc,          &
     &              TRIM(Vname(1,i))
              END DO
              DO itrc=1,NST
                i=idBmas(itrc)
                IF (Qout(i,ng)) WRITE (out,160) Qout(i,ng),             &
     &              'Qout(idfrac)',                                     &
     &              'Write out mass, sediment ', itrc,                  &
     &              TRIM(Vname(1,i))
              END DO
#ifdef BEDLOAD
              DO itrc=1,NST
                i=idUbld(itrc)
                IF (Qout(i,ng)) WRITE (out,160) Qout(i,ng),             &
     &              'Qout(idUbld)',                                     &
     &              'Write out bed load at U-points, sediment ', itrc,  &
     &              TRIM(Vname(1,i))
                i=idVbld(itrc)
                IF (Qout(i,ng)) WRITE (out,160) Qout(i,ng),             &
     &              'Qout(idVbld)',                                     &
     &              'Write out bed load at V-points, sediment ', itrc,  &
     &              TRIM(Vname(1,i))
              END DO
#endif
              DO itrc=1,MBEDP
                i=idSbed(itrc)
                IF (Qout(i,ng)) WRITE (out,160) Qout(i,ng),             &
     &              'Qout(idSbed)',                                     &
     &              'Write out BED property ', itrc, TRIM(Vname(1,i))
              END DO
              DO itrc=1,MBOTP
                i=idBott(itrc)
                IF (Qout(i,ng)) WRITE (out,160) Qout(i,ng),             &
     &             'Qout(idBott)',                                      &
     &             'Write out BOTTOM property ', itrc, TRIM(Vname(1,i))
              END DO
            END IF
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            IF ((nAVG(ng).gt.0).and.ANY(Aout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NST
                i=idTvar(idsed(itrc))
                IF (Aout(i,ng)) WRITE (out,160) Aout(i,ng),             &
     &              'Aout(idTvar)',                                     &
     &              'Write out averaged sediment', itrc,                &
     &              TRIM(Vname(1,i))
              END DO
              DO itrc=1,NST
                i=idsed(itrc)
                IF (Aout(idTTav(i),ng)) WRITE (out,160)                 &
     &              Aout(idTTav(i),ng), 'Aout(idTTav)',                 &
     &              'Write out averaged <t*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NST
                i=idsed(itrc)
                IF (Aout(idUTav(i),ng)) WRITE (out,160)                 &
     &              Aout(idUTav(i),ng), 'Aout(idUTav)',                 &
     &              'Write out averaged <u*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NST
                i=idsed(itrc)
                IF (Aout(idVTav(i),ng)) WRITE (out,160)                 &
     &              Aout(idVTav(i),ng), 'Aout(idVTav)',                 &
     &              'Write out averaged <v*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NST
                i=idsed(itrc)
                IF (Aout(iHUTav(i),ng)) WRITE (out,160)                 &
     &              Aout(iHUTav(i),ng), 'Aout(iHUTav)',                 &
     &              'Write out averaged <Huon*t> for tracer ', i,       &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NST
                i=idsed(itrc)
                IF (Aout(iHVTav(i),ng)) WRITE (out,160)                 &
     &              Aout(iHVTav(i),ng), 'Aout(iHVTav)',                 &
     &              'Write out averaged <Hvom*t> for tracer ', i,       &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
# ifdef BEDLOAD
              DO itrc=1,NST
                i=idUbld(itrc)
                IF (Aout(i,ng)) WRITE (out,160) Aout(i,ng),             &
     &              'Aout(idUbld)',                                     &
     &              'Write out U-bedload, sediment ', itrc,             &
     &              TRIM(Vname(1,i))
                i=idVbld(itrc)
                IF (Aout(i,ng)) WRITE (out,160) Aout(i,ng),             &
     &              'Aout(idVbld)',                                     &
     &              'Write out V-bedload, sediment ', itrc,             &
     &              TRIM(Vname(1,i))
              END DO
# endif
            END IF
#endif
#ifdef DIAGNOSTICS_TS
            IF ((nDIA(ng).gt.0).and.ANY(Dout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO i=1,NST
                itrc=idsed(i)
                IF (Dout(idDtrc(itrc,iTrate),ng))                       &
     &            WRITE (out,160) .TRUE., 'Dout(iTrate)',               &
     &              'Write out rate of change of tracer ', itrc,        &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NST
                itrc=idsed(i)
                IF (Dout(idDtrc(itrc,iThadv),ng))                       &
     &            WRITE (out,160) .TRUE., 'Dout(iThadv)',               &
     &              'Write out horizontal advection, tracer ', itrc,    &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NST
                itrc=idsed(i)
                IF (Dout(idDtrc(itrc,iTxadv),ng))                       &
     &            WRITE (out,160) .TRUE., 'Dout(iTxadv)',               &
     &              'Write out horizontal X-advection, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NST
                itrc=idsed(i)
                IF (Dout(idDtrc(itrc,iTyadv),ng))                       &
     &            WRITE (out,160) .TRUE., 'Dout(iTyadv)',               &
     &              'Write out horizontal Y-advection, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NST
                itrc=idsed(i)
                IF (Dout(idDtrc(itrc,iTvadv),ng))                       &
     &            WRITE (out,160) .TRUE., 'Dout(iTvadv)',               &
     &              'Write out vertical advection, tracer ', itrc,      &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
# if defined TS_DIF2 || defined TS_DIF4
              DO i=1,NST
                itrc=idsed(i)
                IF (Dout(idDtrc(itrc,iThdif),ng))                       &
     &            WRITE (out,160) .TRUE., 'Dout(iThdif)',               &
     &              'Write out horizontal diffusion, tracer ', itrc,    &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NST
                itrc=idsed(i)
                IF (Dout(idDtrc(i,iTxdif),ng))                          &
     &            WRITE (out,160) .TRUE., 'Dout(iTxdif)',               &
     &              'Write out horizontal X-diffusion, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NST
                itrc=idsed(i)
                IF (Dout(idDtrc(itrc,iTydif),ng))                       &
     &            WRITE (out,160) .TRUE., 'Dout(iTydif)',               &
     &              'Write out horizontal Y-diffusion, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
              DO i=1,NST
                itrc=idsed(i)
                IF (Dout(idDtrc(itrc,iTsdif),ng))                       &
     &            WRITE (out,160) .TRUE., 'Dout(iTsdif)',               &
     &              'Write out horizontal S-diffusion, tracer ', itrc,  &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
#  endif
# endif
              DO i=1,NST
                itrc=idsed(i)
                IF (Dout(idDtrc(itrc,iTvdif),ng))                       &
     &            WRITE (out,160) .TRUE., 'Dout(iTvdif)',               &
     &              'Write out vertical diffusion, tracer ', itrc,      &
     &              TRIM(Vname(1,idTvar(itrc)))
              END DO
            END IF
#endif
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Scale relevant input parameters
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO i=1,NST
          Sd50(i,ng)=Sd50(i,ng)*0.001_r8
          Wsed(i,ng)=Wsed(i,ng)*0.001_r8
          tau_ce(i,ng)=tau_ce(i,ng)/rho0
          tau_cd(i,ng)=tau_cd(i,ng)/rho0
          nl_tnu4(idsed(i),ng)=SQRT(ABS(nl_tnu4(idsed(i),ng)))
#ifdef ADJOINT
          ad_tnu4(idsed(i),ng)=SQRT(ABS(ad_tnu4(idsed(i),ng)))
#endif
#if defined TANGENT || defined TL_IOMS
          tl_tnu4(idsed(i),ng)=SQRT(ABS(tl_tnu4(idsed(i),ng)))
#endif
          IF (Tnudg(idsed(i),ng).gt.0.0_r8) THEN
            Tnudg(idsed(i),ng)=1.0_r8/(Tnudg(idsed(i),ng)*86400.0_r8)
          ELSE
            Tnudg(idsed(i),ng)=0.0_r8
          END IF
        END DO
      END DO

  30  FORMAT (/,' READ_SedPar - variable info not yet loaded, ', a)
  40  FORMAT (/,' READ_SedPar - Error while processing line: ',/,a)
  50  FORMAT (/,/,' Sediment Parameters, Grid: ',i2.2,                  &
     &        /,  ' =============================',/)
  60  FORMAT (/,1x,'Size',5x,'Sd50',8x,'Csed',8x,'Srho',8x,'Wsed',      &
     &        8x,'Erate',7x,'poros',/,1x,'Class',4x,'(mm)',7x,          &
     &        '(kg/m3)',5x,'(kg/m3)',5x,'(mm/s)',5x,'(kg/m2/s)',4x,     &
     &        '(nondim)',/)
  70  FORMAT (2x,i2,2x,6(1x,1p,e11.4))
  80  FORMAT (/,9x,'tau_ce',6x,'tau_cd',6x,'nl_tnu2',5x,'nl_tnu4',5x,   &
     &        'Akt_bak',6x,'Tnudg',/,9x,'(N/m2)',6x,'(N/m2)',6x,        &
     &        '(m2/s)',6x,'(m4/s)',7x,'(m2/s)',6x,'(day)',/)
  90  FORMAT (/,9x,'morph_fac',/,9x,'(nondim)',/)
 100  FORMAT (/,' New bed layer formed when deposition exceeds ',e12.5, &
     &        ' (m).')
 110  FORMAT (' Two first layers are combined when 2nd layer smaller ', &
     &         'than ',e12.5,' (m).')
 120  FORMAT (' Rate coefficient for bed load transport = ',e12.5,/)
 130  FORMAT (' Transition for mixed sediment =',e12.5,/)
 140  FORMAT (' Transition for cohesive sediment =',e12.5,/)
 150  FORMAT (10x,l1,2x,a,'(',i2.2,')',t30,a,i2.2,':',1x,a)
 160  FORMAT (10x,l1,2x,a,t29,a,i2.2,':',1x,a)

      RETURN
      END SUBROUTINE read_IcePar

