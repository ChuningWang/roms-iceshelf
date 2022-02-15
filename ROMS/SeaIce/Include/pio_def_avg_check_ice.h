#  if defined ICE_MODEL || defined ICESHELF
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
        DO i=1,n_var
          IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
            got_var(idtime)=.TRUE.
#   ifdef ICE_MODEL
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIuice))) THEN
            got_var(idIuice)=.TRUE.
            AVG(ng)%pioVar(idIuice)%vd=var_desc(i)
            AVG(ng)%pioVar(idIuice)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIuice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIvice))) THEN
            got_var(idIvice)=.TRUE.
            AVG(ng)%pioVar(idIvice)%vd=var_desc(i)
            AVG(ng)%pioVar(idIvice)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIvice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIuiceE))) THEN
            got_var(idIuiceE)=.TRUE.
            AVG(ng)%pioVar(idIuiceE)%vd=var_desc(i)
            AVG(ng)%pioVar(idIuiceE)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIuiceE)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIviceN))) THEN
            got_var(idIviceN)=.TRUE.
            AVG(ng)%pioVar(idIviceN)%vd=var_desc(i)
            AVG(ng)%pioVar(idIviceN)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIviceN)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIaice))) THEN
            got_var(idIaice)=.TRUE.
            AVG(ng)%pioVar(idIaice)%vd=var_desc(i)
            AVG(ng)%pioVar(idIaice)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIaice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhice))) THEN
            got_var(idIhice)=.TRUE.
            AVG(ng)%pioVar(idIhice)%vd=var_desc(i)
            AVG(ng)%pioVar(idIhice)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIhice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhsno))) THEN
            got_var(idIhsno)=.TRUE.
            AVG(ng)%pioVar(idIhsno)%vd=var_desc(i)
            AVG(ng)%pioVar(idIhsno)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIhsno)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIagei))) THEN
            got_var(idIagei)=.TRUE.
            AVG(ng)%pioVar(idIagei)%vd=var_desc(i)
            AVG(ng)%pioVar(idIagei)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIagei)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItice))) THEN
            got_var(idItice)=.TRUE.
            AVG(ng)%pioVar(idItice)%vd=var_desc(i)
            AVG(ng)%pioVar(idItice)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idItice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItmid))) THEN
            got_var(idItmid)=.TRUE.
            AVG(ng)%pioVar(idItmid)%vd=var_desc(i)
            AVG(ng)%pioVar(idItmid)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idItmid)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt2ic))) THEN
            got_var(idIt2ic)=.TRUE.
            AVG(ng)%pioVar(idIt2ic)%vd=var_desc(i)
            AVG(ng)%pioVar(idIt2ic)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIt2ic)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg11))) THEN
            got_var(idIsg11)=.TRUE.
            AVG(ng)%pioVar(idIsg11)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsg11)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsg11)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg12))) THEN
            got_var(idIsg12)=.TRUE.
            AVG(ng)%pioVar(idIsg12)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsg12)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsg12)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg22))) THEN
            got_var(idIsg22)=.TRUE.
            AVG(ng)%pioVar(idIsg22)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsg22)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsg22)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIutau))) THEN
            got_var(idIutau)=.TRUE.
            AVG(ng)%pioVar(idIutau)%vd=var_desc(i)
            AVG(ng)%pioVar(idIutau)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIutau)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImchu))) THEN
            got_var(idImchu)=.TRUE.
            AVG(ng)%pioVar(idImchu)%vd=var_desc(i)
            AVG(ng)%pioVar(idImchu)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idImchu)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt0mk))) THEN
            got_var(idIt0mk)=.TRUE.
            AVG(ng)%pioVar(idIt0mk)%vd=var_desc(i)
            AVG(ng)%pioVar(idIt0mk)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIt0mk)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIs0mk))) THEN
            got_var(idIs0mk)=.TRUE.
            AVG(ng)%pioVar(idIs0mk)%vd=var_desc(i)
            AVG(ng)%pioVar(idIs0mk)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIs0mk)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImflx))) THEN
            got_var(idImflx)=.TRUE.
            AVG(ng)%pioVar(idImflx)%vd=var_desc(i)
            AVG(ng)%pioVar(idImflx)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idImflx)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwfr))) THEN
            got_var(idIwfr)=.TRUE.
            AVG(ng)%pioVar(idIwfr)%vd=var_desc(i)
            AVG(ng)%pioVar(idIwfr)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIwfr)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwai))) THEN
            got_var(idIwai)=.TRUE.
            AVG(ng)%pioVar(idIwai)%vd=var_desc(i)
            AVG(ng)%pioVar(idIwai)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIwai)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwao))) THEN
            got_var(idIwao)=.TRUE.
            AVG(ng)%pioVar(idIwao)%vd=var_desc(i)
            AVG(ng)%pioVar(idIwao)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIwao)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwio))) THEN
            got_var(idIwio)=.TRUE.
            AVG(ng)%pioVar(idIwio)%vd=var_desc(i)
            AVG(ng)%pioVar(idIwio)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIwio)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwro))) THEN
            got_var(idIwro)=.TRUE.
            AVG(ng)%pioVar(idIwro)%vd=var_desc(i)
            AVG(ng)%pioVar(idIwro)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIwro)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwdiv))) THEN
            got_var(idIwdiv)=.TRUE.
            AVG(ng)%pioVar(idIwdiv)%vd=var_desc(i)
            AVG(ng)%pioVar(idIwdiv)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIwdiv)%gtype=r2dvar
#    ifdef ICE_DIAGS
#     ifdef ICE_ALBEDO
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIalbe))) THEN
            got_var(idIalbe)=.TRUE.
            AVG(ng)%pioVar(idIalbe)%vd=var_desc(i)
            AVG(ng)%pioVar(idIalbe)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIalbe)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIalbI))) THEN
            got_var(idIalbI)=.TRUE.
            AVG(ng)%pioVar(idIalbI)%vd=var_desc(i)
            AVG(ng)%pioVar(idIalbI)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIalbI)%gtype=r2dvar
#      ifdef ICE_ALB_CSIM
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIfrsw))) THEN
            got_var(idIfrsw)=.TRUE.
            AVG(ng)%pioVar(idIfrsw)%vd=var_desc(i)
            AVG(ng)%pioVar(idIfrsw)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIfrsw)%gtype=r2dvar
#      endif
#     endif
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIrfac))) THEN
            got_var(idIrfac)=.TRUE.
            AVG(ng)%pioVar(idIrfac)%vd=var_desc(i)
            AVG(ng)%pioVar(idIrfac)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIrfac)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIlrdn))) THEN
            got_var(idIlrdn)=.TRUE.
            AVG(ng)%pioVar(idIlrdn)%vd=var_desc(i)
            AVG(ng)%pioVar(idIlrdn)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIlrdn)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsrdn))) THEN
            got_var(idIsrdn)=.TRUE.
            AVG(ng)%pioVar(idIsrdn)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsrdn)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsrdn)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIlatI))) THEN
            got_var(idIlatI)=.TRUE.
            AVG(ng)%pioVar(idIlatI)%vd=var_desc(i)
            AVG(ng)%pioVar(idIlatI)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIlatI)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsenI))) THEN
            got_var(idIsenI)=.TRUE.
            AVG(ng)%pioVar(idIsenI)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsenI)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsenI)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIlrup))) THEN
            got_var(idIlrup)=.TRUE.
            AVG(ng)%pioVar(idIlrup)%vd=var_desc(i)
            AVG(ng)%pioVar(idIlrup)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIlrup)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsrup))) THEN
            got_var(idIsrup)=.TRUE.
            AVG(ng)%pioVar(idIsrup)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsrup)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsrup)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsrin))) THEN
            got_var(idIsrin)=.TRUE.
            AVG(ng)%pioVar(idIsrin)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsrin)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsrin)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsrth))) THEN
            got_var(idIsrth)=.TRUE.
            AVG(ng)%pioVar(idIsrth)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsrth)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsrth)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsfI))) THEN
            got_var(idIsfI)=.TRUE.
            AVG(ng)%pioVar(idIsfI)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsfI)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsfI)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsfL))) THEN
            got_var(idIsfL)=.TRUE.
            AVG(ng)%pioVar(idIsfL)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsfL)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsfL)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqao))) THEN
            got_var(idIqao)=.TRUE.
            AVG(ng)%pioVar(idIqao)%vd=var_desc(i)
            AVG(ng)%pioVar(idIqao)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIqao)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqai))) THEN
            got_var(idIqai)=.TRUE.
            AVG(ng)%pioVar(idIqai)%vd=var_desc(i)
            AVG(ng)%pioVar(idIqai)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIqai)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqio))) THEN
            got_var(idIqio)=.TRUE.
            AVG(ng)%pioVar(idIqio)%vd=var_desc(i)
            AVG(ng)%pioVar(idIqio)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIqio)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqi2))) THEN
            got_var(idIqi2)=.TRUE.
            AVG(ng)%pioVar(idIqi2)%vd=var_desc(i)
            AVG(ng)%pioVar(idIqi2)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIqi2)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsnoi))) THEN
            got_var(idIsnoi)=.TRUE.
            AVG(ng)%pioVar(idIsnoi)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsnoi)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsnoi)%gtype=r2dvar
#    endif
#   endif
#   ifdef ICESHELF
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsMr))) THEN
            got_var(idIsMr)=.TRUE.
            AVG(ng)%pioVar(idIsMr)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsMr)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsMr)%gtype=r2dvar
#    ifdef ICESHELF_3EQN_VBC
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTs))) THEN
            got_var(idIsTs)=.TRUE.
            AVG(ng)%pioVar(idIsTs)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsTs)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsTs)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsUs))) THEN
            got_var(idIsUs)=.TRUE.
            AVG(ng)%pioVar(idIsUs)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsUs)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsUs)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTb))) THEN
            got_var(idIsTb)=.TRUE.
            AVG(ng)%pioVar(idIsTb)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsTb)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsTb)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSb))) THEN
            got_var(idIsSb)=.TRUE.
            AVG(ng)%pioVar(idIsSb)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsSb)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsSb)%gtype=r2dvar
#    endif
#    ifdef ICESHELF_MORPH
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsDrft))) THEN
            got_var(idIsDrft)=.TRUE.
            AVG(ng)%pioVar(idIsDrft)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsDrft)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsDrft)%gtype=r2dvar
#    endif
#    ifdef ICESHELF_SOF
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSOFs))) THEN
            got_var(idIsSOFs)=.TRUE.
            AVG(ng)%pioVar(idIsSOFs)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsSOFs)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsSOFs)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSOFh))) THEN
            got_var(idIsSOFh)=.TRUE.
            AVG(ng)%pioVar(idIsSOFh)%vd=var_desc(i)
            AVG(ng)%pioVar(idIsSOFh)%dkind=PIO_FOUT
            AVG(ng)%pioVar(idIsSOFh)%gtype=r2dvar
#    endif
#   endif
          END IF
        END DO
!
!  Check if variables are available in input NetCDF file.
!
#   ifdef ICE_MODEL
        IF (.not.got_var(idIuice).and.Aout(idIuice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIvice).and.Aout(idIvice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIvice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIuiceE).and.Aout(idIuiceE,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuiceE)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIviceN).and.Aout(idIviceN,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIviceN)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIaice).and.Aout(idIaice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIaice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhice).and.Aout(idIhice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhsno).and.Aout(idIhsno,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhsno)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIagei).and.Aout(idIagei,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIagei)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItice).and.Aout(idItice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItmid).and.Aout(idItmid,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItmid)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt2ic).and.Aout(idIt2ic,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt2ic)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg11).and.Aout(idIsg11,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg11)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg12).and.Aout(idIsg12,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg12)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg22).and.Aout(idIsg22,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg22)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIutau).and.Aout(idIutau,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIutau)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImchu).and.Aout(idImchu,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImchu)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt0mk).and.Aout(idIt0mk,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIs0mk).and.Aout(idIs0mk,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIs0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImflx).and.Aout(idImflx,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImflx)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwfr).and.Aout(idIwfr,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwfr)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwai).and.Aout(idIwai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwai)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwao).and.Aout(idIwao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwio).and.Aout(idIwio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwro).and.Aout(idIwro,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwro)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwdiv).and.Aout(idIwdiv,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwdiv)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    ifdef ICE_DIAGS
#     ifdef ICE_ALBEDO
        IF (.not.got_var(idIalbe).and.Aout(idIalbe,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIalbe)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIalbI).and.Aout(idIalbI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIalbI)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#      ifdef ICE_ALB_CSIM
        IF (.not.got_var(idIfrsw).and.Aout(idIfrsw,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIfrsw)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#      endif
#     endif
        IF (.not.got_var(idIrfac).and.Aout(idIrfac,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIrfac)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIlrdn).and.Aout(idIlrdn,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIlrdn)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsrdn).and.Aout(idIsrdn,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsrdn)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIlatI).and.Aout(idIlatI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIlatI)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsenI).and.Aout(idIsenI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsenI)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIlrup).and.Aout(idIlrup,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIlrup)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsrup).and.Aout(idIsrup,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsrup)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsrin).and.Aout(idIsrin,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsrin)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsrth).and.Aout(idIsrth,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsrth)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsfI).and.Aout(idIsfI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsfI)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsfL).and.Aout(idIsfL,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsfL)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqao).and.Aout(idIqao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqai).and.Aout(idIqai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqai)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqio).and.Aout(idIqio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqi2).and.Aout(idIqi2,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqi2)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsnoi).and.Aout(idIsnoi,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsnoi)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    endif
#   endif
#   ifdef ICESHELF
        IF (.not.got_var(idIsMr).and.Aout(idIsMr,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsMr)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    ifdef ICESHELF_3EQN_VBC
        IF (.not.got_var(idIsTs).and.Aout(idIsTs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTs)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsUs).and.Aout(idIsUs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsUs)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsTb).and.Aout(idIsTb,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTb)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsSb).and.Aout(idIsSb,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSb)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    endif
#    ifdef ICESHELF_MORPH
        IF (.not.got_var(idIsDrft).and.Aout(idIsDrft,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsDrft)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    endif
#    ifdef ICESHELF_SOF
        IF (.not.got_var(idIsSOFs).and.Aout(idIsSOFs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSOFs)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsSOFh).and.Aout(idIsSOFh,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSOFh)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    endif
#   endif
#  endif
