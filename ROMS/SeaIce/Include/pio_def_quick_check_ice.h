# if defined ICE_MODEL || defined ICESHELF
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
        DO i=1,n_var
          IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
            got_var(idtime)=.TRUE.
#  ifdef ICE_MODEL
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIuice))) THEN
            got_var(idIuice)=.TRUE.
            QCK(ng)%pioVar(idIuice)%vd=var_desc(i)
            QCK(ng)%pioVar(idIuice)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIuice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIvice))) THEN
            got_var(idIvice)=.TRUE.
            QCK(ng)%pioVar(idIvice)%vd=var_desc(i)
            QCK(ng)%pioVar(idIvice)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIvice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIuiceE))) THEN
            got_var(idIuiceE)=.TRUE.
            QCK(ng)%pioVar(idIuiceE)%vd=var_desc(i)
            QCK(ng)%pioVar(idIuiceE)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIuiceE)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIviceN))) THEN
            got_var(idIviceN)=.TRUE.
            QCK(ng)%pioVar(idIviceN)%vd=var_desc(i)
            QCK(ng)%pioVar(idIviceN)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIviceN)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIaice))) THEN
            got_var(idIaice)=.TRUE.
            QCK(ng)%pioVar(idIaice)%vd=var_desc(i)
            QCK(ng)%pioVar(idIaice)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIaice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhice))) THEN
            got_var(idIhice)=.TRUE.
            QCK(ng)%pioVar(idIhice)%vd=var_desc(i)
            QCK(ng)%pioVar(idIhice)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIhice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhsno))) THEN
            got_var(idIhsno)=.TRUE.
            QCK(ng)%pioVar(idIhsno)%vd=var_desc(i)
            QCK(ng)%pioVar(idIhsno)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIhsno)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIagei))) THEN
            got_var(idIagei)=.TRUE.
            QCK(ng)%pioVar(idIagei)%vd=var_desc(i)
            QCK(ng)%pioVar(idIagei)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIagei)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItice))) THEN
            got_var(idItice)=.TRUE.
            QCK(ng)%pioVar(idItice)%vd=var_desc(i)
            QCK(ng)%pioVar(idItice)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idItice)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItmid))) THEN
            got_var(idItmid)=.TRUE.
            QCK(ng)%pioVar(idItmid)%vd=var_desc(i)
            QCK(ng)%pioVar(idItmid)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idItmid)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt2ic))) THEN
            got_var(idIt2ic)=.TRUE.
            QCK(ng)%pioVar(idIt2ic)%vd=var_desc(i)
            QCK(ng)%pioVar(idIt2ic)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIt2ic)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg11))) THEN
            got_var(idIsg11)=.TRUE.
            QCK(ng)%pioVar(idIsg11)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsg11)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsg11)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg12))) THEN
            got_var(idIsg12)=.TRUE.
            QCK(ng)%pioVar(idIsg12)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsg12)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsg12)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg22))) THEN
            got_var(idIsg22)=.TRUE.
            QCK(ng)%pioVar(idIsg22)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsg22)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsg22)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIutau))) THEN
            got_var(idIutau)=.TRUE.
            QCK(ng)%pioVar(idIutau)%vd=var_desc(i)
            QCK(ng)%pioVar(idIutau)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIutau)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImchu))) THEN
            got_var(idImchu)=.TRUE.
            QCK(ng)%pioVar(idImchu)%vd=var_desc(i)
            QCK(ng)%pioVar(idImchu)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idImchu)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt0mk))) THEN
            got_var(idIt0mk)=.TRUE.
            QCK(ng)%pioVar(idIt0mk)%vd=var_desc(i)
            QCK(ng)%pioVar(idIt0mk)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIt0mk)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIs0mk))) THEN
            got_var(idIs0mk)=.TRUE.
            QCK(ng)%pioVar(idIs0mk)%vd=var_desc(i)
            QCK(ng)%pioVar(idIs0mk)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIs0mk)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImflx))) THEN
            got_var(idImflx)=.TRUE.
            QCK(ng)%pioVar(idImflx)%vd=var_desc(i)
            QCK(ng)%pioVar(idImflx)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idImflx)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwfr))) THEN
            got_var(idIwfr)=.TRUE.
            QCK(ng)%pioVar(idIwfr)%vd=var_desc(i)
            QCK(ng)%pioVar(idIwfr)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIwfr)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwai))) THEN
            got_var(idIwai)=.TRUE.
            QCK(ng)%pioVar(idIwai)%vd=var_desc(i)
            QCK(ng)%pioVar(idIwai)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIwai)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwao))) THEN
            got_var(idIwao)=.TRUE.
            QCK(ng)%pioVar(idIwao)%vd=var_desc(i)
            QCK(ng)%pioVar(idIwao)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIwao)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwio))) THEN
            got_var(idIwio)=.TRUE.
            QCK(ng)%pioVar(idIwio)%vd=var_desc(i)
            QCK(ng)%pioVar(idIwio)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIwio)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwro))) THEN
            got_var(idIwro)=.TRUE.
            QCK(ng)%pioVar(idIwro)%vd=var_desc(i)
            QCK(ng)%pioVar(idIwro)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIwro)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwdiv))) THEN
            got_var(idIwdiv)=.TRUE.
            QCK(ng)%pioVar(idIwdiv)%vd=var_desc(i)
            QCK(ng)%pioVar(idIwdiv)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIwdiv)%gtype=r2dvar
#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIalbe))) THEN
            got_var(idIalbe)=.TRUE.
            QCK(ng)%pioVar(idIalbe)%vd=var_desc(i)
            QCK(ng)%pioVar(idIalbe)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIalbe)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIalbI))) THEN
            got_var(idIalbI)=.TRUE.
            QCK(ng)%pioVar(idIalbI)%vd=var_desc(i)
            QCK(ng)%pioVar(idIalbI)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIalbI)%gtype=r2dvar
#     ifdef ICE_ALB_CSIM
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIfrsw))) THEN
            got_var(idIfrsw)=.TRUE.
            QCK(ng)%pioVar(idIfrsw)%vd=var_desc(i)
            QCK(ng)%pioVar(idIfrsw)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIfrsw)%gtype=r2dvar
#     endif
#    endif
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIrfac))) THEN
            got_var(idIrfac)=.TRUE.
            QCK(ng)%pioVar(idIrfac)%vd=var_desc(i)
            QCK(ng)%pioVar(idIrfac)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIrfac)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIlrdn))) THEN
            got_var(idIlrdn)=.TRUE.
            QCK(ng)%pioVar(idIlrdn)%vd=var_desc(i)
            QCK(ng)%pioVar(idIlrdn)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIlrdn)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsrdn))) THEN
            got_var(idIsrdn)=.TRUE.
            QCK(ng)%pioVar(idIsrdn)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsrdn)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsrdn)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIlatI))) THEN
            got_var(idIlatI)=.TRUE.
            QCK(ng)%pioVar(idIlatI)%vd=var_desc(i)
            QCK(ng)%pioVar(idIlatI)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIlatI)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsenI))) THEN
            got_var(idIsenI)=.TRUE.
            QCK(ng)%pioVar(idIsenI)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsenI)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsenI)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIlrup))) THEN
            got_var(idIlrup)=.TRUE.
            QCK(ng)%pioVar(idIlrup)%vd=var_desc(i)
            QCK(ng)%pioVar(idIlrup)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIlrup)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsrup))) THEN
            got_var(idIsrup)=.TRUE.
            QCK(ng)%pioVar(idIsrup)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsrup)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsrup)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsrin))) THEN
            got_var(idIsrin)=.TRUE.
            QCK(ng)%pioVar(idIsrin)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsrin)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsrin)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsrth))) THEN
            got_var(idIsrth)=.TRUE.
            QCK(ng)%pioVar(idIsrth)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsrth)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsrth)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsfI))) THEN
            got_var(idIsfI)=.TRUE.
            QCK(ng)%pioVar(idIsfI)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsfI)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsfI)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsfL))) THEN
            got_var(idIsfL)=.TRUE.
            QCK(ng)%pioVar(idIsfL)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsfL)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsfL)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqao))) THEN
            got_var(idIqao)=.TRUE.
            QCK(ng)%pioVar(idIqao)%vd=var_desc(i)
            QCK(ng)%pioVar(idIqao)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIqao)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqai))) THEN
            got_var(idIqai)=.TRUE.
            QCK(ng)%pioVar(idIqai)%vd=var_desc(i)
            QCK(ng)%pioVar(idIqai)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIqai)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqio))) THEN
            got_var(idIqio)=.TRUE.
            QCK(ng)%pioVar(idIqio)%vd=var_desc(i)
            QCK(ng)%pioVar(idIqio)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIqio)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqi2))) THEN
            got_var(idIqi2)=.TRUE.
            QCK(ng)%pioVar(idIqi2)%vd=var_desc(i)
            QCK(ng)%pioVar(idIqi2)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIqi2)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsnoi))) THEN
            got_var(idIsnoi)=.TRUE.
            QCK(ng)%pioVar(idIsnoi)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsnoi)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsnoi)%gtype=r2dvar
#   endif
#  endif
#  ifdef ICESHELF
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsMr))) THEN
            got_var(idIsMr)=.TRUE.
            QCK(ng)%pioVar(idIsMr)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsMr)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsMr)%gtype=r2dvar
#   ifdef ICESHELF_3EQN_VBC
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTs))) THEN
            got_var(idIsTs)=.TRUE.
            QCK(ng)%pioVar(idIsTs)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsTs)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsTs)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsUs))) THEN
            got_var(idIsUs)=.TRUE.
            QCK(ng)%pioVar(idIsUs)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsUs)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsUs)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTb))) THEN
            got_var(idIsTb)=.TRUE.
            QCK(ng)%pioVar(idIsTb)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsTb)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsTb)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSb))) THEN
            got_var(idIsSb)=.TRUE.
            QCK(ng)%pioVar(idIsSb)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsSb)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsSb)%gtype=r2dvar
#   endif
#   ifdef ICESHELF_MORPH
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsDrft))) THEN
            got_var(idIsDrft)=.TRUE.
            QCK(ng)%pioVar(idIsDrft)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsDrft)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsDrft)%gtype=r2dvar
#   endif
#   ifdef ICESHELF_SOF
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSOFs))) THEN
            got_var(idIsSOFs)=.TRUE.
            QCK(ng)%pioVar(idIsSOFs)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsSOFs)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsSOFs)%gtype=r2dvar
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSOFh))) THEN
            got_var(idIsSOFh)=.TRUE.
            QCK(ng)%pioVar(idIsSOFh)%vd=var_desc(i)
            QCK(ng)%pioVar(idIsSOFh)%dkind=PIO_FOUT
            QCK(ng)%pioVar(idIsSOFh)%gtype=r2dvar
#   endif
#  endif
          END IF
        END DO
!
!  Check if variables are available in input NetCDF file.
!
#  ifdef ICE_MODEL
        IF (.not.got_var(idIuice).and.Qout(idIuice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIvice).and.Qout(idIvice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIvice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIuiceE).and.Qout(idIuiceE,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuiceE)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIviceN).and.Qout(idIviceN,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIviceN)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIaice).and.Qout(idIaice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIaice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhice).and.Qout(idIhice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhsno).and.Qout(idIhsno,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhsno)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIagei).and.Qout(idIagei,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIagei)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItice).and.Qout(idItice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItmid).and.Qout(idItmid,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItmid)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt2ic).and.Qout(idIt2ic,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt2ic)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg11).and.Qout(idIsg11,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg11)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg12).and.Qout(idIsg12,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg12)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg22).and.Qout(idIsg22,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg22)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIutau).and.Qout(idIutau,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIutau)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImchu).and.Qout(idImchu,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImchu)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt0mk).and.Qout(idIt0mk,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIs0mk).and.Qout(idIs0mk,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIs0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImflx).and.Qout(idImflx,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImflx)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwfr).and.Qout(idIwfr,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwfr)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwai).and.Qout(idIwai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwai)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwao).and.Qout(idIwao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwio).and.Qout(idIwio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwro).and.Qout(idIwro,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwro)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwdiv).and.Qout(idIwdiv,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwdiv)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   ifdef ICE_DIAGS
#    ifdef ICE_ALBEDO
        IF (.not.got_var(idIalbe).and.Qout(idIalbe,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIalbe)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIalbI).and.Qout(idIalbI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIalbI)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#     ifdef ICE_ALB_CSIM
        IF (.not.got_var(idIfrsw).and.Qout(idIfrsw,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIfrsw)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#     endif
#    endif
        IF (.not.got_var(idIrfac).and.Qout(idIrfac,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIrfac)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIlrdn).and.Qout(idIlrdn,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIlrdn)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsrdn).and.Qout(idIsrdn,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsrdn)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIlatI).and.Qout(idIlatI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIlatI)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsenI).and.Qout(idIsenI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsenI)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIlrup).and.Qout(idIlrup,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIlrup)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsrup).and.Qout(idIsrup,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsrup)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsrin).and.Qout(idIsrin,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsrin)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsrth).and.Qout(idIsrth,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsrth)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsfI).and.Qout(idIsfI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsfI)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsfL).and.Qout(idIsfL,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsfL)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqao).and.Qout(idIqao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqai).and.Qout(idIqai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqai)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqio).and.Qout(idIqio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqi2).and.Qout(idIqi2,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqi2)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsnoi).and.Qout(idIsnoi,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsnoi)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#  endif
#  ifdef ICESHELF
        IF (.not.got_var(idIsMr).and.Qout(idIsMr,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsMr)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   ifdef ICESHELF_3EQN_VBC
        IF (.not.got_var(idIsTs).and.Qout(idIsTs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTs)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsUs).and.Qout(idIsUs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsUs)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsTb).and.Qout(idIsTb,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTb)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsSb).and.Qout(idIsSb,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSb)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#   ifdef ICESHELF_MORPH
        IF (.not.got_var(idIsDrft).and.Qout(idIsDrft,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsDrft)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#   ifdef ICESHELF_SOF
        IF (.not.got_var(idIsSOFs).and.Qout(idIsSOFs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSOFs)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsSOFh).and.Qout(idIsSOFh,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSOFh)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#   endif
#  endif
# endif
