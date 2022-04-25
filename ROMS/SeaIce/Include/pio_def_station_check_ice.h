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
            STA(ng)%pioVar(idIuice)%vd=var_desc(i)
            STA(ng)%pioVar(idIuice)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIuice)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIvice))) THEN
            got_var(idIvice)=.TRUE.
            STA(ng)%pioVar(idIvice)%vd=var_desc(i)
            STA(ng)%pioVar(idIvice)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIvice)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIuiceE))) THEN
            got_var(idIuiceE)=.TRUE.
            STA(ng)%pioVar(idIuiceE)%vd=var_desc(i)
            STA(ng)%pioVar(idIuiceE)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIuiceE)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIviceN))) THEN
            got_var(idIviceN)=.TRUE.
            STA(ng)%pioVar(idIviceN)%vd=var_desc(i)
            STA(ng)%pioVar(idIviceN)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIviceN)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIaice))) THEN
            got_var(idIaice)=.TRUE.
            STA(ng)%pioVar(idIaice)%vd=var_desc(i)
            STA(ng)%pioVar(idIaice)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIaice)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhice))) THEN
            got_var(idIhice)=.TRUE.
            STA(ng)%pioVar(idIhice)%vd=var_desc(i)
            STA(ng)%pioVar(idIhice)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIhice)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIhsno))) THEN
            got_var(idIhsno)=.TRUE.
            STA(ng)%pioVar(idIhsno)%vd=var_desc(i)
            STA(ng)%pioVar(idIhsno)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIhsno)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIagei))) THEN
            got_var(idIagei)=.TRUE.
            STA(ng)%pioVar(idIagei)%vd=var_desc(i)
            STA(ng)%pioVar(idIagei)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIagei)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItice))) THEN
            got_var(idItice)=.TRUE.
            STA(ng)%pioVar(idItice)%vd=var_desc(i)
            STA(ng)%pioVar(idItice)%dkind=PIO_FOUT
            STA(ng)%pioVar(idItice)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idItmid))) THEN
            got_var(idItmid)=.TRUE.
            STA(ng)%pioVar(idItmid)%vd=var_desc(i)
            STA(ng)%pioVar(idItmid)%dkind=PIO_FOUT
            STA(ng)%pioVar(idItmid)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt2ic))) THEN
            got_var(idIt2ic)=.TRUE.
            STA(ng)%pioVar(idIt2ic)%vd=var_desc(i)
            STA(ng)%pioVar(idIt2ic)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIt2ic)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg11))) THEN
            got_var(idIsg11)=.TRUE.
            STA(ng)%pioVar(idIsg11)%vd=var_desc(i)
            STA(ng)%pioVar(idIsg11)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsg11)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg12))) THEN
            got_var(idIsg12)=.TRUE.
            STA(ng)%pioVar(idIsg12)%vd=var_desc(i)
            STA(ng)%pioVar(idIsg12)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsg12)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsg22))) THEN
            got_var(idIsg22)=.TRUE.
            STA(ng)%pioVar(idIsg22)%vd=var_desc(i)
            STA(ng)%pioVar(idIsg22)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsg22)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIutau))) THEN
            got_var(idIutau)=.TRUE.
            STA(ng)%pioVar(idIutau)%vd=var_desc(i)
            STA(ng)%pioVar(idIutau)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIutau)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImchu))) THEN
            got_var(idImchu)=.TRUE.
            STA(ng)%pioVar(idImchu)%vd=var_desc(i)
            STA(ng)%pioVar(idImchu)%dkind=PIO_FOUT
            STA(ng)%pioVar(idImchu)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIt0mk))) THEN
            got_var(idIt0mk)=.TRUE.
            STA(ng)%pioVar(idIt0mk)%vd=var_desc(i)
            STA(ng)%pioVar(idIt0mk)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIt0mk)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIs0mk))) THEN
            got_var(idIs0mk)=.TRUE.
            STA(ng)%pioVar(idIs0mk)%vd=var_desc(i)
            STA(ng)%pioVar(idIs0mk)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIs0mk)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idImflx))) THEN
            got_var(idImflx)=.TRUE.
            STA(ng)%pioVar(idImflx)%vd=var_desc(i)
            STA(ng)%pioVar(idImflx)%dkind=PIO_FOUT
            STA(ng)%pioVar(idImflx)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwfr))) THEN
            got_var(idIwfr)=.TRUE.
            STA(ng)%pioVar(idIwfr)%vd=var_desc(i)
            STA(ng)%pioVar(idIwfr)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIwfr)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwai))) THEN
            got_var(idIwai)=.TRUE.
            STA(ng)%pioVar(idIwai)%vd=var_desc(i)
            STA(ng)%pioVar(idIwai)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIwai)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwao))) THEN
            got_var(idIwao)=.TRUE.
            STA(ng)%pioVar(idIwao)%vd=var_desc(i)
            STA(ng)%pioVar(idIwao)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIwao)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwio))) THEN
            got_var(idIwio)=.TRUE.
            STA(ng)%pioVar(idIwio)%vd=var_desc(i)
            STA(ng)%pioVar(idIwio)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIwio)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwro))) THEN
            got_var(idIwro)=.TRUE.
            STA(ng)%pioVar(idIwro)%vd=var_desc(i)
            STA(ng)%pioVar(idIwro)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIwro)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwdiv))) THEN
            got_var(idIwdiv)=.TRUE.
            STA(ng)%pioVar(idIwdiv)%vd=var_desc(i)
            STA(ng)%pioVar(idIwdiv)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIwdiv)%gtype=0
#    ifdef ICE_DIAGS
#     ifdef ICE_ALBEDO
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIalbO))) THEN
            got_var(idIalbO)=.TRUE.
            STA(ng)%pioVar(idIalbO)%vd=var_desc(i)
            STA(ng)%pioVar(idIalbO)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIalbO)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIalbI))) THEN
            got_var(idIalbI)=.TRUE.
            STA(ng)%pioVar(idIalbI)%vd=var_desc(i)
            STA(ng)%pioVar(idIalbI)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIalbI)%gtype=0
#      ifdef ICE_ALB_CSIM
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIfrsw))) THEN
            got_var(idIfrsw)=.TRUE.
            STA(ng)%pioVar(idIfrsw)%vd=var_desc(i)
            STA(ng)%pioVar(idIfrsw)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIfrsw)%gtype=0
#      endif
#     endif
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswi))) THEN
            got_var(idIqswi)=.TRUE.
            STA(ng)%pioVar(idIqswi)%vd=var_desc(i)
            STA(ng)%pioVar(idIqswi)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqswi)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlwi))) THEN
            got_var(idIqlwi)=.TRUE.
            STA(ng)%pioVar(idIqlwi)%vd=var_desc(i)
            STA(ng)%pioVar(idIqlwi)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqlwi)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlai))) THEN
            got_var(idIqlai)=.TRUE.
            STA(ng)%pioVar(idIqlai)%vd=var_desc(i)
            STA(ng)%pioVar(idIqlai)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqlai)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqsei))) THEN
            got_var(idIqsei)=.TRUE.
            STA(ng)%pioVar(idIqsei)%vd=var_desc(i)
            STA(ng)%pioVar(idIqsei)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqsei)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswl))) THEN
            got_var(idIqswl)=.TRUE.
            STA(ng)%pioVar(idIqswl)%vd=var_desc(i)
            STA(ng)%pioVar(idIqswl)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqswl)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlwl))) THEN
            got_var(idIqlwl)=.TRUE.
            STA(ng)%pioVar(idIqlwl)%vd=var_desc(i)
            STA(ng)%pioVar(idIqlwl)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqlwl)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqlwa))) THEN
            got_var(idIqlwa)=.TRUE.
            STA(ng)%pioVar(idIqlwa)%vd=var_desc(i)
            STA(ng)%pioVar(idIqlwa)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqlwa)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswa))) THEN
            got_var(idIqswa)=.TRUE.
            STA(ng)%pioVar(idIqswa)%vd=var_desc(i)
            STA(ng)%pioVar(idIqswa)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqswa)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqswo))) THEN
            got_var(idIqswo)=.TRUE.
            STA(ng)%pioVar(idIqswo)%vd=var_desc(i)
            STA(ng)%pioVar(idIqswo)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqswo)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqao))) THEN
            got_var(idIqao)=.TRUE.
            STA(ng)%pioVar(idIqao)%vd=var_desc(i)
            STA(ng)%pioVar(idIqao)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqao)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqai))) THEN
            got_var(idIqai)=.TRUE.
            STA(ng)%pioVar(idIqai)%vd=var_desc(i)
            STA(ng)%pioVar(idIqai)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqai)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqio))) THEN
            got_var(idIqio)=.TRUE.
            STA(ng)%pioVar(idIqio)%vd=var_desc(i)
            STA(ng)%pioVar(idIqio)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqio)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIqi2))) THEN
            got_var(idIqi2)=.TRUE.
            STA(ng)%pioVar(idIqi2)%vd=var_desc(i)
            STA(ng)%pioVar(idIqi2)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIqi2)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsao))) THEN
            got_var(idIsao)=.TRUE.
            STA(ng)%pioVar(idIsao)%vd=var_desc(i)
            STA(ng)%pioVar(idIsao)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsao)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsio))) THEN
            got_var(idIsio)=.TRUE.
            STA(ng)%pioVar(idIsio)%vd=var_desc(i)
            STA(ng)%pioVar(idIsio)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsio)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIwsni))) THEN
            got_var(idIwsni)=.TRUE.
            STA(ng)%pioVar(idIwsni)%vd=var_desc(i)
            STA(ng)%pioVar(idIwsni)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIwsni)%gtype=0
#    endif
#   endif
#   ifdef ICESHELF
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsMr))) THEN
            got_var(idIsMr)=.TRUE.
            STA(ng)%pioVar(idIsMr)%vd=var_desc(i)
            STA(ng)%pioVar(idIsMr)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsMr)%gtype=0
#    ifdef ICESHELF_3EQN_VBC
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTs))) THEN
            got_var(idIsTs)=.TRUE.
            STA(ng)%pioVar(idIsTs)%vd=var_desc(i)
            STA(ng)%pioVar(idIsTs)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsTs)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsUs))) THEN
            got_var(idIsUs)=.TRUE.
            STA(ng)%pioVar(idIsUs)%vd=var_desc(i)
            STA(ng)%pioVar(idIsUs)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsUs)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsTb))) THEN
            got_var(idIsTb)=.TRUE.
            STA(ng)%pioVar(idIsTb)%vd=var_desc(i)
            STA(ng)%pioVar(idIsTb)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsTb)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSb))) THEN
            got_var(idIsSb)=.TRUE.
            STA(ng)%pioVar(idIsSb)%vd=var_desc(i)
            STA(ng)%pioVar(idIsSb)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsSb)%gtype=0
#    endif
#    ifdef ICESHELF_MORPH
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsDrft))) THEN
            got_var(idIsDrft)=.TRUE.
            STA(ng)%pioVar(idIsDrft)%vd=var_desc(i)
            STA(ng)%pioVar(idIsDrft)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsDrft)%gtype=0
#    endif
#    ifdef ICESHELF_SOF
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSOFs))) THEN
            got_var(idIsSOFs)=.TRUE.
            STA(ng)%pioVar(idIsSOFs)%vd=var_desc(i)
            STA(ng)%pioVar(idIsSOFs)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsSOFs)%gtype=0
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idIsSOFh))) THEN
            got_var(idIsSOFh)=.TRUE.
            STA(ng)%pioVar(idIsSOFh)%vd=var_desc(i)
            STA(ng)%pioVar(idIsSOFh)%dkind=PIO_FOUT
            STA(ng)%pioVar(idIsSOFh)%gtype=0
#    endif
#   endif
          END IF
        END DO
!
!  Check if station variables are available in input NetCDF file.
!
#   ifdef ICE_MODEL
        IF (.not.got_var(idIuice).and.Sout(idIuice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIvice).and.Sout(idIvice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIvice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIuiceE).and.Sout(idIuiceE,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIuiceE)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIviceN).and.Sout(idIviceN,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIviceN)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIaice).and.Sout(idIaice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIaice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhice).and.Sout(idIhice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIhsno).and.Sout(idIhsno,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIhsno)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIagei).and.Sout(idIagei,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIagei)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItice).and.Sout(idItice,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItice)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idItmid).and.Sout(idItmid,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idItmid)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt2ic).and.Sout(idIt2ic,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt2ic)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg11).and.Sout(idIsg11,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg11)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg12).and.Sout(idIsg12,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg12)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsg22).and.Sout(idIsg22,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsg22)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIutau).and.Sout(idIutau,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIutau)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImchu).and.Sout(idImchu,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImchu)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIt0mk).and.Sout(idIt0mk,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIt0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIs0mk).and.Sout(idIs0mk,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIs0mk)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idImflx).and.Sout(idImflx,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idImflx)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwfr).and.Sout(idIwfr,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwfr)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwai).and.Sout(idIwai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwai)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwao).and.Sout(idIwao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwio).and.Sout(idIwio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwro).and.Sout(idIwro,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwro)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwdiv).and.Sout(idIwdiv,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwdiv)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    ifdef ICE_DIAGS
#     ifdef ICE_ALBEDO
        IF (.not.got_var(idIalbO).and.Sout(idIalbO,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIalbO)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIalbI).and.Sout(idIalbI,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIalbI)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#      ifdef ICE_ALB_CSIM
        IF (.not.got_var(idIfrsw).and.Sout(idIfrsw,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIfrsw)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#      endif
#     endif
        IF (.not.got_var(idIqswi).and.Sout(idIqswi,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswi)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlwi).and.Sout(idIqlwi,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlwi)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlai).and.Sout(idIqlai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlai)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqsei).and.Sout(idIqsei,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqsei)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqswl).and.Sout(idIqswl,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswl)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlwl).and.Sout(idIqlwl,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlwl)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqlwa).and.Sout(idIqlwa,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqlwa)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqswa).and.Sout(idIqswa,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswa)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqswo).and.Sout(idIqswo,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqswo)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqao).and.Sout(idIqao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqai).and.Sout(idIqai,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqai)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqio).and.Sout(idIqio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIqi2).and.Sout(idIqi2,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIqi2)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsao).and.Sout(idIsao,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsao)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsio).and.Sout(idIsio,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsio)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIwsni).and.Sout(idIwsni,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIwsni)),         &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    endif
#   endif
#   ifdef ICESHELF
        IF (.not.got_var(idIsMr).and.Sout(idIsMr,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsMr)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    ifdef ICESHELF_3EQN_VBC
        IF (.not.got_var(idIsTs).and.Sout(idIsTs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTs)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsUs).and.Sout(idIsUs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsUs)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsTb).and.Sout(idIsTb,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsTb)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsSb).and.Sout(idIsSb,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSb)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    endif
#    ifdef ICESHELF_MORPH
        IF (.not.got_var(idIsDrft).and.Sout(idIsDrft,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsDrft)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    endif
#    ifdef ICESHELF_SOF
        IF (.not.got_var(idIsSOFs).and.Sout(idIsSOFs,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSOFs)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idIsSOFh).and.Sout(idIsSOFh,ng)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idIsSOFh)),        &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#    endif
#   endif
#  endif
