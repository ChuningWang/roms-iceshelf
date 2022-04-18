# ifdef ICE_MODEL
            CASE ('Hout(idIuice)')
              IF (idIuice.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIuice'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIuice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIvice)')
              IF (idIvice.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIvice'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIvice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIuiceE)')
              IF (idIuiceE.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIuiceE'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIuiceE,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIviceN)')
              IF (idIviceN.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIviceN'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIviceN,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIaice)')
              IF (idIaice.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIaice'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIaice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIhice)')
              IF (idIhice.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIhice'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIhice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIhsno)')
              IF (idIhsno.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIhsno'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIhsno,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIagei)')
              IF (idIagei.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIagei'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIagei,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idItice)')
              IF (idItice.eq.0) THEN
                IF (Master) WRITE (out,80) 'idItice'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idItice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idItmid)')
              IF (idItmid.eq.0) THEN
                IF (Master) WRITE (out,80) 'idItmid'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idItmid,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIt2ic)')
              IF (idIt2ic.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIt2ic'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIt2ic,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIsg11)')
              IF (idIsg11.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsg11'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsg11,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIsg12)')
              IF (idIsg12.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsg12'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsg12,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIsg22)')
              IF (idIsg22.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsg22'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsg22,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIutau)')
              IF (idIutau.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIutau'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIutau,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idImchu)')
              IF (idImchu.eq.0) THEN
                IF (Master) WRITE (out,80) 'idImchu'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idImchu,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIt0mk)')
              IF (idIt0mk.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIt0mk'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIt0mk,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIs0mk)')
              IF (idIs0mk.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIs0mk'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIs0mk,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idImflx)')
              IF (idImflx.eq.0) THEN
                IF (Master) WRITE (out,80) 'idImflx'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idImflx,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIwfr)')
              IF (idIwfr.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIwfr'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIwfr,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIwai)')
              IF (idIwai.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIwai'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIwai,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIwao)')
              IF (idIwao.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIwao'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIwao,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIwio)')
              IF (idIwio.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIwio'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIwio,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIwro)')
              IF (idIwro.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIwro'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIwro,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIwdiv)')
              IF (idIwdiv.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIwdiv'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIwdiv,1:Ngrids)=Lswitch(1:Ngrids)
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
            CASE ('Hout(idIalbe)')
              IF (idIalbe.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIalbe'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIalbe,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIalbI)')
              IF (idIalbI.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIalbI'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIalbI,1:Ngrids)=Lswitch(1:Ngrids)
#    ifdef ICE_ALB_CSIM
            CASE ('Hout(idIfrsw)')
              IF (idIfrsw.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIfrsw'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIfrsw,1:Ngrids)=Lswitch(1:Ngrids)
#    endif
#   endif
            CASE ('Hout(idIrfac)')
              IF (idIrfac.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIrfac'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIrfac,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqswl)')
              IF (idIqswl.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqswl'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqswl,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqlwl)')
              IF (idIqlwl.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqlwl'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqlwl,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqlai)')
              IF (idIqlai.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqlai'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqlai,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqsei)')
              IF (idIqsei.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqsei'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqsei,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqlwa)')
              IF (idIqlwa.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqlwa'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqlwa,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqswa)')
              IF (idIqswa.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqswa'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqswa,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqswi)')
              IF (idIqswi.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqswi'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqswi,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqswo)')
              IF (idIqswo.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqswo'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqswo,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIsio)')
              IF (idIsio.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsio'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsio,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIsao)')
              IF (idIsao.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsao'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsao,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqao)')
              IF (idIqao.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqao'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqao,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqai)')
              IF (idIqai.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqai'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqai,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqio)')
              IF (idIqio.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqio'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqio,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIqi2)')
              IF (idIqi2.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIqi2'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIqi2,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIwsni)')
              IF (idIwsni.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIwsni'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIwsni,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
# endif
# ifdef ICESHELF
            CASE ('Hout(idIsMr)')
              IF (idIsMr.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsMr'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsMr,1:Ngrids)=Lswitch(1:Ngrids)
#  ifdef ICESHELF_3EQN_VBC
            CASE ('Hout(idIsTs)')
              IF (idIsTs.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsTs'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsTs,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIsUs)')
              IF (idIsUs.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsUs'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsUs,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIsTb)')
              IF (idIsTb.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsTb'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsTb,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIsSb)')
              IF (idIsSb.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsSb'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsSb,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
#  ifdef ICESHELF_MORPH
            CASE ('Hout(idIsDrft)')
              IF (idIsDrft.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsDrft'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsDrft,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
#  ifdef ICESHELF_SOF
            CASE ('Hout(idIsSOFs)')
              IF (idIsSOFs.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsSOFs'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsSOFs,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Hout(idIsSOFh)')
              IF (idIsSOFh.eq.0) THEN
                IF (Master) WRITE (out,80) 'idIsSOFh'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Hout(idIsSOFh,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
# endif

# ifdef ICE_MODEL
            CASE ('Aout(idIuice)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIuice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIvice)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIvice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIuiceE)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIuiceE,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIviceN)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIviceN,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIaice)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIaice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIhice)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIhice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIhsno)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIhsno,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIagei)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIagei,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idItice)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idItice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idItmid)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idItmid,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIt2ic)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIt2ic,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIsg11)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsg11,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIsg12)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsg12,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIsg22)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsg22,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIutau)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIutau,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idImchu)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idImchu,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIt0mk)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIt0mk,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIs0mk)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIs0mk,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idImflx)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idImflx,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIwfr)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIwfr,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIwai)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIwai,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIwao)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIwao,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIwio)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIwio,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIwro)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIwro,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIwdiv)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIwdiv,1:Ngrids)=Lswitch(1:Ngrids)
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
            CASE ('Aout(idIalbe)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIalbe,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIalbI)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIalbI,1:Ngrids)=Lswitch(1:Ngrids)
#    ifdef ICE_ALB_CSIM
            CASE ('Aout(idIfrsw)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIfrsw,1:Ngrids)=Lswitch(1:Ngrids)
#    endif
#   endif
            CASE ('Aout(idIrfac)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIrfac,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqswl)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqswl,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqlwl)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqlwl,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqlai)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqlai,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqsei)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqsei,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqlwa)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqlwa,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqswa)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqswa,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqswi)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqswi,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqswo)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqswo,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIsio)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsio,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIsao)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsao,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqao)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqao,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqai)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqai,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqio)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqio,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIqi2)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIqi2,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIwsni)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIwsni,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
# endif
# ifdef ICESHELF
            CASE ('Aout(idIsMr)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsMr,1:Ngrids)=Lswitch(1:Ngrids)
#  ifdef ICESHELF_3EQN_VBC
            CASE ('Aout(idIsTs)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsTs,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIsUs)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsUs,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIsTb)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsTb,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIsSb)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsSb,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
#  ifdef ICESHELF_MORPH
            CASE ('Aout(idIsDrft)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsDrft,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
#  ifdef ICESHELF_SOF
            CASE ('Aout(idIsSOFs)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsSOFs,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Aout(idIsSOFh)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Aout(idIsSOFh,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
# endif

# ifdef ICE_MODEL
            CASE ('Qout(idIuice)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIuice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIvice)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIvice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIuiceE)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIuiceE,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIviceN)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIviceN,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIaice)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIaice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIhice)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIhice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIhsno)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIhsno,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIagei)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIagei,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idItice)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idItice,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idItmid)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idItmid,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIt2ic)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIt2ic,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIsg11)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsg11,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIsg12)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsg12,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIsg22)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsg22,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIutau)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIutau,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idImchu)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idImchu,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIt0mk)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIt0mk,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIs0mk)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIs0mk,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idImflx)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idImflx,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIwfr)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIwfr,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIwai)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIwai,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIwao)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIwao,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIwio)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIwio,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIwro)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIwro,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIwdiv)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIwdiv,1:Ngrids)=Lswitch(1:Ngrids)
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
            CASE ('Qout(idIalbe)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIalbe,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIalbI)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIalbI,1:Ngrids)=Lswitch(1:Ngrids)
#    ifdef ICE_ALB_CSIM
            CASE ('Qout(idIfrsw)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIfrsw,1:Ngrids)=Lswitch(1:Ngrids)
#    endif
#   endif
            CASE ('Qout(idIrfac)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIrfac,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqswl)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqswl,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqlwl)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqlwl,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqlai)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqlai,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqsei)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqsei,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqlwa)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqlwa,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqswa)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqswa,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqswi)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqswi,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqswo)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqswo,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIsio)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsio,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIsao)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsao,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqao)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqao,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqai)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqai,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqio)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqio,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIqi2)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIqi2,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIwsni)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIwsni,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
# endif
# ifdef ICESHELF
            CASE ('Qout(idIsMr)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsMr,1:Ngrids)=Lswitch(1:Ngrids)
#  ifdef ICESHELF_3EQN_VBC
            CASE ('Qout(idIsTs)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsTs,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIsUs)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsUs,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIsTb)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsTb,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIsSb)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsSb,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
#  ifdef ICESHELF_MORPH
            CASE ('Qout(idIsDrft)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsDrft,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
#  ifdef ICESHELF_SOF
            CASE ('Qout(idIsSOFs)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsSOFs,1:Ngrids)=Lswitch(1:Ngrids)
            CASE ('Qout(idIsSOFh)')
              Npts=load_l(Nval, Cval, Ngrids, Lswitch)
              Qout(idIsSOFh,1:Ngrids)=Lswitch(1:Ngrids)
#  endif
# endif
