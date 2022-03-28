# ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
!  Read in 2D ice momentum in the XI-direction.
!
        IF (get_var(idIuice)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIuice)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=u2dvar
            IF (Perfect2D) THEN
              IF (KIND(ICE(ng)%ui).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_rubar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_rubar(ng)
              END IF
              status=nf_fread3d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIuice), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj, 1, 2,               &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % umask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % ui,                           &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % ui)
#  endif
            ELSE
              IF (KIND(ICE(ng)%ui).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_u2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_u2dvar(ng)
              END IF
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIuice), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % umask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % ui(:,:,Tindex),               &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % ui(:,:,Tindex))
#  endif
            END IF
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIuice)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIuice)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIuice)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIuice)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in 2D ice momentum in the ETA-direction.
!
        IF (get_var(idIvice)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIvice)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=v2dvar
            IF (Perfect2D) THEN
              IF (KIND(ICE(ng)%vi).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_rvbar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_rvbar(ng)
              END IF
              status=nf_fread3d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIvice), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj, 1, 2,               &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % vmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % vi,                           &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % vi)
#  endif
            ELSE
              IF (KIND(ICE(ng)%vi).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_v2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_v2dvar(ng)
              END IF
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIvice), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % vmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % vi(:,:,Tindex),               &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % vi(:,:,Tindex))
#  endif
            END IF
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIvice)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIvice)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIvice)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIvice)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in ice concentration.
!
        IF (get_var(idIaice)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIaice)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (Perfect2D) THEN
              IF (KIND(ICE(ng)%ai).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_rzeta(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_rzeta(ng)
              END IF
              status=nf_fread3d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIaice), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj, 1, 2,               &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % ai,                           &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % ai)
#  endif
            ELSE
              IF (KIND(ICE(ng)%ai).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_r2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_r2dvar(ng)
              END IF
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIaice), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % ai(:,:,Tindex),               &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % ai(:,:,Tindex))
#  endif
            END IF
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIaice)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIaice)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIaice)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIaice)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in ice average thickness.
!
        IF (get_var(idIhice)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIhice)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (Perfect2D) THEN
              IF (KIND(ICE(ng)%hi).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_rzeta(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_rzeta(ng)
              END IF
              status=nf_fread3d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIhice), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj, 1, 2,               &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % hi,                           &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % hi)
#  endif
            ELSE
              IF (KIND(ICE(ng)%hi).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_r2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_r2dvar(ng)
              END IF
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIhice), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % hi(:,:,Tindex),               &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % hi(:,:,Tindex))
#  endif
            END IF
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIhice)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIhice)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIhice)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIhice)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in snow thickness.
!
        IF (get_var(idIhsno)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIhsno)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (Perfect2D) THEN
              IF (KIND(ICE(ng)%hsn).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_rzeta(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_rzeta(ng)
              END IF
              status=nf_fread3d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIhsno), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj, 1, 2,               &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % hsn,                          &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % hsn)
#  endif
            ELSE
              IF (KIND(ICE(ng)%hsn).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_r2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_r2dvar(ng)
              END IF
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIhsno), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % hsn(:,:,Tindex),              &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % hsn(:,:,Tindex))
#  endif
            END IF
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIhsno)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIhsno)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIhsno)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIhsno)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in ice age.
!
        IF (get_var(idIagei)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIagei)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (Perfect2D) THEN
              IF (KIND(ICE(ng)%ageice).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_rzeta(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_rzeta(ng)
              END IF
              status=nf_fread3d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIagei), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj, 1, 2,               &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % ageice,                       &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % ageice)
#  endif
            ELSE
              IF (KIND(ICE(ng)%ageice).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_r2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_r2dvar(ng)
              END IF
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIagei), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % ageice(:,:,Tindex),           &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % ageice(:,:,Tindex))
#  endif
            END IF
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIagei)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIagei)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIagei)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIagei)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in temperature of ice/snow surface.
!
        IF (get_var(idItice)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idItice)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (KIND(ICE(ng)%tis).eq.8) THEN
              my_pioVar%dkind=PIO_double
              ioDesc => ioDesc_dp_r2dvar(ng)
            ELSE
              my_pioVar%dkind=PIO_real
              ioDesc => ioDesc_sp_r2dvar(ng)
            END IF
            status=nf_fread2d(ng, IDmod, ncname, pioFile,               &
     &                        Vname(1,idItice), my_pioVar,              &
     &                        InpRec, ioDesc, Vsize,                    &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
#  ifdef MASKING
     &                        GRID(ng) % rmask,                         &
#  endif
#  ifdef CHECKSUM
     &                        ICE(ng) % tis,                            &
     &                        checksum = Fhash)
#  else
     &                        ICE(ng) % tis)
#  endif
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idItice)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idItice)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idItice)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idItice)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in temperature of internal ice.
!
        IF (get_var(idItmid)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idItmid)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (Perfect2D) THEN
              IF (KIND(ICE(ng)%ti).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_rzeta(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_rzeta(ng)
              END IF
              status=nf_fread3d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idItmid), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj, 1, 2,               &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % ti,                           &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % ti)
#  endif
            ELSE
              IF (KIND(ICE(ng)%ti).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_r2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_r2dvar(ng)
              END IF
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idItmid), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % ti(:,:,Tindex),               &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % ti(:,:,Tindex))
#  endif
            END IF
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idItmid)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idItmid)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idItmid)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idItmid)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in internal ice stress component 11.
!
        IF (get_var(idIsg11)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIsg11)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (Perfect2D) THEN
              IF (KIND(ICE(ng)%sig11).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_rzeta(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_rzeta(ng)
              END IF
              status=nf_fread3d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIsg11), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj, 1, 2,               &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % sig11,                        &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % sig11)
#  endif
            ELSE
              IF (KIND(ICE(ng)%sig11).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_r2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_r2dvar(ng)
              END IF
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIsg11), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % sig11(:,:,Tindex),            &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % sig11(:,:,Tindex))
#  endif
            END IF
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIsg11)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIsg11)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIsg11)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIsg11)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in internal ice stress component 12.
!
        IF (get_var(idIsg12)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIsg12)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (Perfect2D) THEN
              IF (KIND(ICE(ng)%sig12).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_rzeta(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_rzeta(ng)
              END IF
              status=nf_fread3d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIsg12), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj, 1, 2,               &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % sig12,                        &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % sig12)
#  endif
            ELSE
              IF (KIND(ICE(ng)%sig12).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_r2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_r2dvar(ng)
              END IF
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIsg12), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % sig12(:,:,Tindex),            &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % sig12(:,:,Tindex))
#  endif
            END IF
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIsg12)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIsg12)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIsg12)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIsg12)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in internal ice stress component 22.
!
        IF (get_var(idIsg22)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIsg22)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (Perfect2D) THEN
              IF (KIND(ICE(ng)%sig22).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_rzeta(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_rzeta(ng)
              END IF
              status=nf_fread3d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIsg22), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj, 1, 2,               &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % sig22,                        &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % sig22)
#  endif
            ELSE
              IF (KIND(ICE(ng)%sig22).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_r2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_r2dvar(ng)
              END IF
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIsg22), my_pioVar,            &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
#  ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#  endif
#  ifdef CHECKSUM
     &                          ICE(ng) % sig22(:,:,Tindex),            &
     &                          checksum = Fhash)
#  else
     &                          ICE(ng) % sig22(:,:,Tindex))
#  endif
            END IF
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIsg22)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIsg22)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIsg22)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIsg22)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
#  ifndef ICE_MOM_BULK
!
!  Read in ice-water friction velocity.
!
        IF (get_var(idIutau)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIutau)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (KIND(ICE(ng)%utau_iw).eq.8) THEN
              my_pioVar%dkind=PIO_double
              ioDesc => ioDesc_dp_r2dvar(ng)
            ELSE
              my_pioVar%dkind=PIO_real
              ioDesc => ioDesc_sp_r2dvar(ng)
            END IF
            status=nf_fread2d(ng, IDmod, ncname, pioFile,               &
     &                        Vname(1,idIutau), my_pioVar,              &
     &                        InpRec, ioDesc, Vsize,                    &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
#   ifdef MASKING
     &                        GRID(ng) % rmask,                         &
#   endif
#   ifdef CHECKSUM
     &                        ICE(ng) % utau_iw,                        &
     &                        checksum = Fhash)
#   else
     &                        ICE(ng) % utau_iw)
#   endif
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIutau)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#   ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIutau)), Fmin, Fmax,   &
     &                            Fhash
#   else
                WRITE (stdout,70) TRIM(Vname(2,idIutau)), Fmin, Fmax
#   endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIutau)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in ice-water momentum transfer coefficient.
!
        IF (get_var(idImchu)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idImchu)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (KIND(ICE(ng)%chu_iw).eq.8) THEN
              my_pioVar%dkind=PIO_double
              ioDesc => ioDesc_dp_r2dvar(ng)
            ELSE
              my_pioVar%dkind=PIO_real
              ioDesc => ioDesc_sp_r2dvar(ng)
            END IF
            status=nf_fread2d(ng, IDmod, ncname, pioFile,               &
     &                        Vname(1,idImchu), my_pioVar,              &
     &                        InpRec, ioDesc, Vsize,                    &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
#   ifdef MASKING
     &                        GRID(ng) % rmask,                         &
#   endif
#   ifdef CHECKSUM
     &                        ICE(ng) % chu_iw,                         &
     &                        checksum = Fhash)
#   else
     &                        ICE(ng) % chu_iw)
#   endif
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idImchu)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#   ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idImchu)), Fmin, Fmax,   &
     &                            Fhash
#   else
                WRITE (stdout,70) TRIM(Vname(2,idImchu)), Fmin, Fmax
#   endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idImchu)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
#  endif
!
!  Read in temperature of molecular sublayer under ice.
!
        IF (get_var(idIt0mk)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIt0mk)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (KIND(ICE(ng)%t0mk).eq.8) THEN
              my_pioVar%dkind=PIO_double
              ioDesc => ioDesc_dp_r2dvar(ng)
            ELSE
              my_pioVar%dkind=PIO_real
              ioDesc => ioDesc_sp_r2dvar(ng)
            END IF
            status=nf_fread2d(ng, IDmod, ncname, pioFile,               &
     &                        Vname(1,idIt0mk), my_pioVar,              &
     &                        InpRec, ioDesc, Vsize,                    &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
#  ifdef MASKING
     &                        GRID(ng) % rmask,                         &
#  endif
#  ifdef CHECKSUM
     &                        ICE(ng) % t0mk,                           &
     &                        checksum = Fhash)
#  else
     &                        ICE(ng) % t0mk)
#  endif
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIt0mk)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIt0mk)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIt0mk)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIt0mk)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in salinity of molecular sublayer under ice.
!
        IF (get_var(idIs0mk)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIs0mk)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (KIND(ICE(ng)%s0mk).eq.8) THEN
              my_pioVar%dkind=PIO_double
              ioDesc => ioDesc_dp_r2dvar(ng)
            ELSE
              my_pioVar%dkind=PIO_real
              ioDesc => ioDesc_sp_r2dvar(ng)
            END IF
            status=nf_fread2d(ng, IDmod, ncname, pioFile,               &
     &                        Vname(1,idIs0mk), my_pioVar,              &
     &                        InpRec, ioDesc, Vsize,                    &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
#  ifdef MASKING
     &                        GRID(ng) % rmask,                         &
#  endif
#  ifdef CHECKSUM
     &                        ICE(ng) % s0mk,                           &
     &                        checksum = Fhash)
#  else
     &                        ICE(ng) % s0mk)
#  endif
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIs0mk)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#  ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIs0mk)), Fmin, Fmax,   &
     &                            Fhash
#  else
                WRITE (stdout,70) TRIM(Vname(2,idIs0mk)), Fmin, Fmax
#  endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIs0mk)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
#  ifdef PERFECT_RESTART
!
!  Read in surface active tracer fluxes.
!
        DO itrc=1,NAT
          IF (get_var(idTsur(itrc))) THEN
            foundit=find_string(var_name, n_var,                        &
     &                          TRIM(Vname(1,idTsur(itrc))), vindex)
            IF (foundit) THEN
              scale=1.0_dp
              my_pioVar%vd=var_desc(vindex)
              my_pioVar%gtype=r2dvar
              IF (KIND(FORCES(ng)%stflx_save).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_r2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_r2dvar(ng)
              END IF
!
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idTsur(itrc)), my_pioVar,       &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          scale, Fmin, Fmax,                      &
#   ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#   endif
#   ifdef CHECKSUM
     &                          FORCES(ng)% stflx_save(:,:,itrc),          &
     &                          checksum = Fhash)
#   else
     &                          FORCES(ng)% stflx_save(:,:,itrc))
#   endif
              IF (FoundError(status, PIO_noerr, __LINE__, MyFile))      &
     &          THEN
                IF (Master) THEN
                  WRITE (stdout,60) string,                             &
     &                              TRIM(Vname(1,idTsur(itrc))),        &
     &                              InpRec, TRIM(ncname)
                END IF
                exit_flag=2
                ioerror=status
                RETURN
              ELSE
                IF (Master) THEN
#   ifdef CHECKSUM
                  WRITE (stdout,70) TRIM(Vname(2,idTsur(itrc)))//       &
     &                              ', saved stflux', Fmin, Fmax,       &
     &                              Fhash
#   else
                  WRITE (stdout,70) TRIM(Vname(2,idTsur(itrc)))//       &
     &                              ', saved stflux', Fmin, Fmax
#   endif
                END IF
              END IF
            ELSE
              IF (Master) THEN
                WRITE (stdout,80) string, TRIM(Vname(1,idTsur(itrc))),  &
     &                            TRIM(ncname)
              END IF
              exit_flag=4
              IF (FoundError(exit_flag, PIO_noerr,                      &
     &                       __LINE__, MyFile)) THEN
                RETURN
              END IF
            END IF
          END IF
        END DO
!
!  Read in surface U-momentum stress.
!
        IF (get_var(idUsms)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idUsms)),   &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (KIND(FORCES(ng)%sustr).eq.8) THEN
              my_pioVar%dkind=PIO_double
              ioDesc => ioDesc_dp_r2dvar(ng)
            ELSE
              my_pioVar%dkind=PIO_real
              ioDesc => ioDesc_sp_r2dvar(ng)
            END IF
            status=nf_fread2d(ng, IDmod, ncname, pioFile,               &
     &                        Vname(1,idUsms), my_pioVar,               &
     &                        InpRec, ioDesc, Vsize,                    &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
#   ifdef MASKING
     &                        GRID(ng) % rmask,                         &
#   endif
#   ifdef CHECKSUM
     &                        FORCES(ng) % sustr,                       &
     &                        checksum = Fhash)
#   else
     &                        FORCES(ng) % sustr)
#   endif
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idUsms)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#   ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idUsms)), Fmin, Fmax,    &
     &                            Fhash
#   else
                WRITE (stdout,70) TRIM(Vname(2,idUsms)), Fmin, Fmax
#   endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idUsms)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in surface V-momentum stress.
!
        IF (get_var(idVsms)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idVsms)),   &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (KIND(FORCES(ng)%svstr).eq.8) THEN
              my_pioVar%dkind=PIO_double
              ioDesc => ioDesc_dp_r2dvar(ng)
            ELSE
              my_pioVar%dkind=PIO_real
              ioDesc => ioDesc_sp_r2dvar(ng)
            END IF
            status=nf_fread2d(ng, IDmod, ncname, pioFile,               &
     &                        Vname(1,idVsms), my_pioVar,               &
     &                        InpRec, ioDesc, Vsize,                    &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
#   ifdef MASKING
     &                        GRID(ng) % rmask,                         &
#   endif
#   ifdef CHECKSUM
     &                        FORCES(ng) % svstr,                       &
     &                        checksum = Fhash)
#   else
     &                        FORCES(ng) % svstr)
#   endif
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idVsms)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#   ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idVsms)), Fmin, Fmax,    &
     &                            Fhash
#   else
                WRITE (stdout,70) TRIM(Vname(2,idVsms)), Fmin, Fmax
#   endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idVsms)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
#  endif
# endif
# ifdef ICESHELF
#  ifdef ICESHELF_MORPH
!
!  Read in time-varying ice draft.
!
        IF (get_var(idIsDrft)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIsDrft)), &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (KIND(GRID(ng)%zice).eq.8) THEN
              my_pioVar%dkind=PIO_double
              ioDesc => ioDesc_dp_r2dvar(ng)
            ELSE
              my_pioVar%dkind=PIO_real
              ioDesc => ioDesc_sp_r2dvar(ng)
            END IF
            status=nf_fread2d(ng, IDmod, ncname, pioFile,               &
     &                        Vname(1,idIsDrft), my_pioVar,             &
     &                        InpRec, ioDesc, Vsize,                    &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
#   ifdef MASKING
     &                        GRID(ng) % rmask,                         &
#   endif
#   ifdef CHECKSUM
     &                        GRID(ng) % zice,                          &
     &                        checksum = Fhash)
#   else
     &                        GRID(ng) % zice)
#   endif
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIsDrft)),      &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#   ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIsDrft)), Fmin, Fmax,  &
     &                            Fhash
#   else
                WRITE (stdout,70) TRIM(Vname(2,idIsDrft)), Fmin, Fmax
#   endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIsDrft)),        &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
#  endif
#  ifdef ICESHELF_TRACER
!
!  Read in iceshelf volume (m).
!
        IF (get_var(idIsVol)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idIsVol)),  &
     &                        vindex)
          IF (foundit) THEN
            my_pioVar%vd=var_desc(vindex)
            my_pioVar%gtype=r2dvar
            IF (KIND(ICESHELFVAR(ng)%isvol).eq.8) THEN
              my_pioVar%dkind=PIO_double
              ioDesc => ioDesc_dp_r2dvar(ng)
            ELSE
              my_pioVar%dkind=PIO_real
              ioDesc => ioDesc_sp_r2dvar(ng)
            END IF
            status=nf_fread2d(ng, IDmod, ncname, pioFile,               &
     &                        Vname(1,idIsVol), my_pioVar,              &
     &                        InpRec, ioDesc, Vsize,                    &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
#   ifdef MASKING
     &                        GRID(ng) % rmask,                         &
#   endif
#   ifdef CHECKSUM
     &                        ICESHELFVAR(ng) % isvol,                  &
     &                        checksum = Fhash)
#   else
     &                        ICESHELFVAR(ng) % isvol)
#   endif
            IF (FoundError(status, PIO_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idIsVol)),       &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
#   ifdef CHECKSUM
                WRITE (stdout,70) TRIM(Vname(2,idIsVol)), Fmin, Fmax,   &
     &                            Fhash
#   else
                WRITE (stdout,70) TRIM(Vname(2,idIsVol)), Fmin, Fmax
#   endif

              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idIsVol)),         &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, PIO_noerr,                        &
     &                     __LINE__, MyFile)) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in integrated tracer (m Tunits).
!
        DO itrc=NAT+1, NT(ng)
          IF (get_var(idIsTrc(itrc))) THEN
            foundit=find_string(var_name, n_var,                        &
     &                          TRIM(Vname(1,idIsTrc(itrc))), vindex)
            IF (foundit) THEN
              scale=1.0_dp
              my_pioVar%vd=var_desc(vindex)
              my_pioVar%gtype=r2dvar
              IF (KIND(ICESHELFVAR(ng)%istrc).eq.8) THEN
                my_pioVar%dkind=PIO_double
                ioDesc => ioDesc_dp_r2dvar(ng)
              ELSE
                my_pioVar%dkind=PIO_real
                ioDesc => ioDesc_sp_r2dvar(ng)
              END IF
!
              status=nf_fread2d(ng, IDmod, ncname, pioFile,             &
     &                          Vname(1,idIsTrc(itrc)), my_pioVar,      &
     &                          InpRec, ioDesc, Vsize,                  &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          scale, Fmin, Fmax,                      &
#   ifdef MASKING
     &                          GRID(ng) % rmask,                       &
#   endif
#   ifdef CHECKSUM
     &                          ICESHELFVAR(ng)% istrc(:,:,itrc),       &
     &                          checksum = Fhash)
#   else
     &                          ICESHELFVAR(ng)% istrc(:,:,itrc))
#   endif
              IF (FoundError(status, PIO_noerr, __LINE__, MyFile))      &
     &          THEN
                IF (Master) THEN
                  WRITE (stdout,60) string,                             &
     &                              TRIM(Vname(1,idIsTrc(itrc))),       &
     &                              InpRec, TRIM(ncname)
                END IF
                exit_flag=2
                ioerror=status
                RETURN
              ELSE
                IF (Master) THEN
#   ifdef CHECKSUM
                  WRITE (stdout,70) TRIM(Vname(2,idIsTrc(itrc)))//      &
     &                              ', saved stflux', Fmin, Fmax,       &
     &                              Fhash
#   else
                  WRITE (stdout,70) TRIM(Vname(2,idIsTrc(itrc)))//      &
     &                              ', saved stflux', Fmin, Fmax
#   endif
                END IF
              END IF
            ELSE
              IF (Master) THEN
                WRITE (stdout,80) string, TRIM(Vname(1,idIsTrc(itrc))), &
     &                            TRIM(ncname)
              END IF
              exit_flag=4
              IF (FoundError(exit_flag, PIO_noerr,                      &
     &                       __LINE__, MyFile)) THEN
                RETURN
              END IF
            END IF
          END IF
        END DO
#  endif
# endif
