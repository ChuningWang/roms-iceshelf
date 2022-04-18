# ifdef ICE_MODEL
!
!-----------------------------------------------------------------------
!  Ice model variables.
!-----------------------------------------------------------------------
!
          real(r8), pointer :: avgui(:,:)
          real(r8), pointer :: avgvi(:,:)
          real(r8), pointer :: avguie(:,:)
          real(r8), pointer :: avgvin(:,:)
          real(r8), pointer :: avgai(:,:)
          real(r8), pointer :: avghi(:,:)
          real(r8), pointer :: avghsn(:,:)
          real(r8), pointer :: avgageice(:,:)
          real(r8), pointer :: avgtis(:,:)
          real(r8), pointer :: avgti(:,:)
          real(r8), pointer :: avgt2(:,:)
          real(r8), pointer :: avgsig11(:,:)
          real(r8), pointer :: avgsig12(:,:)
          real(r8), pointer :: avgsig22(:,:)
          real(r8), pointer :: avgutau_iw(:,:)
          real(r8), pointer :: avgchu_iw(:,:)
          real(r8), pointer :: avgt0mk(:,:)
          real(r8), pointer :: avgs0mk(:,:)
          real(r8), pointer :: avgiomflx(:,:)
          real(r8), pointer :: avgwfr(:,:)
          real(r8), pointer :: avgwai(:,:)
          real(r8), pointer :: avgwao(:,:)
          real(r8), pointer :: avgwio(:,:)
          real(r8), pointer :: avgwro(:,:)
          real(r8), pointer :: avgwdiv(:,:)
#  ifdef ICE_DIAGS
#   ifdef ICE_ALBEDO
          real(r8), pointer :: avgalbo(:,:)
          real(r8), pointer :: avgalbi(:,:)
#    ifdef ICE_ALB_CSIM
          real(r8), pointer :: avgfrswp(:,:)
#    endif
#   endif
          real(r8), pointer :: avgrfaci(:,:)
          real(r8), pointer :: avgqswl(:,:)
          real(r8), pointer :: avgqlwl(:,:)
          real(r8), pointer :: avgqlai(:,:)
          real(r8), pointer :: avgqsei(:,:)
          real(r8), pointer :: avgqlwa(:,:)
          real(r8), pointer :: avgqswa(:,:)
          real(r8), pointer :: avgqswi(:,:)
          real(r8), pointer :: avgqswo(:,:)
          real(r8), pointer :: avgsio(:,:)
          real(r8), pointer :: avgsao(:,:)
          real(r8), pointer :: avgqao(:,:)
          real(r8), pointer :: avgqai(:,:)
          real(r8), pointer :: avgqio(:,:)
          real(r8), pointer :: avgqi2(:,:)
          real(r8), pointer :: avgwsni(:,:)
#  endif
# endif
# ifdef ICESHELF
          real(r8), pointer :: avgm(:,:)
#  ifdef ICESHELF_3EQN_VBC
          real(r8), pointer :: avgTstar(:,:)
          real(r8), pointer :: avgUstar(:,:)
          real(r8), pointer :: avgTb(:,:)
          real(r8), pointer :: avgSb(:,:)
#  endif
#  ifdef ICESHELF_MORPH
          real(r8), pointer :: avgzice(:,:)
#  endif
#  ifdef ICESHELF_SOF
          real(r8), pointer :: avgSOFs(:,:)
          real(r8), pointer :: avgSOFh(:,:)
#  endif
# endif
