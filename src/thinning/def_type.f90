module def_type
  use lib_const
  use lib_io
  use def_const_pb
  implicit none
  public
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type time_
    integer(8) :: datetime
    integer :: tout_bhd, tout_ahd
  end type
  !-------------------------------------------------------------
  type order_
    character(CLEN_MNNC) :: msgtyp
    character(CLEN_VAR)  :: method
    integer              :: nvar
    integer, allocatable :: vidx(:)
    logical              :: is_surface
    real(8) :: ticld_bhd, ticld_ahd
    integer :: nmax
    real(8) :: rho_h, rho_v
    real(8) :: dist_h_thresh, dist_v_thresh
    real(8) :: dnst_thresh
    logical :: use_all_q0
    real(8), pointer :: wgt_qlt(:)
    integer :: code_same_id
    integer :: nmax_same_sid
    real(8) :: wgt_same_sid
    integer :: nmax_same_said
    real(8) :: wgt_same_said
    integer :: opt_Tv
    integer :: opt_Qerr
    integer :: opt_sfc
    real(8) :: obserr_scale
    real(8) :: lapse_rate
  end type
  !-------------------------------------------------------------
  type lay_local_
    integer :: ilay_all
    integer :: irec_local
  end type

  type rec_local_
    integer :: imsg, isub, irec
    integer :: irec_local
    integer :: irec_all  !for rcdnst mode. To be checked
    integer :: irec_valid
    integer :: ilay_all
    real(8) :: dist_h, dist_v
    real(8) :: wgt_dist
    real(8) :: wgt
  end type

  type rec_
    real(8), pointer :: obs(:)
    real(8), pointer :: oer(:)
    real(8), pointer :: qmk(:)
    real(8) :: logp
    integer, pointer :: iqmk(:)  ! int(qmk) and miss_value -> -1
    integer :: nlay_local
    type(lay_local_) , pointer :: lay_local(:)
    integer :: irec_valid
    logical :: is_valid
    logical :: is_selected
    integer :: irec_selected
    integer :: ilay_all_belong
    integer :: ilay_all_nearest
    real(8) :: wgt_dist_max
    integer :: idx_same_sid
    integer :: idx_same_said
  end type

  type sub_local_
    integer :: imsg, isub
    integer :: irecs, irece
    real(8) :: dist_h
    real(8), pointer :: dist_v(:)
    real(8), pointer :: weight(:)
  end type

  type sub_
    character(CLEN_SID) :: sid
    real(8) :: lon, lat
    real(8) :: tdiff
    real(8) :: elv
    integer :: typ
    integer :: said
    integer :: t29
    real(8), pointer :: hdr(:)  !(nhdr)
    integer :: report_type
    integer :: nrec
    type(rec_), pointer :: rec(:)
    integer :: ilon, ilat
    integer :: time
  end type

  type msg_
    character(CLEN_MNNC) :: typ
    integer :: nsub
    type(sub_), pointer :: sub(:)
  end type

  type pb_
    character(CLEN_MNNC) :: msgtyp
    logical :: is_surface
    integer :: nhdr, nobs, noer, nqmk
    integer :: nobs_ext
    integer :: nmsg
    type(msg_), pointer :: msg(:)
    integer :: nrec_all
    integer :: nrec_valid
    integer :: nrec_selected
  end type

  type grid_lay_
    integer :: nrec_local
    type(rec_local_), pointer :: rec_local(:)
    integer :: ilay_all
  end type

  type grid_
    integer :: nsub
    integer, pointer :: imsg(:)
    integer, pointer :: isub(:)
    integer :: ngrid_around
    integer, pointer :: ilon_around(:), ilat_around(:)
    type(grid_lay_), pointer :: lay(:)  !(nlev)
  end type

  type lay_all_
    integer :: ilon, ilat, ilev
    integer :: nrec_local
    type(rec_local_), pointer :: rec_local(:)
    real(8) :: dnst_all
    real(8) :: dnst_used
  end type

  type rec_valid_
    integer :: imsg, isub, irec
    integer :: nlay_local
    integer, pointer :: ilay_all(:)   !(nlay_local)
    integer, pointer :: irec_local(:) !(nlay_local)
    integer :: ilay_all_belong
  end type
  !-------------------------------------------------------------
  type lst_idx_same_id_
    integer :: nsub
    integer, pointer :: imsg(:)
    integer, pointer :: isub(:)
  end type
  !-------------------------------------------------------------
  type model_
    integer :: nlon, nlat
    real(8) :: west, east, south, north
    real(8) :: lonsize, latsize
    real(8), pointer :: lonb(:), latb(:)
    logical :: is_south_to_north
    type(file_) :: f_elv
    real(8) :: elv_miss
  end type
  !-------------------------------------------------------------
  type monit_
    logical :: monit_proc
    logical :: monit_itr_sct_rec
    logical :: monit_itr_add_rec
    logical :: save_modif
    logical :: save_monit
    character(CLEN_PATH) :: dir
    character(16) :: opt_log_proc
  end type
  !-------------------------------------------------------------
end module def_type
