module def_type_pb
  use def_const_pb
  implicit none
  public
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type record_
    real(8), pointer :: obs(:)
    real(8), pointer :: oer(:)
    real(8), pointer :: qmk(:)
    real(8), pointer :: pcd(:)
    real(8), pointer :: rcd(:)
  end type

  type subset_
    character(CLEN_SID) :: sid
    real(8) :: lon, lat
    real(8) :: tdiff
    real(8) :: elv
    integer :: typ
    integer :: said
    integer :: t29
    real(8), pointer :: hdr(:)  !(nhdr)
    integer :: nrec
    type(record_), pointer :: rec(:)
  end type

  type message_
    character(CLEN_MNNC) :: typ
    integer :: nsub
    type(subset_), pointer :: sub(:)
  end type

  type prepbufr_
    character(CLEN_MNNC) :: msgtyp
    integer :: nhdr, nobs, noer, nqmk, npcd, nrcd
    integer :: nmsg
    type(message_), pointer :: msg(:)
  end type
  !-------------------------------------------------------------
end module def_type_pb
