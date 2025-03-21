module mod_pb
  use lib_const
  use lib_log
  use lib_math
  use lib_io
  use def_const_pb
  use def_type_pb
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: dump_prepbufr
  public :: read_prepbufr_dumped
  public :: free_prepbufr

  public :: get_num_key_all
  public :: get_nvar

  public :: idx_to_mnc_obs
  public :: mnc_to_idx_obs
  public :: idx_to_typ_msgtyp
  public :: typ_to_idx_msgtyp

  public :: set_mod_pb__opt_log_proc

  public :: set_dgt_nmsg
  public :: set_dgt_nsub
  public :: set_dgt_nrec

  public :: dgt_nmsg
  public :: dgt_nsub
  public :: dgt_nrec
  !-------------------------------------------------------------
  ! Interfaces of External Procedures from NCEPLIBS
  !-------------------------------------------------------------
  interface 
    subroutine datelen(len)  ! s013vals.F90
      implicit none
      integer, intent(in) :: len
    end subroutine 

    subroutine openbf(lunit,io,lundx)  ! openclosebf.F90
      implicit none
      integer, intent(in) :: lunit, lundx
      character(*), intent(in) :: io
    end subroutine

    subroutine closbf(lunit)  ! openclosebf.F90
      implicit none
      integer, intent(in) :: lunit
    end subroutine closbf

    subroutine rewnbf(lunit,isr)  ! openclosebf.F90
      implicit none
      integer, intent(in) :: lunit, isr
    end subroutine

    subroutine ufbint(lunin,usr,i1,i2,info,str)  ! readwriteval.F90
      implicit none
      character(*), intent(in) :: str
      real(8), intent(inout) :: usr(i1,i2)
      integer, intent(in) :: lunin, i1, i2
      integer, intent(out) :: info
    end subroutine

    recursive subroutine dxdump(lunit,ldxot)  ! dumpdata.F90
      implicit none
      integer, intent(in) :: lunit, ldxot
    end subroutine
  end interface
  !-------------------------------------------------------------
  ! Private Module Variables
  !-------------------------------------------------------------
  integer :: nhdr, nobs, noer, nqmk, npcd, nrcd

  integer :: d_nmsg = 0
  integer :: d_nsub = 0
  integer :: d_nrec = 0

  character(16) :: opt_log_proc = ''
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine dump_prepbufr(&
    fin_pb, fout_table, fout_data_template, &
    replace_old_file)
  implicit none
  character(*), intent(in) :: fin_pb
  character(*), intent(in) :: fout_table
  character(*), intent(in) :: fout_data_template
  logical     , intent(in) :: replace_old_file

  integer :: nmsg, imsg
  integer :: nsub, isub
  integer :: nlev, ilev
  integer :: idate
  integer :: imsgtyp
  character(CLEN_MNNC) :: msgtyp
  character(CLEN_SID) :: sid
  real(8), allocatable :: hdr(:)
  real(8), allocatable :: obs(:,:), oer(:,:), qmk(:,:)
  real(8), allocatable :: pcd(:,:), rcd(:,:)
  integer :: lst_nmsg(NMSGTYP)
  logical :: to_be_updated(NMSGTYP)
  character(CLEN_VAR) :: mark
  integer :: ic

  integer :: info

  character(64) :: wfmt_hdr, wfmt_obs, wfmt_oer, wfmt_qmk, &
                   wfmt_pcd, wfmt_rcd
  integer :: un_pb, un_data(NMSGTYP), un_table
  character(CLEN_PATH) :: fout_data

  ! Functions from NCEPLIBS-bufr
  integer :: ireadmg
  integer :: nmsub
  integer :: ireadsb

  integer :: access

  call echo(code%bgn, 'dump_prepbufr', opt_log_proc)
  !-------------------------------------------------------------
  ! Make output directories
  !-------------------------------------------------------------
  call mkdir(dirname(fout_table))
  call mkdir(dirname(fout_data_template))
  !-------------------------------------------------------------
  ! Open PREPBUFR file
  !-------------------------------------------------------------
  un_pb = unit_number()
  open(un_pb, file=fin_pb, form='unformatted', status='old')

  call openbf(un_pb, 'IN', un_pb)
  !-------------------------------------------------------------
  ! Dump table
  !-------------------------------------------------------------
  un_table = unit_number()
  open(un_table, file=fout_table, status='replace')
  call dxdump(un_pb, un_table)
  close(un_table)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call datelen(10)
  !-------------------------------------------------------------
  ! Get num. of messages
  !-------------------------------------------------------------
  lst_nmsg(:) = 0
  do while( ireadmg(un_pb, msgtyp, idate) == 0 )
    imsgtyp = typ_to_idx_msgtyp(msgtyp)
    call add(lst_nmsg(imsgtyp))
  enddo
  nmsg = sum(lst_nmsg(:))

  call rewnbf(un_pb, 0)
  !-------------------------------------------------------------
  ! Dump data
  !-------------------------------------------------------------
  if( nhdr == 0 ) call get_num_key_all(nhdr, nobs, noer, nqmk, npcd, nrcd)

  write(wfmt_hdr,"(a,i0,a,i0,a)") &
        "(1x,a,1x,i8,1x,a,1x,i4,1x,a,1x,a",CLEN_SID,",1x,a,",nhdr-1,"(1x,f14.1))"
  write(wfmt_obs,"(a,i0,a)") "(1x,i6,1x,'obs',",nobs,"(1x,f14.1))"
  write(wfmt_oer,"(a,i0,a)") "(1x,i6,1x,'oer',",noer,"(1x,f14.1))"
  write(wfmt_qmk,"(a,i0,a)") "(1x,i6,1x,'qmk',",nqmk,"(1x,f14.1))"
  write(wfmt_pcd,"(a,i0,a)") "(1x,i6,1x,'pcd',",npcd,"(1x,f14.1))"
  write(wfmt_rcd,"(a,i0,a)") "(1x,i6,1x,'rcd',",nrcd,"(1x,f14.1))"

  allocate(hdr(nhdr))
  allocate(obs(nobs,MXLV))
  allocate(oer(noer,MXLV))
  allocate(qmk(nqmk,MXLV))
  allocate(pcd(nqmk,MXLV))
  allocate(rcd(nqmk,MXLV))

  to_be_updated(:) = .true.
  do imsgtyp = 1, NMSGTYP
    msgtyp = idx_to_typ_msgtyp(imsgtyp)
    fout_data = trim(fout_data_template)//'.'//trim(msgtyp)
    un_data(imsgtyp) = unit_number()

    if( .not. replace_old_file )then
      ! Check if old file exists and it is perfect
      if( access(fout_data,' ') == 0 )then
        open(un_data(imsgtyp), file=fout_data, status='old', position='append')
        backspace(un_data(imsgtyp))
        read(un_data(imsgtyp),*) mark
        close(un_data(imsgtyp))
        if( mark == MARK_EOF )then
          to_be_updated(imsgtyp) = .false.
        endif
      endif
    endif

    if( to_be_updated(imsgtyp) )then
      call edbg('Writing: '//str(fout_data))
      open(un_data(imsgtyp), file=fout_data, status='replace')
      write(un_data(imsgtyp),"(1x,a,1x,i8)") 'nmsg=', lst_nmsg(imsgtyp)
    else
      call edbg('File exists: '//str(fout_data))
    endif
  enddo

  lst_nmsg(:) = 0
  do imsg = 1, nmsg
    selectcase( ireadmg(un_pb, msgtyp, idate) )
    case( 0 )
      continue
    case( -1 )
      call eerr('Reached end of file while reading message.')
    case default
      call eerr('An error occured while reading message.')
    endselect

    imsgtyp = typ_to_idx_msgtyp(msgtyp)
    if( .not. to_be_updated(imsgtyp) ) cycle

    lst_nmsg(imsgtyp) = lst_nmsg(imsgtyp) + 1
    nsub = nmsub(un_pb)

    write(un_data(imsgtyp),*)
    write(un_data(imsgtyp),"(1x,a,1x,i8,1x,1x,a,1x,i6)") &
          'imsg=', lst_nmsg(imsgtyp), 'nsub=', nsub

    do isub = 1, nsub
      selectcase( ireadsb(un_pb) )
      case( 0 )
        continue
      case( -1 )
        call eerr('Reached end of file while reading subset.')
      case default
        call eerr('An error occured while reading subset')
      endselect

      call ufbint(un_pb, hdr, nhdr, 1, info, hdstr)
      call ufbint(un_pb, obs, nobs, MXLV, nlev, obstr)
      call ufbint(un_pb, oer, noer, MXLV, nlev, oestr)
      call ufbint(un_pb, qmk, nqmk, MXLV, nlev, qmstr)
      call ufbint(un_pb, pcd, npcd, MXLV, nlev, pcstr)
      call ufbint(un_pb, rcd, nrcd, MXLV, nlev, rcstr)

      if( hdr(1) == PREPBUFR_MISS )then
        sid = PREPBUFR_SID_MISS
      else
        sid = transfer(hdr(1),sid)
        do while( index(sid,'/') /= 0 )
          ic = index(sid,'/')
          sid(ic:ic) = '-'
        enddo
        do while( index(trim(sid),' ') /= 0 )
          ic = index(sid,' ')
          sid(ic:ic) = '_'
        enddo
      endif

      write(un_data(imsgtyp),wfmt_hdr) &
            'isub=', isub, 'nlev=', nlev, 'SID=', sid, &
            'HDR(2:)=', hdr(2:nhdr)
      do ilev = 1, nlev
        write(un_data(imsgtyp),wfmt_obs) ilev, obs(:,ilev)
        write(un_data(imsgtyp),wfmt_oer) ilev, oer(:,ilev)
        write(un_data(imsgtyp),wfmt_qmk) ilev, qmk(:,ilev)
        write(un_data(imsgtyp),wfmt_pcd) ilev, pcd(:,ilev)
        write(un_data(imsgtyp),wfmt_rcd) ilev, rcd(:,ilev)
      enddo
    enddo  ! isub/
  enddo  ! while ireadmg() == 0/

  deallocate(hdr)
  deallocate(obs)
  deallocate(oer)
  deallocate(qmk)
  deallocate(pcd)
  deallocate(rcd)

  do imsgtyp = 1, NMSGTYP
    if( .not. to_be_updated(imsgtyp) ) cycle

    write(un_data(imsgtyp),*)
    write(un_data(imsgtyp),*) MARK_EOF
    close(un_data(imsgtyp))
  enddo

  call closbf(un_pb)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine dump_prepbufr
!===============================================================
!
!===============================================================
subroutine read_prepbufr_dumped(fin_template, msgtyp, pb)
  implicit none
  character(*)   , intent(in)  :: fin_template
  character(*)   , intent(in)  :: msgtyp
  type(prepbufr_), intent(out) :: pb

  integer :: imsgtyp

  call echo(code%bgn, 'read_prepbufr_dumped', opt_log_proc)
  !-------------------------------------------------------------
  pb%msgtyp = msgtyp

  pb%nhdr = get_num_key(hdstr)
  pb%nobs = get_num_key(obstr)
  pb%noer = get_num_key(oestr)
  pb%nqmk = get_num_key(qmstr)
  pb%npcd = get_num_key(pcstr)
  pb%nrcd = get_num_key(rcstr)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( msgtyp == 'all' )then
    pb%nmsg = 0
    do imsgtyp = 1, NMSGTYP
      call add(pb%nmsg, get_nmsg_dumped(&
                 fin_template, idx_to_typ_msgtyp(imsgtyp)))
    enddo

    allocate(pb%msg(pb%nmsg))

    pb%nmsg = 0
    do imsgtyp = 1, NMSGTYP
      call read_prepbufr_dumped_msgtyp(&
             fin_template, idx_to_typ_msgtyp(imsgtyp), pb, 'append')
    enddo
  else
    pb%nmsg = 0
    call read_prepbufr_dumped_msgtyp(&
           fin_template, msgtyp, pb, 'new')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_prepbufr_dumped
!===============================================================
!
!===============================================================
subroutine read_prepbufr_dumped_msgtyp(&
    fin_template, msgtyp, pb, mode)
  implicit none
  character(*), intent(in) :: fin_template
  character(*), intent(in) :: msgtyp
  type(prepbufr_), intent(inout) :: pb
  character(*), intent(in) :: mode

  type(message_), pointer :: msg
  type(subset_) , pointer :: sub
  type(record_) , pointer :: rec
  integer :: nmsg, imsg
  integer :: nsub, isub
  integer :: irec

  integer :: i_
  character :: c_

  character(CLEN_PATH) :: fin
  integer :: un
  logical :: ex
  integer :: ios

  call echo(code%bgn, 'read_prepbufr_dumped_msgtyp', opt_log_proc)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fin = get_filename_msgtyp(fin_template, msgtyp)

  inquire(file=fin, exist=ex)
  if( .not. ex )then
    call edbg('File not found: '//str(fin))
    call echo(code%ret)
    return
  endif

  call edbg('Reading '//trim(fin))
  un = unit_number()
  open(un, file=fin, status='old')
  !-------------------------------------------------------------
  ! Count num. of msg. of msgtyp
  !-------------------------------------------------------------
  read(un,*) c_, nmsg

  selectcase( mode )
  case( MODE_NEW )
    allocate(pb%msg(nmsg))
  case( MODE_APPEND )
    continue
  case default
    call eerr('Invalid value in $mode: '//str(mode))
  endselect
  !-------------------------------------------------------------
  ! Read data
  !-------------------------------------------------------------
  do imsg = 1, nmsg
    read(un,*)
    read(un,*) c_, i_, c_, nsub

    call add(pb%nmsg)
    msg => pb%msg(pb%nmsg)

    msg%typ = msgtyp

    msg%nsub = nsub
    allocate(msg%sub(nsub))

    do isub = 1, nsub
      sub => msg%sub(isub)
      allocate(sub%hdr(pb%nhdr))

      read(un,*,iostat=ios) c_, i_, c_, sub%nrec, c_, sub%sid, c_, sub%hdr(2:)
      if( ios /= 0 )then
        call eerr('Reading error @ imsg='//str(imsg)//', isub='//str(isub))
      endif
      sub%lon   = sub%hdr(2)
      sub%lat   = sub%hdr(3)
      sub%tdiff = sub%hdr(4)
      sub%typ   = val_to_int(sub%hdr(5))
      sub%elv   = sub%hdr(6)
      sub%said  = val_to_int(sub%hdr(7))
      sub%t29   = val_to_int(sub%hdr(8))

      allocate(sub%rec(sub%nrec))

      do irec = 1, sub%nrec
        rec => sub%rec(irec)
        allocate(rec%obs(pb%nobs))
        allocate(rec%oer(pb%noer))
        allocate(rec%qmk(pb%nqmk))
        allocate(rec%pcd(pb%npcd))
        allocate(rec%rcd(pb%nrcd))

        read(un,*) i_, c_, rec%obs(:)
        read(un,*) i_, c_, rec%oer(:)
        read(un,*) i_, c_, rec%qmk(:)
        read(un,*) i_, c_, rec%pcd(:)
        read(un,*) i_, c_, rec%rcd(:)
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/

  close(un)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
integer function val_to_int(val) result(ret)
  implicit none
  real(8), intent(in) :: val

  if( val == PREPBUFR_MISS )then
    ret = PREPBUFR_MISS_INT
  else
    ret = int(val,4)
  endif
end function val_to_int
!---------------------------------------------------------------
end subroutine read_prepbufr_dumped_msgtyp
!===============================================================
!
!===============================================================
subroutine free_prepbufr(pb)
  implicit none
  type(prepbufr_), intent(inout) :: pb

  pb%msgtyp = ''
  if( pb%nmsg > 0 )then
    pb%nmsg = 0
    deallocate(pb%msg)
  endif
end subroutine free_prepbufr
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine get_num_key_all(nhdr, nobs, noer, nqmk, npcd, nrcd)
  implicit none
  integer, intent(out) :: nhdr, nobs, noer, nqmk, npcd, nrcd

  nhdr = get_num_key(hdstr)
  nobs = get_num_key(obstr)
  noer = get_num_key(oestr)
  nqmk = get_num_key(qmstr)
  npcd = get_num_key(pcstr)
  nrcd = get_num_key(rcstr)
end subroutine get_num_key_all
!===============================================================
!
!===============================================================
integer function get_num_key(str) result(n)
  implicit none
  character(*), intent(in) :: str

  integer :: i
  logical :: is_prev_space

  is_prev_space = .false.
  n = 1
  do i = 1, len_trim(str)
    if( str(i:i) == ' ' )then
      if( .not. is_prev_space )then
        is_prev_space = .true.
        n = n + 1
      endif
    else
      is_prev_space = .false.
    endif
  enddo
end function get_num_key
!===============================================================
!
!===============================================================
integer function get_nvar(nobs) result(nvar)
  implicit none
  integer, intent(in) :: nobs

  nvar = nobs - 1
end function get_nvar
!===============================================================
!
!===============================================================
integer function get_nmsg_dumped(f_template, msgtyp) result(nmsg)
  implicit none
  character(*), intent(in) :: f_template
  character(*), intent(in) :: msgtyp

  character :: c_

  character(CLEN_PATH) :: f
  integer :: un
  logical :: ex

  call echo(code%bgn, 'get_nmsg_dumped', '-p')
  !-------------------------------------------------------------
  f = get_filename_msgtyp(f_template, msgtyp)

  inquire(file=f, exist=ex)
  if( ex )then
    un = unit_number()
    open(un, file=f, status='old')
    read(un,*) c_, nmsg
    close(un)
  else
    nmsg = 0
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function get_nmsg_dumped
!===============================================================
!
!===============================================================
character(CLEN_PATH) function get_filename_msgtyp(&
    f_template, msgtyp) result(f)
  implicit none
  character(*), intent(in) :: f_template
  character(*), intent(in) :: msgtyp

  f = trim(f_template)//'.'//trim(msgtyp)
end function get_filename_msgtyp
!===============================================================
!
!===============================================================
character(CLEN_MNNC) function idx_to_typ_msgtyp(idx) result(typ)
  implicit none
  integer, intent(in) :: idx

  call echo(code%bgn, 'idx_to_typ_msgtyp', '-p')
  !-------------------------------------------------------------
  selectcase( idx )
  case(  1 ); typ = MSGTYP_ADPUPA
  case(  2 ); typ = MSGTYP_AIRCAR
  case(  3 ); typ = MSGTYP_AIRCFT
  case(  4 ); typ = MSGTYP_SATWND
  case(  5 ); typ = MSGTYP_PROFLR
  case(  6 ); typ = MSGTYP_VADWND
  case(  7 ); typ = MSGTYP_SATEMP
  case(  8 ); typ = MSGTYP_ADPSFC
  case(  9 ); typ = MSGTYP_SFCSHP
  case( 10 ); typ = MSGTYP_SFCBOG
  case( 11 ); typ = MSGTYP_SPSSMI
  case( 12 ); typ = MSGTYP_SYNDAT
  case( 13 ); typ = MSGTYP_ERS1DA
  case( 14 ); typ = MSGTYP_GOESND
  case( 15 ); typ = MSGTYP_QKSWND
  case( 16 ); typ = MSGTYP_MSONET
  case( 17 ); typ = MSGTYP_GPSIPW
  case( 18 ); typ = MSGTYP_RASSDA
  case( 19 ); typ = MSGTYP_WDSATR
  case( 20 ); typ = MSGTYP_ASCATW
  case default
    call eerr('Invalid value in $idx: '//str(idx))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function idx_to_typ_msgtyp
!===============================================================
!
!===============================================================
integer function typ_to_idx_msgtyp(typ) result(idx)
  implicit none
  character(*), intent(in) :: typ

  call echo(code%bgn, 'typ_to_idx_msgtyp', '-p')
  !-------------------------------------------------------------
  selectcase( typ )
  case( MSGTYP_ADPUPA ); idx =  1
  case( MSGTYP_AIRCAR ); idx =  2
  case( MSGTYP_AIRCFT ); idx =  3
  case( MSGTYP_SATWND ); idx =  4
  case( MSGTYP_PROFLR ); idx =  5
  case( MSGTYP_VADWND ); idx =  6
  case( MSGTYP_SATEMP ); idx =  7
  case( MSGTYP_ADPSFC ); idx =  8
  case( MSGTYP_SFCSHP ); idx =  9
  case( MSGTYP_SFCBOG ); idx = 10
  case( MSGTYP_SPSSMI ); idx = 11
  case( MSGTYP_SYNDAT ); idx = 12
  case( MSGTYP_ERS1DA ); idx = 13
  case( MSGTYP_GOESND ); idx = 14
  case( MSGTYP_QKSWND ); idx = 15
  case( MSGTYP_MSONET ); idx = 16
  case( MSGTYP_GPSIPW ); idx = 17
  case( MSGTYP_RASSDA ); idx = 18
  case( MSGTYP_WDSATR ); idx = 19
  case( MSGTYP_ASCATW ); idx = 20
  case default
    call eerr('Invalid value in $typ: '//str(typ))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function typ_to_idx_msgtyp
!===============================================================
!
!===============================================================
character(CLEN_MNNC) function idx_to_mnc_obs(vidx) result(vmnc)
  implicit none
  integer, intent(in) :: vidx

  character(CLEN_MNNC), allocatable :: lst_obstr(:)

  call echo(code%bgn, 'idx_to_mnc_obs', '-p')
  !-------------------------------------------------------------
  if( nhdr == 0 ) call get_num_key_all(nhdr, nobs, noer, nqmk, npcd, nrcd)

  allocate(lst_obstr(nobs))
  read(obstr,*) lst_obstr(:)
  vmnc = lst_obstr(vidx)
  deallocate(lst_obstr)
  !-------------------------------------------------------------
  call echo(code%ret)
end function idx_to_mnc_obs
!===============================================================
!
!===============================================================
integer function mnc_to_idx_obs(vmnc, swap_p_ps, allow_not_found) result(vidx)
  implicit none
  character(*), intent(in) :: vmnc
  logical     , intent(in), optional :: swap_p_ps
  logical     , intent(in), optional :: allow_not_found

  logical :: swap_p_ps_
  logical :: allow_not_found_
  character(CLEN_MNNC) :: vmnc_

  character(CLEN_MNNC), allocatable :: lst_obstr(:)
  integer :: iobs

  call echo(code%bgn, 'mnc_to_idx_obs', '-p')
  !-------------------------------------------------------------
  swap_p_ps_ = .false.
  allow_not_found_ = .false.
  if( present(swap_p_ps) ) swap_p_ps_ = swap_p_ps
  if( present(allow_not_found) ) allow_not_found_ = allow_not_found

  if( nhdr == 0 ) call get_num_key_all(nhdr, nobs, noer, nqmk, npcd, nrcd)

  if( swap_p_ps_ )then
    if( vmnc == VMNC_P_OBS )then
      vmnc_ = VMNC_PS_OBS
    elseif( vmnc == VMNC_PS_OBS )then
      vmnc_ = VMNC_P_OBS
    else
      vmnc_ = vmnc
    endif
  else
    vmnc_ = vmnc
  endif

  allocate(lst_obstr(nobs))
  read(obstr,*) lst_obstr(:)

  vidx = 0
  do iobs = 1, nobs
    if( lst_obstr(iobs) == vmnc_ )then
      vidx = iobs
      call echo(code%ret)
      return
    endif
  enddo

  if( .not. allow_not_found_ )then
    call eerr('Mnemonic "'//trim(vmnc_)//&
              '" was not found in $obstr')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function mnc_to_idx_obs
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine set_mod_pb__opt_log_proc(c)
  implicit none
  character(*), intent(in) :: c

  opt_log_proc = c
end subroutine set_mod_pb__opt_log_proc
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine set_dgt_nmsg(pb)
  implicit none
  type(prepbufr_), intent(in) :: pb

  d_nmsg = dgt(pb%nmsg)
end subroutine set_dgt_nmsg
!===============================================================
!
!===============================================================
subroutine set_dgt_nsub(pb)
  implicit none
  type(prepbufr_), intent(in) :: pb

  integer :: imsg

  d_nsub = 0
  do imsg = 1, pb%nmsg
    d_nsub = max(d_nsub, dgt(pb%msg(imsg)%nsub))
  enddo
end subroutine set_dgt_nsub
!===============================================================
!
!===============================================================
subroutine set_dgt_nrec(pb)
  implicit none
  type(prepbufr_), intent(in) :: pb

  integer :: imsg, isub

  d_nrec = 0
  do imsg = 1, pb%nmsg
    do isub = 1, pb%msg(imsg)%nsub
      d_nrec = max(d_nrec, dgt(pb%msg(imsg)%sub(isub)%nrec))
    enddo
  enddo
end subroutine set_dgt_nrec
!===============================================================
!
!===============================================================
integer function dgt_nmsg()
  implicit none

  dgt_nmsg = d_nmsg
end function dgt_nmsg
!===============================================================
!
!===============================================================
integer function dgt_nsub()
  implicit none

  dgt_nsub = d_nsub
end function dgt_nsub
!===============================================================
!
!===============================================================
integer function dgt_nrec()
  implicit none

  dgt_nrec = d_nrec
end function dgt_nrec
!===============================================================
!
!===============================================================
end module mod_pb
