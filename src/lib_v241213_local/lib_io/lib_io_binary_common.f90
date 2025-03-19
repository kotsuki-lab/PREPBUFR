module lib_io_binary_common
  use lib_const
  use lib_log
  implicit none

contains
!===============================================================
!
!===============================================================
subroutine open_input_file_stream(&
    un, path, endian, &
    info, present_info)
  implicit none
  integer     , intent(in)  :: un
  character(*), intent(in)  :: path
  character(*), intent(in)  :: endian
  integer     , intent(out) :: info
  logical     , intent(in)  :: present_info

  call echo(code%bgn, 'open_input_file_stream', '-p')
  !-------------------------------------------------------------
  info = 0

  if( present_info )then
    selectcase( endian )
    case( ENDIAN_BIG, ENDIAN_BIG_SHORT, &
          ENDIAN_LITTLE, ENDIAN_LITTLE_SHORT )
      open(un, file=path, &
           form='unformatted', access='stream', &
           action='read', status='old', convert=endian, &
           iostat=info)
    case( ENDIAN_UNDEF )
      open(un, file=path, &
           form='unformatted', access='stream', &
           action='read', status='old', &
           iostat=info)
    case default
      call eerr('Invalid value in $endian: '//str(endian))
    endselect
  else
    info = 0
    selectcase( endian )
    case( ENDIAN_BIG, ENDIAN_BIG_SHORT, &
          ENDIAN_LITTLE, ENDIAN_LITTLE_SHORT )
      open(un, file=path, &
           form='unformatted', access='stream', &
           action='read', status='old', convert=endian)
    case( ENDIAN_UNDEF )
      open(un, file=path, &
           form='unformatted', access='stream', &
           action='read', status='old')
    case default
      call eerr('Invalid value in $endian: '//str(endian))
    endselect
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine open_input_file_stream
!===============================================================
!
!===============================================================
subroutine open_output_file_stream(&
    un, path, endian, replace, &
    info, present_info)
  implicit none
  integer     , intent(in)  :: un
  character(*), intent(in)  :: path
  character(*), intent(in)  :: endian
  logical     , intent(in)  :: replace
  integer     , intent(out) :: info
  logical     , intent(in)  :: present_info

  if( present_info )then
    open(un, file=path, &
         form='unformatted', access='stream', &
         action=action_for_replace(replace), &
         status='unknown', convert=endian, &
         iostat=info)
  else
    info = 0
    open(un, file=path, &
         form='unformatted', access='stream', &
         action=action_for_replace(replace), &
         status='unknown', convert=endian)
  endif
end subroutine open_output_file_stream
!===============================================================
!
!===============================================================
subroutine close_file(&
    un, info, present_info)
  implicit none
  integer, intent(in)  :: un
  integer, intent(out) :: info
  logical, intent(in)  :: present_info

  if( present_info )then
    close(un, iostat=info)
  else
    close(un)
  endif
end subroutine close_file
!===============================================================
!
!===============================================================
character(clen_key) function action_for_replace(replace) result(ret)
  implicit none
  logical, intent(in) :: replace

  if( replace )then
    ret = 'write'
  else
    ret = 'readwrite'
  endif
end function action_for_replace
!===============================================================
!
!===============================================================
end module lib_io_binary_common
