module lib_base_message
  use lib_const, only: &
    CLEN_MSG
  implicit none
  private

  public :: msg_invalid_value
  public :: msg_unexpected_condition
  public :: msg_syntax_error
  public :: msg_io_error
  public :: msg_internal_error
  public :: msg_not_implemented
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
character(CLEN_MSG) function msg_invalid_value() result(res)
  implicit none

  res = 'Invalid value.'
end function msg_invalid_value
!===============================================================
!
!===============================================================
character(CLEN_MSG) function msg_unexpected_condition() result(res)
  implicit none

  res = 'Unexpected condition.'
end function msg_unexpected_condition
!===============================================================
!
!===============================================================
character(CLEN_MSG) function msg_syntax_error() result(res)
  implicit none

  res = 'Syntax error.'
end function msg_syntax_error
!===============================================================
!
!===============================================================
character(CLEN_MSG) function msg_io_error() result(res)
  implicit none

  res = 'I/O error.'
end function msg_io_error
!===============================================================
!
!===============================================================
character(CLEN_MSG) function msg_internal_error() result(res)
  implicit none

  res = 'Internal error.'
end function msg_internal_error
!===============================================================
!
!===============================================================
character(clen_msg) function msg_not_implemented() result(res)
  implicit none

  res = 'Not implemented yet.'
end function msg_not_implemented
!===============================================================
!
!===============================================================
end module lib_base_message
