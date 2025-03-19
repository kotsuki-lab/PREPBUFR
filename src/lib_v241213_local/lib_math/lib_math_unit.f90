module lib_math_unit
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: conv_unit
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface conv_unit
    module procedure conv_unit_self_dble_1d
    module procedure conv_unit_self_dble_2d
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine conv_unit_self_dble_1d(d, uin, uout)
  implicit none
  real(8), intent(inout)  :: d(:)
  character(*), intent(in) :: uin
  character(*), intent(in) :: uout

  logical :: is_ok

  call echo(code%bgn, 'conv_unit__MP__conv_unit_self_dble_1d', '-p -x2')
  !-------------------------------------------------------------
  if( uin == uout )then
    call echo(code%ret)
    return
  endif

  is_ok = .true.

  selectcase( uin )
  case( unit_square_meter )
    selectcase( uout )
    case( unit_square_meter )
      ! uin == uout
    case( unit_square_kilometer )
      d = d * 1d-6
    case default
      is_ok = .false.
    endselect
  case( unit_square_kilometer )
    selectcase( uout )
    case( unit_square_meter )
      d = d * 1d6
    case( unit_square_kilometer )
      ! uin == uout
    case default
      is_ok = .false.
    endselect
  case default
    is_ok = .false.
  endselect

  if( .not. is_ok )then
    call eerr(str(msg_invalid_value())//&
            '\n  $uin : '//str(uin)//&
            '\n  $uout: '//str(uout))
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine conv_unit_self_dble_1d
!===============================================================
!
!===============================================================
subroutine conv_unit_self_dble_2d(d, uin, uout)
  implicit none
  real(8), intent(inout)  :: d(:,:)
  character(*), intent(in) :: uin
  character(*), intent(in) :: uout

  logical :: is_ok

  call echo(code%bgn, 'conv_unit__MP__conv_unit_self_dble_2d', '-p -x2')
  !-------------------------------------------------------------
  if( uin == uout )then
    call echo(code%ret)
    return
  endif

  is_ok = .true.

  selectcase( uin )
  case( unit_square_meter )
    selectcase( uout )
    case( unit_square_meter )
      ! uin == uout
    case( unit_square_kilometer )
      d = d * 1d-6
    case default
      is_ok = .false.
    endselect
  case( unit_square_kilometer )
    selectcase( uout )
    case( unit_square_meter )
      d = d * 1d6
    case( unit_square_kilometer )
      ! uin == uout
    case default
      is_ok = .false.
    endselect
  case default
    is_ok = .false.
  endselect

  if( .not. is_ok )then
    call eerr(str(msg_invalid_value())//&
            '\n  $uin : '//str(uin)//&
            '\n  $uout: '//str(uout))
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine conv_unit_self_dble_2d
!===============================================================
!
!===============================================================
end module lib_math_unit
