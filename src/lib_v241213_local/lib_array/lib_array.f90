module lib_array
  use lib_array_realloc
  use lib_array_copy
  use lib_array_reverse
  use lib_array_sort
  implicit none
  private
  !-------------------------------------------------------------
  ! lib_array_realloc
  !-------------------------------------------------------------
  public :: realloc
  !-------------------------------------------------------------
  ! lib_array_copy
  !-------------------------------------------------------------
  public :: copy
  !-------------------------------------------------------------
  ! lib_array_reverse
  !-------------------------------------------------------------
  public :: reverse
  public :: reversed
  !-------------------------------------------------------------
  ! lib_array_sort
  !-------------------------------------------------------------
  public :: sort
  public :: sorted
  public :: argsort
  public :: search
  public :: search_nearest
  public :: search_linear
  !-------------------------------------------------------------
end module lib_array
