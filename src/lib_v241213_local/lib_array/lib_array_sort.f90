module lib_array_sort
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: sort
  public :: sorted
  public :: argsort
  public :: search
  public :: search_nearest
  public :: search_linear
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface sort
    module procedure sort__int1
    module procedure sort__int2
    module procedure sort__int4
    module procedure sort__int8
    module procedure sort__real
    module procedure sort__dble
    module procedure sort_arg__int1_arg8
    module procedure sort_arg__int2_arg8
    module procedure sort_arg__int4_arg8
    module procedure sort_arg__int8_arg8
    module procedure sort_arg__real_arg8
    module procedure sort_arg__dble_arg8
    module procedure sort_arg__int1_arg4
    module procedure sort_arg__int2_arg4
    module procedure sort_arg__int4_arg4
    module procedure sort_arg__int8_arg4
    module procedure sort_arg__real_arg4
    module procedure sort_arg__dble_arg4
  end interface

  interface argsort
    module procedure argsort__int1_arg8
    module procedure argsort__int2_arg8
    module procedure argsort__int4_arg8
    module procedure argsort__int8_arg8
    module procedure argsort__real_arg8
    module procedure argsort__dble_arg8
    module procedure argsort__int1_arg4
    module procedure argsort__int2_arg4
    module procedure argsort__int4_arg4
    module procedure argsort__int8_arg4
    module procedure argsort__real_arg4
    module procedure argsort__dble_arg4
  end interface

  interface sorted
    module procedure sorted__int1
    module procedure sorted__int2
    module procedure sorted__int4
    module procedure sorted__int8
    module procedure sorted__real
    module procedure sorted__dble
  end interface

  interface search
    module procedure search_binary_sorted__int4_loc8
    module procedure search_binary_sorted__int8_loc8
    module procedure search_binary_sorted__real_loc8
    module procedure search_binary_sorted__dble_loc8
    module procedure search_binary_sorted__int4_loc4
    module procedure search_binary_sorted__int8_loc4
    module procedure search_binary_sorted__real_loc4
    module procedure search_binary_sorted__dble_loc4
    module procedure search_binary_arg__int4_arg8_loc8
    module procedure search_binary_arg__int8_arg8_loc8
    module procedure search_binary_arg__real_arg8_loc8
    module procedure search_binary_arg__dble_arg8_loc8
    module procedure search_binary_arg__int4_arg4_loc4
    module procedure search_binary_arg__int8_arg4_loc4
    module procedure search_binary_arg__real_arg4_loc4
    module procedure search_binary_arg__dble_arg4_loc4
  end interface

  interface search_nearest
    module procedure search_binary_nearest_sorted__int4
    module procedure search_binary_nearest_sorted__dble
    module procedure search_binary_nearest_arg__dble
  end interface

  interface search_linear
    module procedure search_linear__int4_loc8
    module procedure search_linear__int8_loc8
    module procedure search_linear__int4_loc4
    module procedure search_linear__int8_loc4
  end interface
  !-------------------------------------------------------------
contains
!==============================================================
!
!==============================================================
subroutine sort_arg__int1_arg8(array, arg)
  implicit none
  integer(1), intent(inout) :: array(:)
  integer(8), intent(in)    :: arg(:)

  integer(1), allocatable :: arr(:)
  integer(8) :: n, i

  n = size(arg,kind=8)
  allocate(arr(n))
  do i = 1_8, n
    arr(i) = array(arg(i))
  enddo
  do i = 1_8, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__int1_arg8
!==============================================================
!
!==============================================================
subroutine sort_arg__int2_arg8(array, arg)
  implicit none
  integer(2), intent(inout) :: array(:)
  integer(8), intent(in)    :: arg(:)

  integer(2), allocatable :: arr(:)
  integer(8) :: n, i

  n = size(arg,kind=8)
  allocate(arr(n))
  do i = 1_8, n
    arr(i) = array(arg(i))
  enddo
  do i = 1_8, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__int2_arg8
!==============================================================
!
!==============================================================
subroutine sort_arg__int4_arg8(array, arg)
  implicit none
  integer(4), intent(inout) :: array(:)
  integer(8), intent(in)    :: arg(:)

  integer(4), allocatable :: arr(:)
  integer(8) :: n, i

  n = size(arg,kind=8)
  allocate(arr(n))
  do i = 1_8, n
    arr(i) = array(arg(i))
  enddo
  do i = 1_8, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__int4_arg8
!==============================================================
!
!==============================================================
subroutine sort_arg__int8_arg8(array, arg)
  implicit none
  integer(8), intent(inout) :: array(:)
  integer(8), intent(in)    :: arg(:)

  integer(8), allocatable :: arr(:)
  integer(8) :: n, i

  n = size(arg,kind=8)
  allocate(arr(n))
  do i = 1_8, n
    arr(i) = array(arg(i))
  enddo
  do i = 1_8, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__int8_arg8
!==============================================================
!
!==============================================================
subroutine sort_arg__real_arg8(array, arg)
  implicit none
  real(4)   , intent(inout) :: array(:)
  integer(8), intent(in)    :: arg(:)

  real(4), allocatable :: arr(:)
  integer(8) :: n, i

  n = size(arg,kind=8)
  allocate(arr(n))
  do i = 1_8, n
    arr(i) = array(arg(i))
  enddo
  do i = 1_8, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__real_arg8
!==============================================================
!
!==============================================================
subroutine sort_arg__dble_arg8(array, arg)
  implicit none
  real(8)   , intent(inout) :: array(:)
  integer(8), intent(in)    :: arg(:)

  real(8), allocatable :: arr(:)
  integer(8) :: n, i

  n = size(arg,kind=8)
  allocate(arr(n))
  do i = 1_8, n
    arr(i) = array(arg(i))
  enddo
  do i = 1_8, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__dble_arg8
!==============================================================
!
!==============================================================
subroutine sort_arg__int1_arg4(array, arg)
  implicit none
  integer(1), intent(inout) :: array(:)
  integer(4), intent(in)    :: arg(:)

  integer(1), allocatable :: arr(:)
  integer(4) :: n, i

  n = size(arg)
  allocate(arr(n))
  do i = 1, n
    arr(i) = array(arg(i))
  enddo
  do i = 1, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__int1_arg4
!==============================================================
!
!==============================================================
subroutine sort_arg__int2_arg4(array, arg)
  implicit none
  integer(2), intent(inout) :: array(:)
  integer(4), intent(in)    :: arg(:)

  integer(2), allocatable :: arr(:)
  integer(4) :: n, i

  n = size(arg)
  allocate(arr(n))
  do i = 1, n
    arr(i) = array(arg(i))
  enddo
  do i = 1, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__int2_arg4
!==============================================================
!
!==============================================================
subroutine sort_arg__int4_arg4(array, arg)
  implicit none
  integer(4), intent(inout) :: array(:)
  integer(4), intent(in)    :: arg(:)

  integer(4), allocatable :: arr(:)
  integer(4) :: n, i

  n = size(arg)
  allocate(arr(n))
  do i = 1, n
    arr(i) = array(arg(i))
  enddo
  do i = 1, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__int4_arg4
!==============================================================
!
!==============================================================
subroutine sort_arg__int8_arg4(array, arg)
  implicit none
  integer(8), intent(inout) :: array(:)
  integer(4), intent(in)    :: arg(:)

  integer(8), allocatable :: arr(:)
  integer(4) :: n, i

  n = size(arg)
  allocate(arr(n))
  do i = 1, n
    arr(i) = array(arg(i))
  enddo
  do i = 1, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__int8_arg4
!==============================================================
!
!==============================================================
subroutine sort_arg__real_arg4(array, arg)
  implicit none
  real(4)   , intent(inout) :: array(:)
  integer(4), intent(in)    :: arg(:)

  real(4), allocatable :: arr(:)
  integer(4) :: n, i

  n = size(arg)
  allocate(arr(n))
  do i = 1, n
    arr(i) = array(arg(i))
  enddo
  do i = 1, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__real_arg4
!==============================================================
!
!==============================================================
subroutine sort_arg__dble_arg4(array, arg)
  implicit none
  real(8)   , intent(inout) :: array(:)
  integer(4), intent(in)    :: arg(:)

  real(8), allocatable :: arr(:)
  integer(4) :: n, i

  n = size(arg)
  allocate(arr(n))
  do i = 1, n
    arr(i) = array(arg(i))
  enddo
  do i = 1, n
    array(i) = arr(i)
  enddo
  deallocate(arr)
end subroutine sort_arg__dble_arg4
!==============================================================
!
!==============================================================
!
!
!
!
!
!==============================================================
!
!==============================================================
subroutine sort__int1(array)
  implicit none
  integer(1), intent(inout) :: array(:)
  integer(1) :: t
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  if( n == 1 ) return

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = array(l)
    else
      t = array(k)
      array(k) = array(1)
      k = k - 1
      if (k == 1) then
         array(1) = t
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (array(j) < array(j+1)) j = j + 1
      endif
      if (t < array(j)) then
        array(i) = array(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    array(i) = t
  enddo
end subroutine sort__int1
!==============================================================
!
!==============================================================
subroutine sort__int2(array)
  implicit none
  integer(2), intent(inout) :: array(:)
  integer(2) :: t
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  if( n == 1 ) return

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = array(l)
    else
      t = array(k)
      array(k) = array(1)
      k = k - 1
      if (k == 1) then
         array(1) = t
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (array(j) < array(j+1)) j = j + 1
      endif
      if (t < array(j)) then
        array(i) = array(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    array(i) = t
  enddo
end subroutine sort__int2
!==============================================================
!
!==============================================================
subroutine sort__int4(array)
  implicit none
  integer(4), intent(inout) :: array(:)
  integer(4) :: t
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  if( n == 1 ) return

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = array(l)
    else
      t = array(k)
      array(k) = array(1)
      k = k - 1
      if (k == 1) then
         array(1) = t
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (array(j) < array(j+1)) j = j + 1
      endif
      if (t < array(j)) then
        array(i) = array(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    array(i) = t
  enddo
end subroutine sort__int4
!==============================================================
!
!==============================================================
subroutine sort__int8(array)
  implicit none
  integer(8), intent(inout) :: array(:)
  integer(8) :: t
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  if( n == 1 ) return

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = array(l)
    else
      t = array(k)
      array(k) = array(1)
      k = k - 1
      if (k == 1) then
         array(1) = t
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (array(j) < array(j+1)) j = j + 1
      endif
      if (t < array(j)) then
        array(i) = array(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    array(i) = t
  enddo
end subroutine sort__int8
!==============================================================
!
!==============================================================
subroutine sort__real(array)
  implicit none
  real(4), intent(inout) :: array(:)
  real(4) :: t
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  if( n == 1 ) return

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = array(l)
    else
      t = array(k)
      array(k) = array(1)
      k = k - 1
      if (k == 1) then
         array(1) = t
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (array(j) < array(j+1)) j = j + 1
      endif
      if (t < array(j)) then
        array(i) = array(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    array(i) = t
  enddo
end subroutine sort__real
!==============================================================
!
!==============================================================
subroutine sort__dble(array)
  implicit none
  real(8), intent(inout) :: array(:)
  real(8) :: t
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  if( n == 1 ) return

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = array(l)
    else
      t = array(k)
      array(k) = array(1)
      k = k - 1
      if (k == 1) then
         array(1) = t
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (array(j) < array(j+1)) j = j + 1
      endif
      if (t < array(j)) then
        array(i) = array(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    array(i) = t
  enddo
end subroutine sort__dble
!==============================================================
!
!==============================================================
!
!
!
!
!
!==============================================================
!
!==============================================================
subroutine argsort__int1_arg8(array, arg)
  implicit none
  integer, parameter :: byte = 1
  integer(byte), intent(in)  :: array(:)
  integer(8)   , intent(out) :: arg(:)
  integer(byte), allocatable :: arr(:)
  integer(byte) :: t  ! arr
  integer(8)    :: u  ! arg
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__int1_arg8
!==============================================================
!
!==============================================================
subroutine argsort__int2_arg8(array, arg)
  implicit none
  integer, parameter :: byte = 2
  integer(byte), intent(in)  :: array(:)
  integer(8)   , intent(out) :: arg(:)
  integer(byte), allocatable :: arr(:)
  integer(byte) :: t  ! arr
  integer(8)    :: u  ! arg
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__int2_arg8
!==============================================================
!
!==============================================================
subroutine argsort__int4_arg8(array, arg)
  implicit none
  integer, parameter :: byte = 4
  integer(byte), intent(in)  :: array(:)
  integer(8)   , intent(out) :: arg(:)
  integer(byte), allocatable :: arr(:)
  integer(byte) :: t  ! arr
  integer(8)    :: u  ! arg
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__int4_arg8
!==============================================================
!
!==============================================================
subroutine argsort__int8_arg8(array, arg)
  implicit none
  integer, parameter :: byte = 8
  integer(byte), intent(in)  :: array(:)
  integer(8)   , intent(out) :: arg(:)
  integer(byte), allocatable :: arr(:)
  integer(byte) :: t  ! arr
  integer(8)    :: u  ! arg
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__int8_arg8
!==============================================================
!
!==============================================================
subroutine argsort__real_arg8(array, arg)
  implicit none
  integer, parameter :: byte = 4
  real(byte), intent(in)  :: array(:)
  integer(8), intent(out) :: arg(:)
  real(byte), allocatable :: arr(:)
  real(byte) :: t  ! arr
  integer(8) :: u  ! arg
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__real_arg8
!==============================================================
!
!==============================================================
subroutine argsort__dble_arg8(array, arg)
  implicit none
  integer, parameter :: byte = 8
  real(byte), intent(in)  :: array(:)
  integer(8), intent(out) :: arg(:)
  real(byte), allocatable :: arr(:)
  real(byte) :: t  ! arr
  integer(8) :: u  ! arg
  integer(8) :: n
  integer(8) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__dble_arg8
!==============================================================
!
!==============================================================
subroutine argsort__int1_arg4(array, arg)
  implicit none
  integer, parameter :: byte = 1
  integer(byte), intent(in)  :: array(:)
  integer(4)   , intent(out) :: arg(:)
  integer(byte), allocatable :: arr(:)
  integer(byte) :: t  ! arr
  integer(4)    :: u  ! arg
  integer(4) :: n
  integer(4) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__int1_arg4
!==============================================================
!
!==============================================================
subroutine argsort__int2_arg4(array, arg)
  implicit none
  integer, parameter :: byte = 2
  integer(byte), intent(in)  :: array(:)
  integer(4)   , intent(out) :: arg(:)
  integer(byte), allocatable :: arr(:)
  integer(byte) :: t  ! arr
  integer(4)    :: u  ! arg
  integer(4) :: n
  integer(4) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__int2_arg4
!==============================================================
!
!==============================================================
subroutine argsort__int4_arg4(array, arg)
  implicit none
  integer, parameter :: byte = 4
  integer(byte), intent(in)  :: array(:)
  integer(4)   , intent(out) :: arg(:)
  integer(byte), allocatable :: arr(:)
  integer(byte) :: t  ! arr
  integer(4)    :: u  ! arg
  integer(4) :: n
  integer(4) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__int4_arg4
!==============================================================
!
!==============================================================
subroutine argsort__int8_arg4(array, arg)
  implicit none
  integer, parameter :: byte = 8
  integer(byte), intent(in)  :: array(:)
  integer(4)   , intent(out) :: arg(:)
  integer(byte), allocatable :: arr(:)
  integer(byte) :: t  ! arr
  integer(4)    :: u  ! arg
  integer(4) :: n
  integer(4) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__int8_arg4
!==============================================================
!
!==============================================================
subroutine argsort__real_arg4(array, arg)
  implicit none
  integer, parameter :: byte = 4
  real(byte), intent(in)  :: array(:)
  integer(4), intent(out) :: arg(:)
  real(byte), allocatable :: arr(:)
  real(byte) :: t  ! arr
  integer(4) :: u  ! arg
  integer(4) :: n
  integer(4) :: i, j, k, l

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__real_arg4
!==============================================================
!
!==============================================================
subroutine argsort__dble_arg4(array, arg)
  implicit none
  integer, parameter :: byte = 8
  real(byte), intent(in)  :: array(:)
  integer(4), intent(out) :: arg(:)
  real(byte), allocatable :: arr(:)
  real(byte) :: t  ! arr
  integer(4) :: u  ! arg
  integer(4) :: n  ! arg
  integer(4) :: i, j, k, l  ! arg

  n = size(array)

  do i = 1_8, n
    arg(i) = i
  enddo

  if( n == 1 ) return

  allocate(arr(n))
  arr = array

  l = n/2 + 1
  k = n
  do while (k /= 1)
    if (l > 1) then
      l = l-1
      t = arr(l)
      u = arg(l)
    else
      t = arr(k)
      u = arg(k)
      arr(k) = arr(1)
      arg(k) = arg(1)
      k = k - 1
      if (k == 1) then
         arr(1) = t
         arg(1) = u
         exit
      endif
    endif
    i = l
    j = l + l
    do while (j <= k)
      if (j < k) then
        if (arr(j) < arr(j+1)) j = j + 1
      endif
      if (t < arr(j)) then
        arr(i) = arr(j)
        arg(i) = arg(j)
        i = j
        j = j + j
      else
        j = k + 1
      endif
    enddo
    arr(i) = t
    arg(i) = u
  enddo

  deallocate(arr)
end subroutine argsort__dble_arg4
!==============================================================
!
!==============================================================
!
!
!
!
!
!==============================================================
!
!==============================================================
function sorted__int1(array) result(array_sorted)
  implicit none
  integer(1), intent(in) :: array(:)
  integer(1)             :: array_sorted(size(array))

  array_sorted(:) = array(:)

  call sort__int1(array_sorted)
end function sorted__int1
!==============================================================
!
!==============================================================
function sorted__int2(array) result(array_sorted)
  implicit none
  integer(2), intent(in) :: array(:)
  integer(2)             :: array_sorted(size(array))

  array_sorted(:) = array(:)

  call sort__int2(array_sorted)
end function sorted__int2
!==============================================================
!
!==============================================================
function sorted__int4(array) result(array_sorted)
  implicit none
  integer(4), intent(in) :: array(:)
  integer(4)             :: array_sorted(size(array))

  array_sorted(:) = array(:)

  call sort__int4(array_sorted)
end function sorted__int4
!==============================================================
!
!==============================================================
function sorted__int8(array) result(array_sorted)
  implicit none
  integer(8), intent(in) :: array(:)
  integer(8)             :: array_sorted(size(array))

  array_sorted(:) = array(:)

  call sort__int8(array_sorted)
end function sorted__int8
!==============================================================
!
!==============================================================
function sorted__real(array) result(array_sorted)
  implicit none
  real(4)   , intent(in) :: array(:)
  real(4)                :: array_sorted(size(array))

  array_sorted(:) = array(:)

  call sort__real(array_sorted)
end function sorted__real
!==============================================================
!
!==============================================================
function sorted__dble(array) result(array_sorted)
  implicit none
  real(8), intent(in) :: array(:)
  real(8)             :: array_sorted(size(array))

  array_sorted(:) = array(:)

  call sort__dble(array_sorted)
end function sorted__dble
!==============================================================
!
!==============================================================
!
!
!
!
!
!==============================================================
! Binary search
!==============================================================
subroutine search_binary_sorted__int4_loc8(x, arr, loc)
  implicit none
  integer(4), intent(in)  :: x
  integer(4), intent(in)  :: arr(:)
  integer(8), intent(out) :: loc
  integer(8) :: ijmax, left, right

  ijmax = size(arr)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(loc) == x )then
      return
    elseif( arr(loc) < x )then
      left = loc + 1
    else
      right = loc - 1
    endif
  enddo

  loc = 0_8
end subroutine search_binary_sorted__int4_loc8
!==============================================================
!
!==============================================================
subroutine search_binary_sorted__int8_loc8(x, arr, loc)
  implicit none
  integer(8), intent(in)  :: x
  integer(8), intent(in)  :: arr(:)
  integer(8), intent(out) :: loc
  integer(8) :: ijmax, left, right

  ijmax = size(arr)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(loc) == x )then
      return
    elseif( arr(loc) < x )then
      left = loc + 1
    else
      right = loc - 1
    endif
  enddo

  loc = 0_8
end subroutine search_binary_sorted__int8_loc8
!==============================================================
!
!==============================================================
subroutine search_binary_sorted__real_loc8(x, arr, loc)
  implicit none
  real(4)   , intent(in)  :: x
  real(4)   , intent(in)  :: arr(:)
  integer(8), intent(out) :: loc
  integer(8) :: ijmax, left, right

  ijmax = size(arr)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(loc) == x )then
      return
    elseif( arr(loc) < x )then
      left = loc + 1
    else
      right = loc - 1
    endif
  enddo

  loc = 0_8
end subroutine search_binary_sorted__real_loc8
!==============================================================
! Binary search
!==============================================================
subroutine search_binary_sorted__dble_loc8(x, arr, loc)
  implicit none
  real(8)   , intent(in)  :: x
  real(8)   , intent(in)  :: arr(:)
  integer(8), intent(out) :: loc
  integer(8) :: ijmax, left, right

  ijmax = size(arr)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(loc) == x )then
      return
    elseif( arr(loc) < x )then
      left = loc + 1
    else
      right = loc - 1
    endif
  enddo

  loc = 0_8
end subroutine search_binary_sorted__dble_loc8
!==============================================================
!
!==============================================================
subroutine search_binary_sorted__int4_loc4(x, arr, loc)
  implicit none
  integer(4), intent(in)  :: x
  integer(4), intent(in)  :: arr(:)
  integer(4), intent(out) :: loc

  integer(8) :: loc8

  call search_binary_sorted__int4_loc8(x, arr, loc8)
  loc = int(loc8, 4)
end subroutine search_binary_sorted__int4_loc4
!==============================================================
!
!==============================================================
subroutine search_binary_sorted__int8_loc4(x, arr, loc)
  implicit none
  integer(8), intent(in)  :: x
  integer(8), intent(in)  :: arr(:)
  integer(4), intent(out) :: loc

  integer(8) :: loc8

  call search_binary_sorted__int8_loc8(x, arr, loc8)
  loc = int(loc8, 4)
end subroutine search_binary_sorted__int8_loc4
!==============================================================
!
!==============================================================
subroutine search_binary_sorted__real_loc4(x, arr, loc)
  implicit none
  real(4)   , intent(in)  :: x
  real(4)   , intent(in)  :: arr(:)
  integer(4), intent(out) :: loc

  integer(8) :: loc8

  call search_binary_sorted__real_loc8(x, arr, loc8)
  loc = int(loc8, 4)
end subroutine search_binary_sorted__real_loc4
!==============================================================
!
!==============================================================
subroutine search_binary_sorted__dble_loc4(x, arr, loc)
  implicit none
  real(8)   , intent(in)  :: x
  real(8)   , intent(in)  :: arr(:)
  integer(4), intent(out) :: loc

  integer(8) :: loc8

  call search_binary_sorted__dble_loc8(x, arr, loc8)
  loc = int(loc8, 4)
end subroutine search_binary_sorted__dble_loc4
!==============================================================
! Binary search
!==============================================================
subroutine search_binary_arg__int4_arg8_loc8(x, arr, arg, loc)
  implicit none
  integer(4), intent(in)  :: x
  integer(4), intent(in)  :: arr(:)
  integer(8), intent(in)  :: arg(:)
  integer(8), intent(out) :: loc
  integer(8) :: ijmax, left, right

  ijmax = size(arg)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(arg(loc)) == x )then
      return
    elseif( arr(arg(loc)) < x )then
      left = loc + 1
    else
      right = loc - 1
    endif
  enddo

  loc = 0_8
end subroutine search_binary_arg__int4_arg8_loc8
!==============================================================
!
!==============================================================
subroutine search_binary_arg__int8_arg8_loc8(x, arr, arg, loc)
  implicit none
  integer(8), intent(in)  :: x
  integer(8), intent(in)  :: arr(:)
  integer(8), intent(in)  :: arg(:)
  integer(8), intent(out) :: loc
  integer(8) :: ijmax, left, right

  ijmax = size(arg)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(arg(loc)) == x )then
      return
    elseif( arr(arg(loc)) < x )then
      left = loc + 1
    else
      right = loc - 1
    endif
  enddo

  loc = 0_8
end subroutine search_binary_arg__int8_arg8_loc8
!==============================================================
!
!==============================================================
subroutine search_binary_arg__real_arg8_loc8(x, arr, arg, loc)
  implicit none
  real(4)   , intent(in)  :: x
  real(4)   , intent(in)  :: arr(:)
  integer(8), intent(in)  :: arg(:)
  integer(8), intent(out) :: loc
  integer(8) :: ijmax, left, right

  ijmax = size(arg)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(arg(loc)) == x )then
      return
    elseif( arr(arg(loc)) < x )then
      left = loc + 1
    else
      right = loc - 1
    endif
  enddo

  loc = 0_8
end subroutine search_binary_arg__real_arg8_loc8
!==============================================================
!
!==============================================================
subroutine search_binary_arg__dble_arg8_loc8(x, arr, arg, loc)
  implicit none
  real(8)   , intent(in)  :: x
  real(8)   , intent(in)  :: arr(:)
  integer(8), intent(in)  :: arg(:)
  integer(8), intent(out) :: loc
  integer(8) :: ijmax, left, right

  ijmax = size(arg)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(arg(loc)) == x )then
      return
    elseif( arr(arg(loc)) < x )then
      left = loc + 1
    else
      right = loc - 1
    endif
  enddo

  loc = 0_8
end subroutine search_binary_arg__dble_arg8_loc8
!==============================================================
!
!==============================================================
subroutine search_binary_arg__int4_arg4_loc4(x, arr, arg, loc)
  implicit none
  integer(4), intent(in)  :: x
  integer(4), intent(in)  :: arr(:)
  integer(4), intent(in)  :: arg(:)
  integer(4), intent(out) :: loc

  integer(8) :: loc8

  call search_binary_arg__int4_arg8_loc8(x, arr, int(arg,8), loc8)
  loc = int(loc8,4)
end subroutine search_binary_arg__int4_arg4_loc4
!===============================================================
!
!===============================================================
subroutine search_binary_arg__int8_arg4_loc4(x, arr, arg, loc)
  implicit none
  integer(8), intent(in)  :: x
  integer(8), intent(in)  :: arr(:)
  integer(4), intent(in)  :: arg(:)
  integer(4), intent(out) :: loc

  integer(8) :: loc8

  call search_binary_arg__int8_arg8_loc8(x, arr, int(arg,8), loc8)
  loc = int(loc8,4)
end subroutine search_binary_arg__int8_arg4_loc4
!===============================================================
!
!===============================================================
subroutine search_binary_arg__real_arg4_loc4(x, arr, arg, loc)
  implicit none
  real(4)   , intent(in)  :: x
  real(4)   , intent(in)  :: arr(:)
  integer(4), intent(in)  :: arg(:)
  integer(4), intent(out) :: loc

  integer(8) :: loc8

  call search_binary_arg__real_arg8_loc8(x, arr, int(arg,8), loc8)
  loc = int(loc8,4)
end subroutine search_binary_arg__real_arg4_loc4
!===============================================================
!
!===============================================================
subroutine search_binary_arg__dble_arg4_loc4(x, arr, arg, loc)
  implicit none
  real(8)   , intent(in)  :: x
  real(8)   , intent(in)  :: arr(:)
  integer(4), intent(in)  :: arg(:)
  integer(4), intent(out) :: loc

  integer(8) :: loc8

  call search_binary_arg__dble_arg8_loc8(x, arr, int(arg,8), loc8)
  loc = int(loc8,4)
end subroutine search_binary_arg__dble_arg4_loc4
!===============================================================
!
!===============================================================
!
!
!
!
!
!==============================================================
! Usually arr(loc1) <= x <= arr(loc2)
! When arr(ijmax) < x, loc1=ijmax, loc2=ijmax+1
! When x < arr(1), loc1=0, loc2=1
!==============================================================
subroutine search_binary_nearest_sorted__int4(x, arr, loc1, loc2)
  implicit none
  integer(4), intent(in) :: x
  integer(4), intent(in) :: arr(:)
  integer(8), intent(out) :: loc1, loc2

  integer(8) :: ijmax, left, right
  integer(8) :: loc

  ijmax = size(arr)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(loc) == x )then
      loc1 = loc
      do while( loc1 > 1 )
        if( arr(loc1-1) < x ) exit
        loc1 = loc1 - 1
      enddo
      loc2 = loc
      do while( loc2 < ijmax )
        if( arr(loc2+1) > x ) exit
        loc2 = loc2 + 1
      enddo
      return
    elseif( arr(loc) < x )then
      if( loc == ijmax )then
        loc1 = loc
        loc2 = loc + 1
        return
      elseif( arr(loc+1) > x )then
        loc1 = loc
        loc2 = loc + 1
        return
      endif
      left = loc + 1
    else
      if( loc == 1 )then
        loc1 = loc - 1
        loc2 = loc
        return
      elseif( arr(loc-1) < x )then
        loc1 = loc - 1
        loc2 = loc
        return
      endif
      right = loc - 1
    endif
  enddo
end subroutine search_binary_nearest_sorted__int4
!==============================================================
!
!==============================================================
subroutine search_binary_nearest_sorted__dble(x, arr, loc1, loc2)
  implicit none
  real(8), intent(in) :: x
  real(8), intent(in) :: arr(:)
  integer(8), intent(out) :: loc1, loc2

  integer(8) :: ijmax, left, right
  integer(8) :: loc

  ijmax = size(arr)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(loc) == x )then
      loc1 = loc
      do while( loc1 > 1 )
        if( arr(loc1-1) < x ) exit
        loc1 = loc1 - 1
      enddo
      loc2 = loc
      do while( loc2 < ijmax )
        if( arr(loc2+1) > x ) exit
        loc2 = loc2 + 1
      enddo
      return
    elseif( arr(loc) < x )then
      if( loc == ijmax )then
        loc1 = loc
        loc2 = loc + 1
        return
      elseif( arr(loc+1) > x )then
        loc1 = loc
        loc2 = loc + 1
        return
      endif
      left = loc + 1
    else
      if( loc == 1 )then
        loc1 = loc - 1
        loc2 = loc
        return
      elseif( arr(loc-1) < x )then
        loc1 = loc - 1
        loc2 = loc
        return
      endif
      right = loc - 1
    endif
  enddo
end subroutine search_binary_nearest_sorted__dble
!==============================================================
!
!==============================================================
subroutine search_binary_nearest_arg__dble(x, arr, arg, loc1, loc2)
  implicit none
  real(8), intent(in) :: x
  real(8), intent(in) :: arr(:)
  integer(8), intent(in) :: arg(:)
  integer(8), intent(out) :: loc1, loc2

  integer(8) :: ijmax, left, right
  integer(8) :: loc

  ijmax = size(arr)

  left = 1_8
  right = ijmax
  do while( left <= right )
    loc = (left + right) / 2_8
    if( arr(arg(loc)) == x )then
      loc1 = loc
      do while( loc1 > 1 )
        if( arr(arg(loc1-1)) < x ) exit
        loc1 = loc1 - 1
      enddo
      loc2 = loc
      do while( loc2 < ijmax )
        if( arr(arg(loc2+1)) > x ) exit
        loc2 = loc2 + 1
      enddo
      return
    elseif( arr(arg(loc)) < x )then
      if( loc == ijmax )then
        loc1 = loc
        loc2 = loc + 1
        return
      elseif( arr(arg(loc+1)) > x )then
        loc1 = loc
        loc2 = loc + 1
        return
      endif
      left = loc + 1
    else
      if( loc == 1 )then
        loc1 = loc - 1
        loc2 = loc
        return
      elseif( arr(arg(loc-1)) < x )then
        loc1 = loc - 1
        loc2 = loc
        return
      endif
      right = loc - 1
    endif
  enddo
end subroutine search_binary_nearest_arg__dble
!==============================================================
!
!==============================================================
!
!
!
!
!
!==============================================================
!
!==============================================================
subroutine search_linear__int4_loc8(x, arr, loc)
  implicit none
  integer(4), intent(in) :: x
  integer(4), intent(in) :: arr(:)
  integer(8), intent(out) :: loc

  loc = 1_8
  do while( loc <= size(arr) )
    if( arr(loc) == x ) return
    loc = loc + 1_8
  enddo

  ! ERROR
  loc = 0_8
end subroutine search_linear__int4_loc8
!==============================================================
!
!==============================================================
subroutine search_linear__int8_loc8(x, arr, loc)
  implicit none
  integer(8), intent(in) :: x
  integer(8), intent(in) :: arr(:)
  integer(8), intent(out) :: loc

  loc = 1_8
  do while( loc <= size(arr) )
    if( arr(loc) == x ) return
    loc = loc + 1_8
  enddo

  ! ERROR
  loc = 0_8
end subroutine search_linear__int8_loc8
!==============================================================
!
!==============================================================
subroutine search_linear__int4_loc4(x, arr, loc)
  implicit none
  integer(4), intent(in) :: x
  integer(4), intent(in) :: arr(:)
  integer(4), intent(out) :: loc

  integer(8) :: loc8

  call search_linear__int4_loc8(x, arr, loc8)
  loc = int(loc8,4)
end subroutine search_linear__int4_loc4
!==============================================================
!
!==============================================================
subroutine search_linear__int8_loc4(x, arr, loc)
  implicit none
  integer(8), intent(in) :: x
  integer(8), intent(in) :: arr(:)
  integer(4), intent(out) :: loc

  integer(8) :: loc8

  call search_linear__int8_loc8(x, arr, loc8)
  loc = int(loc8,4)
end subroutine search_linear__int8_loc4
!==============================================================
!
!==============================================================
end module lib_array_sort
