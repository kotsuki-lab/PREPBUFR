program main
  use common_XXXXXX, only: &
    set_common_XXXXXX
  use lib_const
  use lib_log
  use lib_io
  use mod_pb, only: &
    dump_prepbufr
  use mod_set, only: &
    read_conf
  use mod_run, only: &
    run_get_stats, &
    run_make_obs
  implicit none

  character(CLEN_VAR) :: job
  character(CLEN_PATH) :: f_conf
  character(CLEN_PATH) :: f_pb, f_table, f_data, f_obs
  logical :: replace_old_dumped_file
  integer :: narg
  integer :: un

  namelist/nml_file/ &
    f_pb, f_table, f_data, f_obs, &
    replace_old_dumped_file

  call echo(code%bgn, 'program main', '+tr')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  narg = argnum()
  if( narg /= 2 )then
    call eerr('Invalid command line argument.'//&
            '\n[usage] $ ./main job f_conf')
  endif

  call getarg(1, job)
  call getarg(2, f_conf)
  !-------------------------------------------------------------
  f_pb    = ''
  f_table = ''
  f_data  = ''
  f_obs   = ''
  replace_old_dumped_file = .false.
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading config file')

  un = unit_number()
  open(un, file=f_conf, status='old')
  read(un, nml=nml_file)
  close(un)
  call edbg('f_pb   : '//str(f_pb))
  call edbg('f_table: '//str(f_table))
  call edbg('f_data : '//str(f_data))
  call edbg('f_obs  : '//str(f_obs))
  call edbg('replace_old_dumped_file: '//str(replace_old_dumped_file))

  call echo(code%ext)
  !-------------------------------------------------------------
  selectcase( job )
  case( 'dump' )
    call dump_prepbufr(f_pb, f_table, f_data, replace_old_dumped_file)
  case( 'get_stats' )
    call run_get_stats(f_conf)
  case( 'make_obs' )
    call run_make_obs(f_conf)
  case default
    call eerr('Invalid value in $job: '//trim(job))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end program main
