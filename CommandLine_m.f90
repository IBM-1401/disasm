module CommandLine_m

! Command line processor for 1401 dissamblers and undumpers.

  implicit NONE
  private

  public :: CommandLine

contains

  subroutine CommandLine ( ShowAddr, ShowMem, Format, Title, Offset, WS )
    logical,intent(out) :: ShowAddr    ! Show addresses in output lines
    logical,intent(out) :: ShowMem     ! Dump memory after reading the SIMH dump
    character, intent(out), optional :: Format   ! A = Autocoder,
                                       ! D = simh Dump, 1 = one per card,
                                       ! 7 = seven per card, S = SPS,
                                       ! 4 = 1440 format
    character(*), intent(out), optional :: Title ! to put on JOB card
    integer, intent(out), optional :: Offset     ! for tape dump
    character, intent(out), optional :: WS       ! Word separator encoding
    integer :: I, L, Stat
    character(100) :: Line, NextField
    character :: MyWS

    if ( present(format) ) format = ''
    if ( present(title) ) title=''

    ! Analyze command line
    l = 1
    showAddr = .false.
    showMem = .false.
    myWS = '~' ! Default is Pierce encoding
    do
      call getarg ( l, line )
      if ( line(1:3) == "" ) exit
      if ( line(1:1) == '-' ) then
        do i = 2, len(line)
          if ( line(i:i) == "" ) then
            exit
          else if ( line(i:i) == "a" ) then
            showAddr = .true.
          else if ( line(i:i) == "m" ) then
            showMem = .true.
          else if ( present(format) .and. line(i:i) == 'A' ) then
            format = 'A' ! Autocoder
          else if ( present(format) .and. line(i:i) == 'D' ) then
            format = 'D' ! simh Dump
          else if ( present(format) .and. line(i:i) == '1' ) then
            format = '1' ! One-field-per-card
          else if ( present(format) .and. line(i:i) == '4' ) then
            format = '4' ! 1440 format
          else if ( present(format) .and. line(i:i) == '7' ) then
            format = '7' ! Seven-fields-per-card
          else if ( line(i:i) == 'p' ) then
            myWS = '~' ! Pierce encoding
          else if ( present(format) .and. line(i:i) == 'S' ) then
            format = 'S' ! SPS
          else if ( line(i:i) == 's' ) then
            myWS = '=' ! traditional SimH encoding
          else if ( present(format) .and. present(offset) &
            & .and. line(i:i) == 'T' ) then
            format = 'T' ! Tape, next field is offset else zero
            offset = 0
            call getarg ( l+1, nextField )
            if ( nextField /= '' ) then
              read ( nextField, *, iostat=stat ) offset
              if ( stat /= 0 ) then
                print *, 'Cannot get offset, zero used'
                offset = 0
              else
                l = l + 1 ! skip next field
              end if
            end if
          else
            call usage
          end if
        end do
      else if ( present(title) ) then
        title = line
      else
        call usage
      end if
      l = l + 1
    end do

    if ( present(ws) ) ws = myWS

  contains

    subroutine Usage
      call getarg ( 0, line )
      if ( present(title) ) then
        print '(4a)', 'Usage: ', trim(line), ' [options] [title] <input >output'
      else
        print '(4a)', 'Usage: ', trim(line), ' [options] <input >output'
      end if
      print '(a)', ' options: -a => Put addresses in 1:5 of output'
      print '(a)', '          -m => Dump simulated memory after input'
      if ( present(format) ) then
        print '(a)', '          -A => Autocoder format input'
        print '(a)', '          -D => simh Dump'
        print '(a)', '          -1 => One-field-per-card format input'
        print '(a)', '          -4 => 1440 format input'
        print '(a)', '          -7 => seven-field-per-card format input'
        print '(a)', '          -S => SPS format input'
        print '(a)', '          -T => Tape dump, next field is offset'
      end if
      print '(a)', '          -p => use ~ for WS (Pierce encoding)'
      print '(a)', '          -s => use = for WS (traditional SimH encoding)'
      print '(a)', '                default encoding is ' // myWS
      print '(a)', '          else => This output'
      print '(a)', ' Options can be combined, e.g. -am'
      print '(a)', 'Copyright (c) Van Snyder 2016.  2016-01-16 version.'
      stop
    end subroutine Usage

  end subroutine CommandLine

end module CommandLine_m

!>> 2013-04-24 Make sure "offset" has a value even if there is no field after -T
