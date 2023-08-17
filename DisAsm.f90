program DisAsm

! Disassembles object decks in Autocoder, SPS, one-field-per-card and
! seven-field-per-card formats.

  use CommandLine_m, only: CommandLine
  use DisAsmAutocoder_m, only: DisAsmAutocoder
  use DisAsmDump_m, only: DisAsmDump
  use DisAsmOnePerCard_m, only: DisAsmOnePerCard
  use DisAsmSevenPerCard_m, only: DisAsmSevenPerCard
  use DisAsmSPS_m, only: DisAsmSPS
  use DisAsmTape_m, only: DisAsmTape
  use DisAsm_1440_m, only: DisAsm_1440

  implicit NONE

  character :: Format    ! A = Autocoder, 1 = one per card,
                         ! 7 = seven per card, S = SPS
  character(120) :: Line ! First card of input
  integer :: Offset      ! For tape dump
  logical :: ShowAddr    ! Show addresses in 6:10; set by -a option
  logical :: ShowMem     ! Show memory contents; set by -m option
  character(55) :: Title ! To put on JOB card
  character :: WS = '~'  ! Word separator character

  ! Analyze command line
  call commandLine ( showAddr, showMem, format, title, offset, WS )

  if ( format == '' ) then ! decide the format from the first card
    read ( *, '(a)', end=9 ) line
    if ( line(38:40) == '057' ) then
      format = 'A'  ! Autocoder, <= 4k
    else if ( line(26:29) == ',049' ) then
      format = 'A'  ! Autocoder, > 4k
    else if ( index(line(1:9),':') /= 0 ) then
      format = 'D'  ! simh Dump
    else if ( line(29:29) == ' ' .and. line(8:11) == '1001' ) then
      format = '1'  ! One-field-per-card
    else if ( line(28:32) == ' 1001' ) then
      format = '7'  ! Seven-field-per-card
    else if ( line(26:29) == ',045' ) then
       format = 'S' ! SPS
    else
      print '(a,a)', 'Cannot determine format; specify it using a ', &
                   & 'command-line option.'
      stop
    end if
  else
    line = ''
  end if

  select case ( format )
  case ( 'A' )
    call DisAsmAutocoder ( showAddr, showMem, line, title )
  case ( 'D' )
    read ( *, '(a)', end=9 ) line
    call DisAsmDump ( showAddr, showMem, line, title )
  case ( '1' )
    call DisAsmOnePerCard ( showAddr, showMem, line, title )
  case ( '4' )
    call DisAsm_1440 ( showAddr, showMem, line, title, ws )
  case ( '7' )
    call DisAsmSevenPerCard ( showAddr, showMem, line, title )
  case ( 'S' )
    call DisAsmSPS ( showAddr, showMem, line, title )
  case ( 'T' )
    call DisAsmTape ( showAddr, showMem, line, title, offset )
  end select

9 continue
end program DisAsm
