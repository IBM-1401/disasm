program DisAsmAutocoder_p

! Disassemble object decks in Autocoder format.

  use CommandLine_m, only: CommandLine
  use DisAsmAutocoder_m, only: DisAsmAutocoder

  implicit NONE

  character(100) :: Line ! First line of input
  logical :: ShowAddr    ! Show addresses in 6:10; set by -a option
  logical :: ShowMem     ! Show memory contents; set by -m option
  character(55) :: Title ! To put on JOB card

  ! Analyze command line
  call commandLine ( showAddr, showMem, title=title )

  read ( *, '(a)', end=9 ) line
  call disAsmAutocoder ( showAddr, showMem, line, title )
9 continue

end program DisAsmAutocoder_p
