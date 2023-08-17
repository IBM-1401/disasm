program DisAsmSevenPerCard_p

! Disassemble object decks in seven-fields-per-card format.

  use CommandLine_m, only: CommandLine
  use DisAsmSevenPerCard_m, only: DisAsmSevenPerCard

  implicit NONE

  character(100) :: Line ! First line of input
  logical :: ShowAddr    ! Show addresses in 6:10; set by -a option
  logical :: ShowMem     ! Show memory contents; set by -m option
  character(55) :: Title ! To put on JOB card

  ! Analyze command line
  call commandLine ( showAddr, showMem, title=title )

  read ( *, '(a)', end=9 ) line
  call disAsmSevenPerCard ( showAddr, showMem, line, title )
9 continue

end program DisAsmSevenPerCard_p
