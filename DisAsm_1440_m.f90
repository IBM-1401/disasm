module DisAsm_1440_m

! Disassemble object decks in _1440 format.

  implicit NONE
  private

  public DisAsm_1440

contains

  subroutine DisAsm_1440 ( ShowAddr, ShowMem, Line, Title, WS )

    use DumpCore_m, only: DumpCore, Undef
    use Read_1440_m, only: Read_1440
    use Undump_m, only: Undump

    logical, intent(in) :: ShowAddr ! Show addresses in 6:10; set by -a option
    logical, intent(in) :: ShowMem  ! Show memory contents; set by -m option
    character(100), intent(inout) :: Line ! of input or output
    character(55), intent(in) :: Title    ! to put on JOB card
    character, intent(in) :: WS     ! Word separator character

    character(16000) :: Core, Wm ! Simulated 1401 memory

    logical :: Any         ! Any stuff in memory?
    integer :: EndAddr     ! In columns 41-43 of card with / in 40
    logical :: EOF         ! Read__1440 got EOF
    integer :: ExAddr      ! In columns 69-71 of card with B in 68

    line(21:75) = title
    write ( *, '(t16,"JOB",a/t16,"CTL  6611")' ) trim('  '//line(21:80))

    do
      call read_1440 ( undef, core, wm, ws, endAddr, exAddr, any, EOF )
      if ( any ) then
        if ( showMem ) call DumpCore ( Core, WM )
        call undump ( Core, WM, showAddr, endAddr, exAddr )
      end if
      if ( EOF ) exit
    end do

  end subroutine DisAsm_1440

end module DisAsm_1440_m
