module DisAsmDump_m

! Disassemble a dump from simh.

! Input consists of two-line pairs.  On the first line of each pair, there
! is a number preceeded by a colon.  This is the address of the character
! in column 9.  From columns 9 to 58 there are 50 characters.  On the
! second line of a pair, 1's indicate word marks.

  implicit NONE
  private

  public DisAsmDump

contains

  subroutine DisAsmDump ( ShowAddr, ShowMem, Line, Title )

    use DumpCore_m, only: DumpCore
    use Undump_m, only: Undump

    logical, intent(in) :: ShowAddr ! Show addresses in 6:10; set by -a option
    logical, intent(in) :: ShowMem  ! Show memory contents; set by -m option
    character(100), intent(inout) :: Line ! of input or output
    character(55), intent(in) :: Title    ! to put on JOB card

    integer :: Addr, Colon ! From 1-8 of a core line.

    character(16000) :: Core, WM

    ! Read the dump and store it in Core and WM

    write ( *, '(t16,"JOB",a/t16,"CTL  6611")' ) trim('  '//title)

    core = ''
    wm = ''

    do
      colon = index(line(1:8),':')
      if ( colon == 0 ) then
        print *, 'No colon on core line in dump'
        stop
      end if
      read ( line(:colon-1), * ) addr
      core(addr:min(addr+49,16000)) = line(9:)
      read ( *, '(t9,a)', end=9 ) wm(addr:min(addr+49,16000))
      read ( *, '(a)', end=9 ) line
    end do
  9 continue

    ! Dump core, just to make sure we got it right
    if ( showMem ) call DumpCore ( Core, WM )
    call undump ( Core, WM, showAddr )

  end subroutine DisAsmDump

end module DisAsmDump_m
