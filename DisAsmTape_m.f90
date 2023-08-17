module DisAsmTape_m

! Disassemble a Tape dump.

! Input consists of two-line pairs.  On the first line of each pair, there
! is a number preceeded by a colon.  This is the address of the character
! in column 9.  From columns 9 to 58 there are 50 characters.  On the
! second line of a pair, 1's indicate word marks.

  implicit NONE
  private

  public DisAsmTape

contains

  subroutine DisAsmTape ( ShowAddr, ShowMem, Line, Title, Offset )

    use DumpCore_m, only: DumpCore, Undef
    use UnDump_m, only: UnDump

    logical, intent(in) :: ShowAddr ! Show addresses in 6:10; set by -a option
    logical, intent(in) :: ShowMem  ! Show memory contents; set by -m option
    character(*), intent(inout) :: Line ! of input or output
    character(55), intent(in) :: Title    ! to put on JOB card
    integer, intent(in) :: Offset   ! to add to addresses

    integer :: Addr ! From 1-8 of a core line.

    character(16000) :: Core, WM
    integer :: I, W

    ! Read the Tape and store it in Core and WM

    write ( *, '(t16,"JOB",a/t16,"CTL  6611")' ) trim('  '//title)

    ! Initialize
    do i = 1, 16000
      core(i:i) = undef
    end do
    wm = ''
    addr = 0

    ! Find first line with colon in 1:8
    do while ( line(6:6) /= ':' )
      read ( *, '(a)', end=9 ) line
    end do
    ! Read the dump
    do
      if ( line(6:6) /= ':' ) then
        print *, 'No colon on core line in tape dump'
        print *, trim(line)
        stop
      end if
      read ( line(:5), * ) addr
      addr = addr + offset
      ! Determine line width
      do w = 99, 0, -1
        if ( line(8+w:8+w) /= '' ) exit
      end do
      w = min(w,len(line)-8)
      do i = 8, 8+w
        if ( line(i:i) == '^' ) line(i:i) = ''
      end do
      core(addr:min(addr+w,16000)) = line(8:)
      read ( *, '(t8,a)', end=9 ) wm(addr:min(addr+w,16000))
      addr = min(addr+w,16000)
      read ( *, '(a)', end=9 ) line
    end do
  9 continue

    ! Intermediate spaces cannot be undefined; must have been
    ! rows not dumped because they're blank.
    do i = offset+1, addr
      if ( core(i:i) == undef ) core(i:i) = ''
    end do

    ! Insert GM after last defined character.
    core(addr+1:addr+1) = '}'
    wm(addr+1:addr+1) = '1'

    ! Dump core, just to make sure we got it right
    if ( showMem ) call dumpCore ( Core, WM )
    call unDump ( Core, WM, showAddr )

  end subroutine DisAsmTape

end module DisAsmTape_m
