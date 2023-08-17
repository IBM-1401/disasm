module DisasmSevenPerCard_m

! Disassemble diagnostic programs distributed in seven-fields-per-card format.

  implicit NONE
  private

  public DisAsmSevenPerCard

contains

  subroutine DisAsmSevenPerCard ( ShowAddr, ShowMem, Line, Title )

    use DumpCore_m, only: DumpCore
    use Get_Addr_m, only: Get_Addr
    use Undump_m, only: Undump

    logical, intent(in) :: ShowAddr ! Show addresses in 6:10; set by -a option
    logical, intent(in) :: ShowMem  ! Show memory contents; set by -m option
    character(100), intent(inout) :: Line ! of input or output
    character(55), intent(in) :: Title    ! to put on JOB card

    character(16000) :: Core, Wm ! Simulated 1401 memory

    integer :: EndAddr=-1  ! In columns 2-4 of card with / in 1
    integer :: I           ! Loop inductor
    integer :: N1, N2      ! Addresses in fields of input

    core = ''
    wm = ''

    ! Create a JOB card if the first card has A in column 80
    if ( title /= '' .and. line(80:80) /= 'A' ) then
      write ( *, '(15x,"JOB",a/t16,"CTL  6611")' ) trim('  '//title )
    else if ( line(80:80) == 'A' ) then
      if ( title /= '' ) then
        write ( *, '(15x,"JOB",a/t16,"CTL  6611")' ) &
          & trim('  '//title//line(73:77))
      else
        write ( *, '(15x,"JOB",a,t76,a/t16,"CTL  6611")' ) &
          & trim('  '//line(49:72)), trim(line(73:77))
      end if
    end if

    do
      read ( *, '(a)', end=9 ) line
      if ( line(1:1) == 'L' ) then
        call get_addr ( line(2:4), n1 )
        call get_addr ( line(5:7), n2 )
        core(n2-n1+33:n2) = line(33:n1)
        wm(n2-n1+33:n2-n1+33) = '1'
        do i = 8, 22, 7
          if ( line(i:i) == ',' ) then ! Set word mark
            call get_addr ( line(i+1:i+3), n1 )
            call get_addr ( line(i+4:i+6), n2 )
            if ( n1 /= 1 ) wm(n1:n1) = '1'
            if ( n2 /= 1 ) wm(n2:n2) = '1'
          else if ( line(i:i) == ')' ) then ! Clear word mark
            call get_addr ( line(i+1:i+3), n1 )
            call get_addr ( line(i+4:i+6), n2 )
            wm(n1:n1) = ' '
            wm(n2:n2) = ' '
          else
            exit
          end if
        end do
      else if ( line(1:1) == '/' ) then
        call get_addr ( line(2:4), endAddr )
        exit
      end if
    end do
  9 continue

    ! Dump core, just to make sure we got it right
    if ( showMem ) call DumpCore ( Core, WM )
    call undump ( Core, WM, showAddr, endAddr )

  end subroutine DisasmSevenPerCard

end module DisasmSevenPerCard_m
