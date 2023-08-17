module DisAsmSPS_m

! Disassemble object decks in SPS format.

  implicit NONE
  private

  public DisAsmSPS

contains

  subroutine DisAsmSPS ( ShowAddr, ShowMem, Line, Title )

    use DumpCore_m, only: DumpCore
    use Get_Addr_m, only: Get_Addr
    use Undump_m, only: Undump

    logical, intent(in) :: ShowAddr ! Show addresses in 6:10; set by -a option
    logical, intent(in) :: ShowMem  ! Show memory contents; set by -m option
    character(100), intent(inout) :: Line ! of input or output
    character(55), intent(in) :: Title    ! to put on JOB card

    character(16000) :: Core, Wm ! Simulated 1401 memory

    logical :: Any         ! Any stuff in memory?
    integer :: EndAddr     ! In columns 41-43 of card with / in 40
    integer :: ExAddr      ! In columns 69-71 of card with B in 68
    integer :: I           ! Subscript
    integer :: Instr4(6) = (/ 63, 67, 39, 43, 47, 51 /) ! Set WM, etc.
    integer :: J           ! Loop inductor
    integer :: N1, N2      ! Addresses in fields of input

    write ( *, '(t16,"JOB",a/t16,"CTL  6611")' ) trim('  '//title//line(76:80))

    do
      any = .false.
      core = ''
      wm = ''
      endAddr = -1
      exAddr = -1

  m:  do
        read ( *, '(a)', end=9 ) line
        if ( line(56:56) == 'L' ) then
          call get_addr ( line(57:59), n1 )
          call get_addr ( line(60:62), n2 )
          core(n2-n1+1:n2) = line(1:n1)
          wm(n2-n1+1:n2-n1+1) = '1'
          any = .true.
          do j = 1, 6
            i = instr4(j)
            if ( line(i:i) == ',' ) then ! Set word mark
              call get_addr ( line(i+1:i+3), n1 )
              wm(n1:n1) = '1'
            else if ( line(i:i) == ')' ) then ! Clear word mark
              call get_addr ( line(i+1:i+3), n1 )
              wm(n1:n1) = ' '
            else if ( line(i:i) == 'B' .and. line(i+1:i+3) /= '039' ) then
              call get_addr ( line(i+1:i+3), exAddr )
              exit m
            else
              exit
            end if
          end do
        else if ( line(56:56) == '/' .and. line(1:38) == '' ) then
          call get_addr ( line(57:59), endAddr )
          core(1:80) = ''
          wm(1:80) = ''
          exit
        end if
      end do m
      if ( showMem ) call DumpCore ( Core, WM )
      call undump ( Core, WM, showAddr, endAddr, exAddr )
    end do
  9 continue

    if ( any ) then
      if ( showMem ) call DumpCore ( Core, WM )
      call undump ( Core, WM, showAddr, endAddr, exAddr )
    end if

  end subroutine DisAsmSPS

end module DisAsmSPS_m
