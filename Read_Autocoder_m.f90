module Read_Autocoder_m

  ! Read an Autocoder deck, after the bootstrap cards have been read

  private
  public :: Read_Autocoder

contains

  subroutine Read_Autocoder ( Undef, Core, Wm, EndAddr, ExAddr, Any, EOF, &
    & LineNo, FirstLine )

    use Get_Addr_m, only: Get_Addr

    implicit NONE

    character, intent(in) :: Undef             ! Empty core signal
    character(16000), intent(out) :: Core, Wm  ! Core and word marks
    integer, intent(out) :: EndAddr            ! nnn from /nnn080 in 40-46
    integer, intent(out) :: ExAddr             ! nnn from Bnnn in 68-71
    logical, intent(out) :: Any                ! Is there anything in Core?
    logical, intent(out) :: EOF                ! Last card was read
    integer, intent(inout), optional :: LineNo ! Line number counter
    character(*), intent(in), optional :: FirstLine ! In case caller already
                                               ! read it, to determine format

    integer :: I           ! Loop inductor
    character(80) :: Line  ! One card
    logical :: Need        ! Need to read
    integer :: N1, N2      ! Addresses in fields of input

    any = .false.
    core = repeat(undef,16000)
    wm = ''
    endAddr = -1
    exAddr = -1
    eof = .false.

    need = .not. present(firstLine)
    if ( .not. need ) line = firstLine

    do
      if ( need ) read ( *, '(a)', end=9 ) line
      need = .true.
      if ( present(lineNo) ) lineNo = lineNo + 1
      if ( line(40:40) == 'L' .or. line(40:40) == 'M' ) then
        if ( index(line(41:67),'###') /= 0 ) cycle
        call get_addr ( line(41:43), n1 )
        call get_addr ( line(44:46), n2 )
        if ( line(40:40) == 'L' ) then
          core(n2-n1+1:n2) = line(1:n1)
          wm(n2-n1+1:n2-n1+1) = '1'
        else
          do i = n1, 1, -1
            core(n2:n2) = line(i:i)
            if ( wm(n2:n2) == '1' ) exit
            n2 = n2 - 1
          end do
        end if
        any = .true.
        do i = 47, 67, 7
          if ( line(i:i) == ',' ) then ! Set word mark
            call get_addr ( line(i+1:i+3), n1 )
            call get_addr ( line(i+4:i+6), n2 )
            if ( n1 /= 40 ) wm(n1:n1) = '1'
            if ( n2 /= 40 ) wm(n2:n2) = '1'
          else if ( line(i:i) == ')' ) then ! Clear word mark
            call get_addr ( line(i+1:i+3), n1 )
            call get_addr ( line(i+4:i+6), n2 )
            wm(n1:n1) = ' '
            wm(n2:n2) = ' '
          else
            exit
          end if
        end do
        if ( line(68:68) == 'B' ) then ! EX card
          call get_addr ( line(69:71), exAddr )
          exit
        end if
      else if ( line(40:40) == 'N' .and. line(68:68) == 'B' ) then ! EX card
        call get_addr ( line(69:71), exAddr )
        exit
      else if ( line(40:40) == '/' .and. line(1:39) == '' ) then
        call get_addr ( line(41:43), endAddr )
        core(1:80) = repeat(undef,80)
        wm(1:80) = ''
        exit
      end if
    end do

    return

  9 eof = .true.

  end subroutine Read_Autocoder

end module Read_Autocoder_m
