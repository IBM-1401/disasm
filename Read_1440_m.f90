module Read_1440_m

  ! Read a 1440 deck, after the bootstrap cards have been read.
  ! 1-3: Start address.
  ! 4-5: Count of characters to be loaded.
  ! 6-71: Data to load.  Two word separators in a row loads one word
  !       separator.  Otherwise, a word separator indicates that the
  !       next character is to get a word mark.
  ! 72-75: Card sequence number.
  ! 76-80: Deck ID.

  private
  public :: Read_1440

contains

  subroutine Read_1440 ( Undef, Core, Wm, WS, EndAddr, ExAddr, Any, EOF, LineNo )

    use Get_Addr_m, only: Get_Addr

    implicit NONE

    character, intent(in) :: Undef             ! Empty core signal
    character(16000), intent(out) :: Core, Wm  ! Core and word marks
    character, intent(in) :: WS                ! Word separator character
    integer, intent(out) :: EndAddr            ! nnn from /nnn080 in 40-46
    integer, intent(out) :: ExAddr             ! nnn from Bnnn in 68-71
    logical, intent(out) :: Any                ! Is there anything in Core?
    logical, intent(out) :: EOF                ! Last card was read
    integer, intent(inout), optional :: LineNo ! Line number counter

    integer :: A           ! Address from columns 1-3
    integer :: C           ! Count from columns 4-5
    integer :: I           ! Loop inductor
    character(80) :: Line  ! One card
    integer :: N1, N2      ! Addresses in fields of input

    any = .false.
    core = repeat(undef,16000)
    wm = ''
    endAddr = -1
    exAddr = -1
    eof = .false.

    do
      read ( *, '(a)', end=9 ) line
      if ( present(lineNo) ) lineNo = lineNo + 1
      call get_addr ( line(1:3), a ) ! Start address
      read ( line(4:5), '(i2)' ) c
      if ( line(1:7) == '00605' // ws // 'B' ) then
        call get_addr ( line(8:10), exAddr )
      else if ( line(1:7) == '00608' // ws // '/' ) then
        call get_addr ( line(8:10), endAddr )
      else
        any = .true.
        i = 6
        do while ( c > 0 )
          if ( line(i:i+1) == repeat(ws,2) ) then
            core(a:a) = ws
            i = i + 2
          else if ( line(i:i) == ws ) then
            core(a:a) = line(i+1:i+1)
            wm(a:a) = '1'
            i = i + 2
          else
            core(a:a) = line(i:i)
            i = i + 1
          end if
          c = c - 1
          a = a + 1
        end do
      end if
    end do

    return

  9 eof = .true.

  end subroutine Read_1440

end module Read_1440_m
