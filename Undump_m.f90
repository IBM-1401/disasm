module Undump_m

! Undump an IBM 1401 memory dump to Autocoder format

  implicit NONE
  private

  public :: Undump

contains

  subroutine Undump ( Core, WM, ShowAddr, EndAddr, ExAddr )

    use DisAsmOne_m, only: DisAsmOne
    use DumpCore_m, only: Undef

    character(16000), intent(in) :: Core, WM
    logical, intent(in) :: ShowAddr ! Put addresses in 1:5 of output
    integer, intent(in), optional :: EndAddr ! To put on END card
    integer, intent(in), optional :: ExAddr  ! To put on EX card

    logical :: Done        ! No more to do
    integer :: L, LP       ! Subscript, loop inductor
    character(100) :: Line ! of output
    integer :: LW          ! Line width from DisAsmOne
    integer :: MyEndAddr, MyExAddr ! Local ones
    integer :: N           ! Next word mark, else highest non-blank
    character :: Nop5      ! D-modifier of NOP if not NoD
    integer :: W           ! Field width

    line = ''
    l = 1
    lp = -100
    w = -100
    done = .false.

    do while ( .not. done )

      ! Find next defined or word mark.
      do n = l, 16000
        if ( core(n:n) /= undef .or. wm(n:n) /= '' ) exit
      end do
      if ( n > 16000 ) exit

      if ( n /= l .or. n /= lp + w ) then
        write ( *, '(15x,"ORG  ",i0)' ) n
        l = n
      end if

      if ( core(n:n) == undef ) then ! must have wm
        line(16:23) = 'DA   1X1'
        if ( showAddr ) write ( line(1:5), '(i5)' ) n
        write ( *, '(a)' ) line(:23)
        l = n + 1
        cycle
      end if

      l = n

      ! Find next WM or undefined, determine width
      do n = l+1, 16000
        if ( core(n:n) == undef .or. wm(n:n) /= '' ) exit
      end do
      done = n > 16000
      w = n - l

      if ( wm(l:l) == '' ) then
        line(16:20) = 'DC'
        call DC_or_DCW
      else
        if ( w <= 50 ) then
          call disAsmOne ( core(l:n-1), line, lw, w, nop5 )
          if ( showAddr ) then
            if ( line(16:20) == 'DCW' .or. line(16:20) == 'DSA' ) then
              write ( line(1:5), '(i5)' ) l+w-1
            else
              write ( line(1:5), '(i5)' ) l
            end if
          end if
          write ( *, '(a)' ) trim(line(:lw))
        else ! Long field, break into DCW and DC
          line(16:20) = 'DCW'
          call DC_or_DCW
        end if
      end if

      lp = l
      l = n

    end do

    myEndAddr = -1
    if ( present(endAddr) ) myEndAddr = endAddr
    myExAddr = -1
    if ( present(exAddr) ) myExAddr = exAddr

    if ( myExAddr >= 0 ) write ( *, '(15x,"EX   ",i0)' ) myExAddr

    if ( myEndAddr >= 0 ) then
      write ( *, '(15x,"END  ",i0)' ) myEndAddr
    else if ( .not. present(exAddr) ) then
      write ( *, '(t16,"END")' )
    end if

  contains

    subroutine DC_or_DCW
      do while ( w > 0 )
        if ( core(l:l+w-1) /= '' .or. wm(l:l+w-1) /= '' ) then
          lw = min(w,50)
          if ( showAddr ) write ( line(1:5), '(i5)' ) l + lw - 1
          line(21:) = '@' // core(l:l+lw-1) // '@'
          write ( *, '(a)' ) trim(line)
          line(18:18) = '' ! Change OP to DC for remainder of field
          l = l + lw
          w = w - lw
        else if ( done ) then
          if ( line(18:18) == '' ) exit ! DC now?
          if ( showAddr ) write ( line(1:5), '(i5)' ) l
          line(21:) = '#1'
          write ( *, '(a)' ) trim(line)
          exit
        else
          l = l + w
          lp = -100 ! Force top of loop to make ORG
          exit
        end if
      end do
    end subroutine DC_or_DCW

  end subroutine Undump

end module Undump_m
