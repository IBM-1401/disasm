module DisasmOnePerCard_m

! Disassemble one-per-card 1401 machine code

! The program load cards begin L031...1001.  The length of each field
! is determined by the difference between the B fields of consecutive
! load cards.

  implicit NONE
  private

  public DisAsmOnePerCard

contains

    subroutine DisAsmOnePerCard ( ShowAddr, ShowMem, Line2, Title )

    use DisAsmOne_m, only: DisAsmOne, NoD
    use DumpCore_m, only: DumpCore
    use Get_Addr_m, only: Get_Addr

    logical, intent(in) :: ShowAddr ! Show addresses in 6:10; set by -a option
    logical, intent(in) :: ShowMem  ! Show memory contents; set by -m option
    character(100), intent(inout) :: Line2 ! of input or output
    character(55), intent(in) :: Title    ! to put on JOB card

    integer :: L, LP  ! Beginning of field, N1 - 19, previous value
    character(80) :: Line1
    character(16000) :: Mem, WM ! Memory contents, Word marks
    character :: NOP5 ! 5-character NOP if /= NoD
    integer :: N1, N2 ! addresses in B fields of Line1, Line2
    character(100) :: P ! Line to print
    integer :: PP     ! Print position
    integer :: W, WP  ! Field width, previous width

    mem = ''
    wm = ''

    l = -1
    w = -1

    if ( showMem ) then
      ! Fill in what preliminaries do
      mem(333:368) = ",008012,00110011B361080AB421/340080 "
      wm (333:368) = "1      1   1   11       1   1      1"
      mem(1200:1245) = "2,049L0772772/2772)/40/60)/80L/992802/2802413 "
      wm (1200:1245) = "11   1      11   11      1   1      11   1   1"

      ! Fill in what postamble does
      mem(1:46) = ',019027,031,038042B031T98"B400L046352BW04BS88 '
      wm (1:46) = '1      1   1      1       1   1      1   1    '
    end if

    if ( title /= '' ) then
      write ( *, '(15x,"JOB",a)' ) trim('  '//title//line2(73:77))
    else
      write ( *, '(15x,"JOB",a,t76,a/t16,"CTL  6611")' ) &
        & trim('  '//line2(49:72)), trim(line2(73:77))
    end if

    ! Skip preliminaries

    do while ( line2(1:4) /= 'L031' )
      read ( *, '(a)' ) line2
    end do

    call get_addr ( line2(5:7), n2 )

    do
      lp = l
      wp = w
      line1 = line2
      if ( line1(1:4) /= 'L031' .and. &
           line1(1:4) /= 'L012' ) exit ! end of the load cards
      read ( *, '(a)' ) line2
      n1 = n2
      call get_addr ( line2(5:7), n2 )
      if ( line2(1:4) == 'L031' ) then
        w = min(n2 - n1, 20)
        if ( w < 0 ) w = 20 ! Load address decreased -- can't compute w
        l = n1 - 19
      else if ( line2(1:4) == 'L012' ) then
        w = min(n2 - n1 + 19, 20)
        l = n1 - 19
      else if ( line1(1:4) == 'L012' ) then
        w = 1
        l = n1
      else
        w = 20
        l = n1 - 19
      end if

      if ( l /= lp + wp ) write ( *, '(15x,"ORG  ",i0)' ) l

      mem(l:l+w-1) = line1(12:12+w-1)
      wm(l:l) = "1"

      call DisAsmOne ( line1(12:12+w-1), p, pp, w, nop5 )
      p(max(35,pp):) = line1(32:72)
      if ( showAddr ) then
        if ( p(16:20) == 'DCW' .or. p(16:20) == 'DSA' ) then
          write ( p(1:5), '(i5)' ) l+w-1
        else
          write ( p(1:5), '(i5)' ) l
        end if
      end if

      print '(a)', trim(p(:81))

      if ( nop5 /= noD ) then
        if ( showAddr ) write ( p(1:5), '(i5)' ) l + 4
        p(16:23) = "DC   @ @"
        print '(a)', p(:32)
      end if

    end do

    write ( *, '(15x,"END")' )

    if ( showMem ) call DumpCore ( mem, wm )

  end subroutine DisasmOnePerCard

end module DisasmOnePerCard_m
