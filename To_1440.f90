program To_1440

  ! Convert a 1401 Autocoder deck to load on a 1440 using the 1442.
  ! It can also convert a deck to load from any device that uses M%x#001R
  ! by specifying the -u option.

  ! Two formats are provided.
  ! (1) Similar to 1401 Autocoder, in that it uses LCA and SW, except
  !     columns 1-32: loader code, columns 33-71: data to be loaded,
  !     columns 72-75: serial number, 76-80: ID.
  ! (2) 1440 Autocoder, in that it uses a word separator (~ or =, depending
  !     upon whether Pierce or traditional SimH encoding is selected).
  !     columns 1-3: Start address, 4-5: count, 6-71: data,
  !     columns 77-80: serial number, 76-80: ID.
  !     A character preceded by a word separator (~ or =) is to have a word
  !     mark, unless the next one is also a word separator, in which case
  !     the character to be loaded is a word weparator without a word mark.
  !     It's impossible to load a word separator with a word mark.
  ! Neither loader clears core.

  use DumpCore_m, only: DumpCore
  use Read_Autocoder_m, only: Read_Autocoder
  use Zone_m, only: Init_Zoned, Num_to_Addr

  implicit NONE

  character(16000) :: Core, Wm ! Simulated 1401 memory

  character(80) :: B1440L(2) = (/ &
  & ',008009),016023,030037,041048,055080,080M071080M067008B001 M%G1009R   }0001     ', &
!    ....5...10...15...20...25...30...35...40...45...50...55...60...65...70...75...80
!    1      11      1      1      1      1   1      x      x                        x            
!    10...15...20...25...30...35...40...45...50...55...60...65...70...75...80
  & ')048055B001                                                            0002     ' /)
!    1      1      1      1      1      1   1
!    Lnnnmmm,......,......,......,......B001---------------------------------------

  ! This is one I found
! character(80), parameter :: B1440W(5) = (/ &
! & ',008015,022029,036058L070086,043087,050075,083054S058B075}M%G1001RB001  BOOTSTCD', &
! !  1      1      1      1      1      1      1      1   1   1                1
! & ')054050)058043L071206,206040B075   B106S201B156202~M106152B13807)  0011 LOADER01', &
! !  1      1      1      1      1      1   1                                  1
! & 'L072170,163156,152145,175179,187194B075M007202M201000)000#205141B175201~LOADER02', &
! !  1      1      1      1      1      1   1                                  1
! & 'L071137,130129,122118,114113,198200B075,004156MM200M003S206005#V075005K LOADER03', &
! !  1      1      1      1      1      1   1                                  1
! & 'L070105,099095,087083,201201,203203B083V0060061/073M%G1001R,001M199141  LOADER04' /)

  ! This is one I wrote based on Bob Feretich's card loader
  character(80) :: B1440W(6) = (/ &
  & ',008009),016023,030037,041048,055080,080M071080M067008B001 M%G1009R   }0001     ' , &
!    ....5...10...15...20...25...30...35...40...45...50...55...60...65...70...75...80
! !  1      11      1      1      1      1   1      1      1                        1
  & ')048055B001                                                            0002     ' , &
!    10...15...20...25...30...35...40...45...50...55...60...65...70...75...80
!    1      1      1      1      1   1       
  & 'L079144,113114,118122,129130B001,004156MM200M003S206005#V075005KM0072020003     ' , &
  & 'L074178,152156,163171,175138B001M201000)000#205141B175201~B106S201     0004     ' , &
  & 'L068206,187194,198200,201206B001B156202~M106152B13807)  0011           0005     ' , &
  & 'L073105,074075,083087,095099B083 }V0060061/073M%G1001R,001M199141      0006     ' /)

  ! A clear storage routine
  character(80) :: CS(2) = (/ &
  & ',008012,019,026052L072101,033037S052,044056M%G1001R}LI0?081M%G1001R CS 0001     ' , &
  & 'L066I2FL096,I0AI0H,I2?I2G/I0A099/H9I#I0D036B001101 B033,001/001I9I  CS 0002     ' /)

  logical :: Any              ! Any stuff in memory?
  character(3) :: Branch      ! Branch address for EX
  integer :: C                ! Card position
  logical :: DoCs = .false.   ! Emit CS
  logical :: Dump = .false.   ! Dump core after reading the deck if -W and -d specified
  integer :: D1               ! Next character /= Undef
  character :: Encoding = 'S' ! Encoding, which matters only for group marks and word,
                              ! separators default SimH traditional in which GM is "
                              ! and WS is =
  integer :: EndAddr          ! In columns 41-43 of card with / in 40
  logical :: EOF              ! Read_Autocoder got EOF
  integer :: ExAddr           ! In columns 69-71 of card with B in 68
  character :: FMT = 'L'      ! 'L' => format (1), 'W' => format (2)
  character :: GM = '"'       ! SimH traditional encoding for GM, '}' is Pierce encoding
  integer :: I                ! Loop inductor
  character(5) :: ID = ''     ! To put in 76-80
  integer :: J                ! Character after D1
  logical :: LeaveUndef = .false. ! Output undef as undef instead of blank
  character(80) :: Line       ! Command line argument or input line
  integer :: LineNo           ! Line number
  integer :: N                ! Number of non-word-separator characters
  character(80) :: Out        ! Output line
  character, parameter :: Undef = 'z'
  character(2) :: Unit = 'G1' ! I/O unit for, e.g., M%G1001R
  character :: WS = '='       ! SimH traditional encoding for WS, '~' is Pierce encoding

  ! Process the command line

  i = 0
  do
    i = i + 1
    call getarg ( i, line )
    if ( line(1:3) == '-c ' ) then
      doCS = .true.
    else if ( line(1:3) == '-L ' ) then
      fmt = 'L'
    else if ( line(1:3) == '-P ' ) then
      encoding = 'P'                    ! Pierce encoding for GM: }
      gm = '}'
      ws = '~'
    else if ( line(1:3) == '-S ' ) then
      encoding = 'S'                    ! Traditional SimH encoding for GM: "
      gm = '"'
      ws = '='
    else if ( line(1:3) == '-W ' ) then
      fmt = 'W'
    else if ( line(1:2) == '-u' ) then
      if ( line(3:) == '' ) then
        i = i + 1
        call getarg ( i, line(3:) )
      end if
      unit = line(3:)
    else if ( line(1:3) == '-d ' ) then
      dump = .true.
    else if ( line(1:3) == '-z ' ) then
      leaveUndef = .true.
    else if ( line == '' ) then
      exit
    else if ( line(1:1) == '-' ) then
      call getarg ( 0, line )
      print '(a)', 'Usage: '
      print '(3a)', '  ', trim(adjustl(line)), ' [options] [ID] < input > output'
      print '(a)', '  Options:'
      print '(a)', '    -c => Emit a clear storage routine first'
      print '(a)', '    -L => 1401-like loader using LCA and SW'
      print '(a)', '    -P => Encode GM and WS using Pierce encoding: } and ~'
      print '(a)', '    -S => Encode GM and WS using SimH traditional encoding: " and ='
      print '(a)', '          Default GM and WS encodings are ' // GM // ' and ' // WS
      print '(a)', '    -W => 1440-like loader using word separator'
      print '(a)', '    -u[ ]Un => Use %Un for I/O (default %G1)'
      print '(a)', '    else => this output'
      print '(a)', '  If ID is present it is punched in columns 76-80 of the output deck'
      stop
    else
      id = line(1:5)
    end if
  end do

  ! Set the unit in the stored programs
  call new_unit ( b1440L, unit )
  call new_unit ( b1440W, unit )
  call new_unit ( cs, unit )

  ! Set group marks in the stored programs
  call GM_Encoding ( b1440L, merge('}', '"', encoding=='S'), merge('}', '"', encoding=='P') )
  call GM_Encoding ( b1440W, merge('}', '"', encoding=='S'), merge('}', '"', encoding=='P') )
  call GM_Encoding ( cs, merge('}', '"', encoding=='S'), merge('}', '"', encoding=='P') )

  ! Set word separators in the stored programs
  call GM_Encoding ( b1440L, merge('~', '=', encoding=='S'), merge('~', '=', encoding=='P') )
  call GM_Encoding ( b1440W, merge('~', '=', encoding=='S'), merge('~', '=', encoding=='P') )
  call GM_Encoding ( cs, merge('~', '=', encoding=='S'), merge('~', '=', encoding=='P') )

  lineNo = 0
  if ( doCS ) then
    do i = 1, size(cs)
      lineNo = lineNo + 1
      write ( *, 2 ) cs(i)(1:71), lineNo, trim(ID)
    end do
  end if

  if ( fmt == 'W' ) then
    call init_zoned
    do
      call read_autocoder ( undef, core, wm, endAddr, exAddr, any, EOF )
      if ( dump ) call dumpCore ( core, wm )
      if ( any ) then
        ! Write the bootstrap loader
        do i = 1, size(b1440w)
          lineNo = lineNo + 1
          write ( *, '(a,t72,i4.4,a)' ) b1440w(i), lineNo, trim(ID)
        end do
        d1 = 1
      o:do
          out = ''
          do d1 = d1, 15999
            if ( core(d1:d1) /= undef .or. wm(d1:d1) /= '' ) exit
          end do
          if ( d1 > 15999 ) exit o
          c = 6
          n = 0
          call num_to_addr ( d1, 0, out(1:3) ) ! Starting address
          do
            if (d1 > 15999 .or. &
               & c > 71 .or. wm(d1:d1) /= '' .and. c > 70 ) then
              if ( out(6:71) /= '' ) then
                lineNo = lineNo + 1
                write ( *, 1 ) out(1:3), n, out(6:c), lineNo, trim(ID)
      1         format ( a,i2.2,a,t72,i4.4,a )
                cycle o
              end if
            end if
            if ( wm(d1:d1) /= '' .or. core(d1:d1) == '~' .or. core(d1:d1) == '=' ) then
              out(c:c) = ws
              c = c + 1
            end if
            n = n + 1
            if ( core(d1:d1) == undef .and. .not. leaveUndef ) core(d1:d1) = ''
            out(c:c) = core(d1:d1)
            c = c + 1
            d1 = d1 + 1
          end do
        end do o
        if ( exAddr >= 0 ) then
          out(6:) = ws // 'B'
          call num_to_addr ( exAddr, 0, out(8:10) ) ! EX address
          lineNo = lineNo + 1
          write ( *, 1 ) '006', 5, out(6:11), lineNo, trim(ID)
        else if ( endAddr >= 0 ) then
          out(6:) = ws // '/   080'
          call num_to_addr ( endAddr, 0, out(8:10) ) ! EX address
          lineNo = lineNo + 1
          write ( *, 1 ) '006', 8, out(6:14), lineNo, trim(ID)
        end if
      end if
      if ( eof ) exit
    end do
  else
    do
      ! Write the bootstrap loader
      lineNo = lineNo + 1
      write ( *, 2 ) b1440l(1), lineNo, trim(ID)
    2 format (a,t72,i4.4,a)
      lineNo = lineNo + 1
      write ( *, 2 ) b1440l(2), lineNo, trim(ID)
      do
        read ( *, '(a)', end=9 ) line
        if ( line(40:40) == 'L' ) then
          branch = '001'
          if ( line(68:68) == 'B' ) branch = line(69:71)
          out(1:28) = line(40:67)
          if ( out(5:7) == '001' ) out(5:7) = '009' ! in case 40-46 == L001001
          out(29:32) = 'B001'
          out(33:71) = line(1:39)
          read ( line(41:43), '(i3)' ) n
          write ( out(2:4), '(i3.3)' ) n + 40
          if ( line(1:1) == '"' .or. line(1:1) == '}' .or. line(1:1) == '"' ) then
            ! GM needs to be clobbered so the loader will read all of the next card
            lineNo = lineNo + 1
            write ( *, 2 ) out(1:71), lineNo, trim(ID)
            out(1:71) = 'M034041B' // branch
          else
            out(30:32) = branch
          end if
        else if ( line(40:40) == 'N' .and. line(68:68) == 'B' ) then
          out(1:71) = line(68:71)
        else if ( line(40:40) == '/' .and. line(1:39) == '' ) then
          out(1:71) = line(40:46)
        else
          cycle
        end if
        ! Replace ,040 with ,001
        do
          i = index(out(9:28),'040')
          if ( i == 0 ) exit
          out(i+8:i+10) = '001'
        end do
        lineNo = lineNo + 1
        write ( *, 2 ) out(1:71), lineNo, trim(ID)
      end do
    end do
    9 continue
  end if      

contains

  subroutine GM_Encoding ( Prog, From, To )
    character(*), intent(inout) :: Prog(:)
    character, intent(in) :: From, To
    integer :: I, J
    do i = 1, size(prog)
      do j = 1, len(prog) - 2
        if ( prog(i)(j:j) == from ) prog(i)(j:j) = to
      end do
    end do
  end subroutine GM_Encoding

  subroutine New_Unit ( Prog, Unit )
    ! Substitute Unit for the next two characters after % in Prog
    character(*), intent(inout) :: Prog(:)
    character(2), intent(in) :: Unit
    integer :: I, J
    do i = 1, size(prog)
      do j = 1, len(prog) - 2
        if ( prog(i)(j:j) == '%' ) prog(i)(j+1:j+2) = unit
      end do
    end do
  end subroutine New_Unit

end program To_1440
