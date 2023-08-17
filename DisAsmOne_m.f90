module DisAsmOne_m

! Disassemble one field.

  implicit NONE
  private

  public :: DisAsmOne

  character, parameter, public :: NoD = achar(1) ! No D modifier

contains

  subroutine DisAsmOne ( Core, Line, LineWid, W, Nop5 )

    use Get_Addr_m, only: Get_Addr
    use Op_Codes_m, only: Any, Op_Codes, Pro

    character(len=*), intent(in) :: Core  ! Input field
    character(len=*), intent(out) :: Line ! Output line
    integer, intent(out) :: LineWid       ! Width of output in line
    character, intent(out) :: NOP5        ! D modifier of 5-char NOP if /= NoD
    integer, intent(out) :: W  ! Width of field, initially len(Core)

    integer :: A, B         ! A and B addresses
    character :: D          ! D modifier if /= NoD
    logical :: Done  ! Have op code already
    integer :: Fields       ! How many fields, 0..3
    integer :: I, J         ! Subscript, loop inductor, temp
    character(len=5) :: Op  ! Op code or pseudo op
    integer :: PP           ! Position in line
    logical :: Valid        ! OP is valid, else use DCW
    integer :: XA, XB       ! Index registers for A and B addresses

    w = len(core)
    a = -100
    b = -100
    d = noD
    done = .false.
    line = ''
    nop5 = noD
    xa = 0
    xb = 0

    valid = core(1:1) /= " "

    ! Instructions that don't need terminal word mark
    if ( w > 5 ) then
      if ( core(1:1) == "B" .and. core(5:5) == " " ) w = 5
    end if
    if ( ( core(1:1) == '/' .or. core(1:1) == ',' ) .and. w > 7 ) w = 7
    if ( w > 50 ) return

    select case ( w )
    case ( 1 )
      fields = 0
    case ( 2 )
      d = core(2:2)
      fields = 1
    case ( 3 )
      op = "DSA"
      call get_addr ( core(1:3), a, xa )
      valid = a >= 0
      fields = 1
      done = .true.
    case ( 4 )
      call get_addr ( core(2:4), a, xa )
      valid = a >= 0
      fields = 1
    case ( 5 )
      valid = core(1:5) /= 'ERROR'
      if ( valid ) then
        call get_addr ( core(2:4), a, xa )
        valid = a >= 0
        d = core(5:5)
        if ( core(1:1) == "N" ) then
          fields = 1
          if ( d == " " ) then
            nop5 = d
          else
            op = "   N" // d
            done = .true.
          end if
        else
          fields = 2
        end if
      end if
    case ( 7 )
      call get_addr ( core(2:4), a, xa )
      call get_addr ( core(5:7), b, xb )
      valid = a >= 0 .and. b >= 0
      fields = 2
    case ( 8 )
      call get_addr ( core(2:4), a, xa )
      call get_addr ( core(5:7), b, xb )
      valid = a >= 0 .and. b >= 0
      d = core(8:8)
      fields = 3
    case default
      fields = 0
      valid = .false.
    end select

    if ( valid .and. .not. done ) then
      do i = 1, size(op_codes)
        if ( op_codes(i)%len /= 0 .and. op_codes(i)%len /= w ) cycle
        if ( op_codes(i)%d /= any .and. op_codes(i)%d /= d &
          & .and. d /= noD ) cycle
        if ( op_codes(i)%d /= any .and. op_codes(i)%d /= pro &
          & .and. d == noD ) cycle
        if ( op_codes(i)%a /= " " .and. op_codes(i)%a /= core(3:3) ) cycle
        if ( op_codes(i)%machineOp /= core(1:1) ) cycle
        op = op_codes(i)%op
        if ( .not. op_codes(i)%show ) then
          d = noD
          fields = fields - 1
        end if
        exit
      end do
      if ( i > size(op_codes) ) valid = .false.
      if ( valid ) op = op_codes(i)%op
    end if

    if ( valid ) then
      line(16:20) = op
      pp = 21  ! Where does the field start in line?
      j = 13   ! Where does the field start in core?
      do i = 1, fields
        if ( i > 1 ) then
          line(pp:pp) = ","
          pp = pp + 1
        end if
        if ( a >= 0 ) then
          if ( op /= "LU" .and. op /= "MU" .or. i > 1 ) then
            write ( line(pp:pp+5), '(i5)' ) a
          else
            line(pp:pp+5) = core(2:4) ! %xy
          end if
          line(pp:pp+5) = adjustl(line(pp:pp+5))
          pp = len_trim(line(1:pp+5)) + 1
          if ( xa /= 0 ) then
            line(pp:pp+1) = "&X"
            write ( line(pp+2:pp+2), "(i1)" ) xa
            pp = pp + 3
          end if
          xa = xb
          xb = 0
          j = j + 3
          a = b
          b = -1
        else if ( d /= noD ) then
          line(pp:pp) = d
          pp = pp + 1
          exit
        else ! no D
          exit
        end if
      end do
      pp = max(pp+2,50)
      line(pp:pp+7+w) = "DCW  '" // core // "'"
      pp = pp + 7 + w
    else
      line(16:20) = "DCW"
      if ( core(1:1+w-1) == '' ) then
        write ( line(22:), * ) w
        line(21:) = '#' // adjustl(line(22:))
      else
        line(21:21+w+1) = "@" // core(1:1+w-1) // "@"
      end if
      pp = 21+w+4
    end if
    lineWid = pp

  end subroutine DisAsmOne

end module DisAsmOne_m
