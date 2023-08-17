module Get_Addr_M

  implicit NONE

contains

  subroutine Get_Addr ( A, N, X )
  ! Convert the address in A to numeric form in N, with an index
  ! register number in X.  N < 0 signifies an invalid address.

    character(3), intent(in) :: A
    integer, intent(out) :: N
    integer, intent(out), optional :: X

    character(40) :: ADDR = "0123456789'/STUVWXYZ!JKLMNOPQR?ABCDEFGHI"
    character(40) :: ADR2 = "0123456789|/STUVWXYZ!JKLMNOPQR?ABCDEFGHI"
    integer :: I, J
    integer :: d(3), Z(3)

    if ( a(1:1) == '%' ) then
      n = index(addr,a(3:3)) - 1
      if ( present(x) ) x = 0
      return
    end if

    ! Separate the digit and zone parts of each character of A
    do i = 1, 3
      j = index(addr,a(i:i)) - 1
      if ( j < 0 ) then
        j = index(adr2,a(i:i)) - 1
        if ( j < 0 ) then
          n = -1
          return
        end if
      end if
      z(i) = j / 10
      d(i) = mod(j,10)
    end do

    n = ( d(1) * 10 + d(2) ) * 10 + d(3) + 1000 * ( 4 * z(3) + z(1) )
    if ( present(x) ) x = z(2)

  end subroutine Get_Addr

end module Get_Addr_M

    
