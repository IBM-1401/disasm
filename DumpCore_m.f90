module DumpCore_m

  implicit NONE
  private

  public :: DumpCore, Undef

  character, parameter :: Undef = 'z' ! a non-BCD character, indicating
                                      ! an undefined character.

contains

  subroutine DumpCore ( Core, WM )
  ! Dump simulated 1401 core memory

    character(16000), intent(in) :: Core, Wm
    logical :: Dump
    integer :: I, L        ! Subscript and loop inductor
    character(100) :: Line ! of output

    do l = 1, 100, 5
      write ( line(l:l+4), "(i5)" ) l+4
    end do
    do l = 1, 100
      if ( line(l:l) == " " ) line(l:l) = "."
    end do
    print "(/7x,a)", line

    do l = 1, 16000, 100
      dump = .false.
      do i = 0, 99
        dump = core(l+i:l+i) /= '' .and. core(l+i:l+i) /= undef .or. &
          & wm(l+i:l+i) /= ''
        if ( dump ) exit
      end do
      if ( dump ) then
        print "(i5,': ',a)", l, trim(core(l:l+99))
        print "(7x,a)", trim(wm(l:l+99))
      end if
    end do
    print *

  end subroutine DumpCore

end module DumpCore_m
