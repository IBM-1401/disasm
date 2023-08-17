module OP_CODES_M

! Table describing op codes and pseudo-op codes.  We could put in the
! letters and numbers from BCD_TO_ASCII, but we assume they are
! mapped to the same upper-case ASCII letters and to the same numbers.

  implicit NONE
  private

  type, public :: OP_CODE_T
    character(5) :: OP        ! Columns 16-20 of input
    character :: MachineOp    ! Blank for pseudo op
    integer :: LEN            ! Instruction length, 0 => any
    character :: A            ! Second character if first is %
    character :: D            ! D-modifier if not OPT, PRO or REQ
    logical :: Show           ! "Show the D modifier"
  end type OP_CODE_T

  character, parameter, public :: Any = achar(2) ! Any D allowed
  character, parameter, public :: Pro = achar(3) ! No D allowed
  integer, private :: I ! Implied-do inductor

  type(op_code_t), public :: OP_CODES(100)

  ! DosAsmOne uses the first one it finds that matches

!   data ( op_codes(i), i = 1, size(OP_CODES) ) / &
  data ( op_codes(i), i = 1, 100 ) / &
    !           OP  MachineOp LEN   A     D,   Show
    & op_code_t("A    ", "A",  0,  " ",  pro, .true.  ), &
    & op_code_t("BLC  ", "B",  5,  " ",  "A", .false. ), &
    & op_code_t("BSS  ", "B",  5,  " ",  "B", .true.  ), &
    & op_code_t("BSS  ", "B",  5,  " ",  "C", .true.  ), &
    & op_code_t("BSS  ", "B",  5,  " ",  "D", .true.  ), &
    & op_code_t("BSS  ", "B",  5,  " ",  "E", .true.  ), &
    & op_code_t("BSS  ", "B",  5,  " ",  "F", .true.  ), &
    & op_code_t("BSS  ", "B",  5,  " ",  "G", .true.  ), &
    & op_code_t("BEF  ", "B",  5,  " ",  "K", .false. ), &
    & op_code_t("BER  ", "B",  5,  " ",  "L", .false. ), &
    & op_code_t("BPB  ", "B",  5,  " ",  "P", .false. ), &
    & op_code_t("BPCB ", "B",  5,  " ",  "R", .false. ), &
    & op_code_t("BE   ", "B",  5,  " ",  "S", .false. ), &
    & op_code_t("BL   ", "B",  5,  " ",  "T", .false. ), &
    & op_code_t("BH   ", "B",  5,  " ",  "U", .false. ), &
    & op_code_t("BAV  ", "B",  5,  " ",  "Z", .false. ), &
    & op_code_t("BC9  ", "B",  5,  " ",  "9", .false. ), &
    & op_code_t("BCV  ", "B",  5,  " ",  "@", .false. ), &
    & op_code_t("BU   ", "B",  5,  " ",  "/", .false. ), &
    & op_code_t("BCE  ", "B",  7,  " ",  any, .true.  ), &
    & op_code_t("BCE  ", "B",  8,  " ",  any, .true.  ), &
!   & op_code_t("B    ", "B",  5,  " ",  " ", .false. ), &
    & op_code_t("BIN  ", "B",  5,  " ",  any, .true.  ), &
    & op_code_t("B    ", "B",  0,  " ",  any, .true.  ), &
    & op_code_t("C    ", "C",  0,  " ",  pro, .true.  ), &
    & op_code_t("MN   ", "D",  0,  " ",  pro, .true.  ), &
    & op_code_t("MCE  ", "E",  0,  " ",  pro, .true.  ), &
    & op_code_t("CC   ", "F",  2,  " ",  any, .true.  ), &
    & op_code_t("CCB  ", "F",  5,  " ",  any, .true.  ), &
    & op_code_t("SBR  ", "H",  0,  " ",  pro, .true.  ), &
    & op_code_t("SS   ", "K",  2,  " ",  any, .true.  ), &
    & op_code_t("SSB  ", "K",  5,  " ",  any, .true.  ), &
    & op_code_t("RDTW ", "L",  8,  "D",  "R", .false. ), &
    & op_code_t("WDTW ", "L",  8,  "D",  "W", .false. ), &
    & op_code_t("RTW  ", "L",  8,  "U",  "R", .false. ), &
    & op_code_t("WTW  ", "L",  8,  "U",  "W", .false. ), &
    & op_code_t("LU   ", "L",  8,  "U",  any, .true.  ), &
    & op_code_t("LU   ", "L",  8,  "G",  any, .true.  ), & ! 1442 reader
    & op_code_t("LU   ", "L",  8,  "Y",  any, .true.  ), & ! 1443 printer
    & op_code_t("LU   ", "L",  8,  "T",  any, .true.  ), &
    & op_code_t("LCA  ", "L",  0,  " ",  pro, .true.  ), &
    & op_code_t("MBC  ", "M",  8,  " ",  "B", .false. ), &
    & op_code_t("MBD  ", "M",  8,  " ",  "D", .false. ), &
    & op_code_t("RTB  ", "M",  8,  "B",  "R", .false. ), &
    & op_code_t("WTB  ", "M",  8,  "B",  "W", .false. ), &
    & op_code_t("RD   ", "M",  8,  "D",  "R", .false. ), &
    & op_code_t("WD   ", "M",  8,  "D",  "W", .false. ), &
    & op_code_t("RT   ", "M",  8,  "U",  "R", .false. ), &
    & op_code_t("WT   ", "M",  8,  "U",  "W", .false. ), &
    & op_code_t("MU   ", "M",  8,  "U",  any, .true.  ), &
    & op_code_t("MU   ", "M",  8,  "T",  any, .true.  ), &
    & op_code_t("MCW  ", "M",  0,  " ",  pro, .true.  ), &
    & op_code_t("R    ", "M",  0,  "G",  "R", .true.  ), & ! 1442 reader
    & op_code_t("PS   ", "M",  0,  "G",  "P", .true.  ), & ! 1442 punch
    & op_code_t("P    ", "M",  0,  "G",  "G", .true.  ), & ! 1442/4 punch
    & op_code_t("PSK  ", "M",  0,  "G",  "C", .true.  ), & ! 1442 punch
    & op_code_t("W    ", "M",  0,  "Y",  "W", .true.  ), & ! 1443 printer
    & op_code_t("WS   ", "M",  0,  "Y",  "S", .true.  ), & ! 1443 printer
    & op_code_t("NOP  ", "N",  0,  " ",  any, .true.  ), &
    & op_code_t("MCM  ", "P",  0,  " ",  pro, .true.  ), &
    & op_code_t("SAR  ", "Q",  0,  " ",  pro, .true.  ), &
    & op_code_t("S    ", "S",  0,  " ",  pro, .true.  ), &
    & op_code_t("DCR  ", "U",  5,  "F",  "D", .false. ), &
    & op_code_t("ECR  ", "U",  5,  "F",  "E", .false. ), &
    & op_code_t("BSP  ", "U",  5,  "U",  "B", .false. ), &
    & op_code_t("SKP  ", "U",  5,  "U",  "E", .false. ), &
    & op_code_t("WTM  ", "U",  5,  "U",  "M", .false. ), &
    & op_code_t("RWD  ", "U",  5,  "U",  "R", .false. ), &
    & op_code_t("RWU  ", "U",  5,  "U",  "U", .false. ), &
    & op_code_t("CU   ", "U",  5,  "U",  "U", .true.  ), &
    & op_code_t("BM   ", "V",  8,  " ",  "K", .false. ), &
    & op_code_t("BW   ", "V",  8,  " ",  "1", .false. ), &
    & op_code_t("BWZ  ", "V",  0,  " ",  any, .true.  ), &
    & op_code_t("BBE  ", "W",  0,  " ",  any, .true.  ), &
    & op_code_t("MIZ  ", "X",  0,  " ",  pro, .true.  ), &
    & op_code_t("MZ   ", "Y",  0,  " ",  pro, .true.  ), &
    & op_code_t("MCS  ", "Z",  0,  " ",  pro, .true.  ), &
    & op_code_t("RCB  ", "1",  0,  " ",  "C", .false. ), &
    & op_code_t("R    ", "1",  0,  " ",  pro, .true.  ), &
    & op_code_t("WM   ", "2",  0,  " ",  ")", .false. ), &
    & op_code_t("W    ", "2",  0,  " ",  pro, .true.  ), &
    & op_code_t("WR   ", "3",  0,  " ",  pro, .true.  ), &
    & op_code_t("PCB  ", "4",  0,  " ",  "C", .false. ), &
    & op_code_t("RF   ", "4",  0,  " ",  "R", .false. ), &
    & op_code_t("P    ", "4",  0,  " ",  pro, .true.  ), &
    & op_code_t("RP   ", "5",  1,  " ",  any, .true.  ), &
    & op_code_t("RP   ", "5",  4,  " ",  any, .true.  ), &
    & op_code_t("WRF  ", "6",  0,  " ",  "R", .false. ), &
    & op_code_t("WP   ", "6",  0,  " ",  pro, .true.  ), &
    & op_code_t("WRP  ", "7",  0,  " ",  pro, .true.  ), &
    & op_code_t("SRF  ", "8",  1,  " ",  any, .true.  ), &
    & op_code_t("SPF  ", "9",  1,  " ",  any, .true.  ), &
    & op_code_t("D    ", "%",  0,  " ",  pro, .true.  ), &
    & op_code_t("M    ", "@",  0,  " ",  pro, .true.  ), &
    & op_code_t("ZA   ", "?",  0,  " ",  pro, .true.  ), &
    & op_code_t("ZS   ", "!",  0,  " ",  pro, .true.  ), &
    & op_code_t("CS   ", "/",  0,  " ",  pro, .true.  ), &
    & op_code_t("CW   ", ")",  0,  " ",  pro, .true.  ), &
    & op_code_t("H    ", ".",  0,  " ",  any, .true.  ), &
    & op_code_t("MA   ", "#",  0,  " ",  pro, .true.  ), &
    & op_code_t("SW   ", ",",  0,  " ",  pro, .true.  )  &
    & /

end module OP_CODES_M
