# Fortran compiler name:
FC=gfortran

# Options to compile one Fortran file:
FOPTS=

# Options to use the Fortran compiler to link a program:
LDOPTS=

# Extension for object files:
O = o

# Options for tar and zip:
TAROPTS=
#TAROPTS=--verbose
ZIPOPTS=-q

# Options to run demos:
RUNOPTS=-a

# Source file names
SRC= bcd_to_ascii_m.f90 DisAsm.f90 DisAsmAutocoder.f90 \
     DisAsmAutocoder_m.f90 DisAsmDump.f90 DisAsmDump_m.f90 \
     DisAsmOnePerCard.f90 DisAsmOnePerCard_m.f90 \
     DisAsmSevenPerCard.f90 DisAsmSevenPerCard_m.f90 \
     DisAsmSPS.f90 DisAsmSPS_m.f90 DisAsmTape_m.f90 DisAsm_1440_m.f90 \
     CommandLine_m.f90 DisAsmOne_m.f90 DumpCore_m.f90 \
     Get_Addr_m.f90 op_codes_m.f90 Read_Autocoder_m.f90 \
     Read_1440_m.f90 To_1440.f90 Undump_m.f90 zone_m.f90

# Files to put on tar and zip files:
DISTFILES=Makefile Manifest $(SRC) 01A6Aobject.txt \
          0300-1object.txt 0300.cd 0300.dmp 0300Object.txt

all: DisAsm DisAsmAutocoder DisAsmDump DisAsmOnePerCard DisAsmSevenPerCard \
     DisAsmSPS

DisAsm:  DisAsm.$O DisAsmAutocoder_m.$O DisAsmDump_m.$O DisAsmOne_m.$O \
         DisAsmOnePerCard_m.$O DisAsmOne_m.$O DisAsmOnePerCard_m.$O \
         DisAsmSevenPerCard_m.$O DisAsmSPS_m.$O DisAsmTape_m.$O \
         DisAsm_1440_m.$O DumpCore_m.$O CommandLine_m.$O Get_Addr_m.$O \
         op_codes_m.$O Read_Autocoder_m.$O Read_1440_m.$O Undump_m.$O
	$(FC) $(LDOPTS) -o DisAsm DisAsm.$O DisAsmAutocoder_m.$O \
              DisAsmDump_m.$O DisAsmOne_m.$O DisAsmOnePerCard_m.$O \
              DisAsmSevenPerCard_m.$O DisAsmSPS_m.$O DisAsmTape_m.$O \
              DisAsm_1440_m.$O DumpCore_m.$O CommandLine_m.$O Get_Addr_m.$O \
              op_codes_m.$O Read_Autocoder_m.$O Read_1440_m.$O Undump_m.$O

DisAsmAutocoder: DisAsmAutocoder.$O DisAsmAutocoder_m.$O \
                 CommandLine_m.$O DumpCore_m.$O \
                 Get_Addr_m.$O op_codes_m.$O Read_Autocoder_m.$O Undump_m.$O
	$(FC) $(LDOPTS) -o DisAsmAutocoder CommandLine_m.$O \
             DisAsmAutocoder.$O DisAsmAutocoder_m.$O DisAsmOne_m.$O \
             DumpCore_m.$O Get_Addr_m.$O op_codes_m.$O Read_Autocoder_m.$O \
             Undump_m.$O

DisAsmDump: DisAsmDump.$O CommandLine_m.$O DisAsmDump_m.$O DisAsmOne_m.$O \
            DumpCore_m.$O Get_Addr_m.$O op_codes_m.$O Undump_m.$O
	$(FC) $(LDOPTS) -o DisAsmDump DisAsmDump.$O CommandLine_m.$O \
             DisAsmDump_m.$O DisAsmOne_m.$O DumpCore_m.$O Get_Addr_m.$O \
             op_codes_m.$O Undump_m.$O

DisAsmOnePerCard: DisAsmOnePerCard.$O CommandLine_m.$O \
                  DisAsmOne_m.$O DisAsmOnePerCard_m.$O DumpCore_m.$O \
                  Get_Addr_m.$O op_codes_m.$O Undump_m.$O
	$(FC) $(LDOPTS) -o DisAsmOnePerCard CommandLine_m.$O \
             DisAsmOnePerCard.$O DisAsmOnePerCard_m.$O DisAsmOne_m.$O \
             DumpCore_m.$O Get_Addr_m.$O op_codes_m.$O

DisAsmSevenPerCard: DisAsmSevenPerCard.$O DisAsmSevenPerCard_m.$O \
                 CommandLine_m.$O DumpCore_m.$O \
                 Get_Addr_m.$O op_codes_m.$O Undump_m.$O
	$(FC) $(LDOPTS) -o DisAsmSevenPerCard CommandLine_m.$O \
             DisAsmSevenPerCard.$O DisAsmSevenPerCard_m.$O DisAsmOne_m.$O \
             DumpCore_m.$O Get_Addr_m.$O op_codes_m.$O Undump_m.$O

DisAsmSPS: DisAsmSPS.$O DisAsmSPS_m.$O \
                 CommandLine_m.$O DumpCore_m.$O \
                 Get_Addr_m.$O op_codes_m.$O Undump_m.$O
	$(FC) $(LDOPTS) -o DisAsmSPS CommandLine_m.$O \
             DisAsmSPS.$O DisAsmSPS_m.$O DisAsmOne_m.$O \
             DumpCore_m.$O Get_Addr_m.$O op_codes_m.$O Undump_m.$O

bcd_to_ascii_m.mod bcd_to_ascii_m.$O: bcd_to_ascii_m.f90
	$(FC) $(FOPTS) -c bcd_to_ascii_m.f90

DisAsm.$O: DisAsm.f90 commandline_m.mod \
          disasmautocoder_m.mod disasmdump_m.mod disasmonepercard_m.mod \
          disasmsevenpercard_m.mod disasmsps_m.mod disasmtape_m.mod \
          disasm_1440_m.mod read_autocoder_m.mod read_1440_m.mod
	$(FC) $(FOPTS) -c DisAsm.f90

DisAsmAutocoder.$O: DisAsmAutocoder.f90 disasmautocoder_m.mod \
                   commandline_m.mod
	$(FC) $(FOPTS) -c DisAsmAutocoder.f90

DisAsmDump.$O: DisAsmDump.f90 commandline_m.mod disasmdump_m.mod
	$(FC) $(FOPTS) -c DisAsmDump.f90

DisAsmOnePerCard.$O: DisAsmOnePerCard.f90 disasmonepercard_m.mod \
                    commandline_m.mod
	$(FC) $(FOPTS) -c DisAsmOnePerCard.f90

DisAsmSevenPerCard.$O: DisAsmSevenPerCard.f90 disasmsevenpercard_m.mod \
                   commandline_m.mod
	$(FC) $(FOPTS) -c DisAsmSevenPerCard.f90

DisAsmSPS.$O: DisAsmSPS.f90 disasmsps_m.mod \
                   commandline_m.mod
	$(FC) $(FOPTS) -c DisAsmSPS.f90

commandline_m.mod CommandLine_m.$O: CommandLine_m.f90
	$(FC) $(FOPTS) -c CommandLine_m.f90

disasmautocoder_m.mod DisAsmAutocoder_m.$O: DisAsmAutocoder_m.f90 \
                     dumpcore_m.mod read_autocoder_m.mod Undump_m.$O
	$(FC) $(FOPTS) -c DisAsmAutocoder_m.f90

disasmdump_m.mod DisAsmDump_m.$O: DisAsmDump_m.f90 \
                     dumpcore_m.mod undump_m.mod
	$(FC) $(FOPTS) -c DisAsmDump_m.f90


disasmone_m.mod DisAsmOne_m.$O: DisAsmOne_m.f90 get_addr_m.mod \
                op_codes_m.mod
	$(FC) $(FOPTS) -c DisAsmOne_m.f90

disasmonepercard_m.mod DisAsmOnePerCard_m.$O: DisAsmOnePerCard_m.f90 \
                       get_addr_m.mod op_codes_m.mod
	$(FC) $(FOPTS) -c DisAsmOnePerCard_m.f90

disasmsevenpercard_m.mod DisAsmSevenPerCard_m.$O: DisAsmSevenPerCard_m.f90 \
                         get_addr_m.mod op_codes_m.mod
	$(FC) $(FOPTS) -c DisAsmSevenPerCard_m.f90

disasmsps_m.mod DisAsmSPS_m.$O: DisAsmSPS_m.f90 get_addr_m.mod \
                op_codes_m.mod
	$(FC) $(FOPTS) -c DisAsmSPS_m.f90

disasmtape_m.mod DisAsmTape_m.$O: DisAsmTape_m.f90 get_addr_m.mod \
                op_codes_m.mod
	$(FC) $(FOPTS) -c DisAsmTape_m.f90

disasm_1440_m.mod DisAsm_1440_m.$O: DisAsm_1440_m.f90 dumpcore_m.mod \
                read_1440_m.mod undump_m.mod
	$(FC) $(FOPTS) -c DisAsm_1440_m.f90

dumpcore_m.mod DumpCore_m.$O: DumpCore_m.f90 get_addr_m.mod op_codes_m.mod
	$(FC) $(FOPTS) -c DumpCore_m.f90

get_addr_m.mod Get_Addr_m.$O: Get_Addr_m.f90
	$(FC) $(FOPTS) -c Get_Addr_m.f90

op_codes_m.mod op_codes_m.$O: op_codes_m.f90
	$(FC) $(FOPTS) -c op_codes_m.f90

read_autocoder_m.mod Read_Autocoder_m.$O: Read_Autocoder_m.f90 get_addr_m.mod
	$(FC) $(FOPTS) -c Read_Autocoder_m.f90

read_1440_m.mod Read_1440_m.$O: Read_1440_m.f90
	$(FC) $(FOPTS) -c Read_1440_m.f90

To_1440: To_1440.$O bcd_to_ascii_m.$O DumpCore_m.$O Get_Addr_m.$O \
         Read_Autocoder_m.$O zone_m.$O
	$(FC) $(FOPTS) -o To_1440 To_1440.$O \
          bcd_to_ascii_m.$O DumpCore_m.$O Get_Addr_m.$O Read_Autocoder_m.$O \
          zone_m.$O

To_1440.$O: To_1440.f90 read_autocoder_m.mod dumpcore_m.mod get_addr_m.mod \
            zone_m.mod
	$(FC) $(FOPTS) -c To_1440.f90

undump_m.mod Undump_m.$O: Undump_m.f90 disasmone_m.mod
	$(FC) $(FOPTS) -c Undump_m.f90

zone_m.mod zone_m.$O: zone_m.f90 bcd_to_ascii_m.mod
	$(FC) $(FOPTS) -c zone_m.f90

clean:
	rm -f *.$O *.mod DisAsm DisAsmAutocoder DisAsmDump \
            DisAsmOnePerCard DisAsmSevenPerCard DisAsmSPS

tar: $(DISTFILES)
	tar $(TAROPTS) -c -h -z -f Disasm.tgz $(DISTFILES)

zip: $(DISTFILES)
	zip $(ZIPOPTS) Disasm.zip $(DISTFILES)

demo: all
	./DisAsmAutocoder $(RUNOPTS) < 0300.cd
	./DisAsmDump $(RUNOPTS) < 0300.dmp
	./DisAsmOnePerCard $(RUNOPTS) < 0300-1object.txt
	./DisAsmSevenPerCard $(RUNOPTS) < 0300Object.txt
	./DisAsmSPS $(RUNOPTS) < 01A6Aobject.txt
	./DisAsm $(RUNOPTS) < 0300.cd
	./DisAsm $(RUNOPTS) < 0300.dmp
	./DisAsm $(RUNOPTS) < 0300-1object.txt
	./DisAsm $(RUNOPTS) < 0300Object.txt
	./DisAsm $(RUNOPTS) < 01A6Aobject.txt
