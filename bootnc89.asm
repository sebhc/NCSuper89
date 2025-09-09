*; BOOTNC89.ASM
*; flags inverted for hdos 1=don't use, 0=use
*; 0 = Use ROM routines
*; 1 = use local stubs
USEMTR EQU     1
*; Don't use setioad
USESTAD EQU     1

IDECH0  EQU     320Q            *; BASE I/O ADDRESS FOR CHANNEL 0
IDECH1  EQU     324Q            *; BASE I/O ADDRESS FOR CHANNEL 1

*; SET DEFAULTS TO PREVENT ASM ERRORS
P8255   EQU     IDECH1
DEVMS   EQU     00H

BOOT    EQU     0000H
IOBYTE  EQU     BOOT+3
LOGDSK  EQU     BOOT+4
BDMAP   EQU     0040H
BBDF    EQU     0048H
BBDA    EQU     0049H
BBP     EQU     004AH
BUPB    EQU     004DH
BBIOS   EQU     004EH
DPEDCF  EQU     10100000B

P8255.A EQU     P8255+0         *; PORTA REG IDE DATA BUS BITS 0-7
P8255.B EQU     P8255+1         *; PORTB REG IDE DATA BUS BITS 8-15
P8255.C EQU     P8255+2         *; PORTC REG IDE BUS CONTROL BITS
P8255.N EQU     P8255+3         *; CONTROL REGISTER WRITE ONLY

*; 8255 CONTROL REGISTER TO PLACE IDE BUS IN INPUT MODE
IDE.IN  EQU     92H
*; 8255 CONTROL REGISTER TO PLACE IDE BUS IN OUTPUT MODE
IDE.OUT EQU     80H

*; IDE CONTROL BITS
IDE.A0  EQU     01H             *; IDE A0
IDE.A1  EQU     02H             *; IDE A1
IDE.A2  EQU     04H             *; IDE A2
IDE.CS0 EQU     08H             *; IDE CS0
IDE.CS1 EQU     10H             *; IDE CS1
IDE.IOW EQU     20H             *; IDE IOWR
IDE.IOR EQU     40H             *; IDE IORD
IDE.RST EQU     80H             *; IDE RESET

*; IDE REGISTERS
RG.DATA EQU     0               *; DATA REGISTER READ/WRITE
RG.ERR  EQU     1               *; ERROR REGISTER READ ONLY
RG.FEA  EQU     1               *; FEATURES REGISTER WRITE ONLY
RG.SCNT EQU     2               *; SECTOR COUNT REGISTER READ/WRITE
RG.LBL  EQU     3               *; LBA LOW BYTE READ/WRITE
RG.LBM  EQU     4               *; LBA MIDDLE BYTE READ/WRITE
RG.LBH  EQU     5               *; LBA HIGH BYTE READ/WRITE
RG.DEV  EQU     6               *; DEVICE REGISTER READ
RG.DRVH EQU     6               *; DRIVE/HEAD REGISTER WRITE ONLY
RG.STAT EQU     7               *; STATUS REGISTER READ ONLY
RG.CMD  EQU     7               *; COMMAND REGISTER WRITE ONLY

*; STATUS REGISTER BITS
ST.BSY  EQU     80H             *; BUSY
ST.DRDY EQU     40H             *; DATA READY
ST.DF   EQU     20H             *; DEVICE FAULT
ST.DRQ  EQU     08H             *; READY TO TRANSFER DATA
ST.ERR  EQU     01H             *; ERROR DETECTED, READ ERROR REGISTER

*;DEVICE REGISTER BITS
DV.LBA  EQU     40H             *; 1 SELECTS LBA MODE, 0 SELECTS CHS MODE
*; DV.DEV       EQU     10H             *; 0=DRIVE0, 1=DRIVE1

*; COMMANDS
CM.RST  EQU     08H
CM.IDNT EQU     0ECH
CM.RDSC EQU     20H
CM.WRSC EQU     30H

IDEDLY  EQU     1
IOTMOUT EQU     10000

*; bit assignments for PORTC
PC.A0   EQU     0               *; IDE BUS A0
PC.A1   EQU     1               *; IDE BUS A1
PC.A2   EQU     2               *; IDE BUS A2
*; THE FOLLOWING ARE PASSED THROUGH INVERTERS
*; WRITE A 1 TO SET LOW
PC.CS0  EQU     3               *; IDE BUS CS0-
PC.CS1  EQU     4               *; IDE BUS CS1-
PC.IOW  EQU     5               *; IDE BUS IOW-
PC.IOR  EQU     6               *; IDE BUS IOR-
PC.RST  EQU     7               *; IDE BUS RESET-

*; CDRBLOCK LOCATIONS
CD.LBAL EQU     0               *; LBA LOW BYTE
CD.LBAM EQU     1               *; LBA MID BYTE
CD.LBAH EQU     2               *; LBA HIGH BYTE
CD.NB   EQU     3               *; NUMBER OF SECTORS TO READ/WRITE
CD.DRV  EQU     4               *; CF MASTER/SLAVE SELECT, 0=MASTER

*; EXTRA DEVICE DRIVER COMMANDS
DC.GDP  EQU     100             *; GET DISK PARAMETERS
DC.RWOF EQU     101             *; RAW OFF
DC.RWON EQU     102             *; RAW ON
*; DC.MSTR      EQU     103             *; SELECT MASTER IDE DRIVE
*; DC.SLAV      EQU     104             *; SELECT SLAVE IDE DRIVE
*; DC.IDE1      EQU     105             *; SET I/O ADDRESSES FOR CHANNEL 1
*; DC.IDE2      EQU     106             *; SET I/O ADDRESSES FOR CHANNEL 2
*; END IDEDEFS

CTLFLG  EQU     2009H
CB.DIGS EQU     0FH             *; FP digit select 0-8
IOCTRL  EQU     360Q            *; control byte I/O address

MSIZE   EQU     64
BIAS    EQU     MSIZE-20*1024
CCP     EQU     1500H+BIAS      *; 3400H+BIAS
BDOS    EQU     CCP+806H
BIOS    EQU     CCP+1600H
LOADAD  EQU     CCP-80H
CPMSZ   EQU     1600H
TMPLDAD EQU     5000H   *4400H        *; temporary load adddress
*                            *; for CP/M
DPDC    EQU     177Q            *; H17 CONTROL
FD.BASE EQU     170Q            *; H37 CONTROL
FD.CON  EQU     FD.BASE

HPROMPT EQU     00D2H           *; ERROR, RETURN TO H: PROMPT
    IF  USEMTR
*; MTR90 ROM Routines
RCC     EQU     03B2H           *; READ CONSOLE CHARACTER
WCC     EQU     03C2H           *; WRITE CONSOLE CHARACTER
WCR     EQU     05F1H           *; WAIT FOR CR
WCR.    EQU     05F8H           *; WRITE CRLF TO CONSOLE
TYPMSG  EQU     0640H           *; TYPE NULL TERMINATED MESSAGE TO CONSOLE

*; H17 ROM ROUTINES
$MOVE   EQU     18AAH           *; MOVE MEMORY
$ZERO   EQU     198AH           *; ZERO MEMORY
    ENDIF
    
RAM0    EQU         00100000B   ; Set RAM only memory map
H88CTL  EQU         0F2H                ; Switch Port (Read)
DATA    EQU         2036H   ; Output to 0F2H data save (GP Port)

*; Writing GpPort
H88BSS		EQU	00000001B	; Single Step enable
H88BCK		EQU	00000010B	; 2mSec clock enable

*; INCLUDE FILES
        XTEXT   ASCII
        XTEXT   HOSDEF
        XTEXT   DIRDEF
        XTEXT   ECDEF
        XTEXT   HOSEQU
        XTEXT   DDDEF
        XTEXT   ESINT
        XTEXT   ESVAL
        XTEXT   MTR
        XTEXT   MTRRAM
        XTEXT   HROM
        
*;---------------------------------------------------------------------
*; This code is exclusively for Norberto's NCSUPER89 board
*; ORG at 042200A. The abs file will be converted to
*; a .sys file and booted from USB by Douglas' boot rom
*; Douglas suggests an org 0f 0x3000
*; Boot code MUST be at 042200 for hdos
*; reserve 10 sectors for boot
*; Less 3 for preceeding jmp

*; Boot code gets loaded at USERFWA
BOOTER  EQU     USERFWA     *;DS      10*256-3

REFIND		EQU	200AH   ; Refresh Index (0-7)
RDXFLG	EQU	2039H   ; Radix Flag
VWHLD	EQU	203AH   ; Holds address under VIEW
AUTOB	EQU	2153h   ; Auto Boot Flag
CB2.M1	EQU	00001000b	; maps full ROM (if !ORG0)
CB2.SPD	EQU	00010100b	; CPU speed control bits

*; Front Panel hardware control bits (port 360Q)
CBSSI  EQU	00010000B	; Single Step interrupt
CBMTL  EQU	00100000B	; Monitor Light
CBCLI  EQU	01000000B	; Clock interrupt enable
CBSPK  EQU	10000000B	; Speaker enable

*; Suggested ORG by the author of the 32K suer 89 ROM
*; when booting from vdip
        ORG     03000H      *;USERFWA *; 042.200  2280H

*; Clear vars
START   EQU     *
        DI
        LXI     SP,MYSTACK-1
        LXI     H,VARSTRT       *; RAM ADDRESS OF LOCAL VARS
        MVI     B,VARLEN        *; AMOUNT TO CLEAR
        CALL    $ZERO
*copy some constants
        LXI 	H,REFIND
    	LXI 	D,PRSROM    ; Source pointer for block move
    	MVI     B,7 		
INIT	LDAX    D			; Copy PRSROM from (DE)
	    MOV     M,A			;  to REFIND in (HL)
	    DCX     H			; Copy is descending
	    INX     D
	    DCR     B
	    JNZ     INIT			;  and loop if not complete
*l make sure DLAB is low
        IN      353Q
        ANI     7FH
        OUT     353Q
*; disable serial interrupts
*; We are polling
        XRA     A
        OUT     351Q
        MVI     A,'*'
        CALL    WCC
        LDA     .MFLAG
        CALL    OUTHEX
        MVI     A,'*'
        CALL    WCC
        LDA     .CTL2FL
        CALL    OUTHEX
        MVI     A,'*'
        CALL    WCC
*; make sure rom is enabled and clock disabled
*; Setup CTLFLG with everything disabled
        MVI     A,CBSSI *; disable SS
        STA     .CTLFLG
        OUT     OP.CTL
*; setup .ctl2fl SS and clk disabled, enable ROM
*;CB2.SSI EQU     00000001B               ; Single Step Interrupt
*;CB2.CLI EQU     00000010B               ; Clock Interrupt Enable
*;CB2.ORG EQU     00100000B               ; ORG 0 Select
*;CB2.SID EQU     01000000B               ; Side 1 Select
        XRA     A
        STA     .CTL2FL
        OUT     OP2.CTL
*; clear MFLAG
*UO_HLT	equ	10000000b		; Disable HALT procesing
*UO_NFR	equ	CBCLI			; No refresh of Front Panel
*UO_DDU	equ	00000010b		; Disable Display Update
*UO_CLK	equ	00000001b		; Allow private interrupt processing
        XRA     A
        STA     .MFLAG
*; copy rom to ram
        LXI     D,0
        MOV     H,D
        MOV     L,E
        LXI     B,2000H *; copy 8K
        CALL    $MOVE
*; disable rom
        LDA     .CTL2FL
    	ORI     RAM0
        STA     .CTL2FL
        OUT     OP2.CTL
*; init some stuff done by rom on start
    	XRA     A			; Set IPL Radix to Octal
	    STA	    RDXFLG
	    LXI	    H,-1			; Set view address
	    SHLD	VWHLD
	    LXI 	B,16000		; Load delay loop counter
*; delay to allow uart to settle
INI0X1  DCX     B
        MOV     A,C
        ORA     B
        JNZ     INI0X1
	    XRA     A
	    STA 	AUTOB		; Ensure autoboot disabled
	    STA 	DATA		; Set H88_CTL port contents
        OUT     DPDC            ; Turn off H17
        
        EI
*; Make sure MEM1 is low
*;        LDA     .CTL2FL
*;        ANI     0F7H               *; bit 3 is mem1h set low
*;        ANI     0DFH                *; bit 5 low enable rom
*;        STA     .CTL2FL
*;        OUT     OP2.CTL
*;        MVI     A,'B'
*;        CALL    WCC
*; copy rom to ram
*;        LXI     H,0
*;        LXI     B,4096
*;CPYLP   EQU     *
*;        MOV     A,M
*;        MOV     M,A
*;        INX     H
*;        DCX     B
*;        MOV     A,C
*;        ORA     B
*;        JNZ     CPYLP
*;        MVI     A,'C'
*;        CALL    WCC
*;        LDA     .CTL2FL
*;        ORI     020H                *; bit 5 high disable rom
*;        STA     .CTL2FL
*;        OUT     OP2.CTL
*;        EI
*;
*;      prompt user for CF channel
*;

*;GETCF   EQU     *
*;        LXI     H,MSG.CF        *; PROMPT USER FOR WHICH CF CHANNEL
*;        CALL    TYPMSG
*;GETCF1  CALL    RCC             *; READ CONSOLE CHARACTER
*;        CPI     CR              *; IS IT A <CR>?
*;        JNZ     GETCF2          *; NO, CHECK FOR 0
*;        MVI     A,'0'           *; YES, DEFAULT <CR> TO CF0
*;GETCF2  CPI     '0'             *; IS IT A 0?
*;        JNZ     GETCF3          *; NO, CHECK FOR 1
*;        MVI     B,IDECH0        *; YES, SELECT CF0
*;        JMP     GOTCF
*;GETCF3  CPI     '1'             *; IS IT A 1?
*;        JNZ     GETCF4          *; NO, MUST BE AN ERROR
*;        MVI     B,IDECH1        *; YES, SELECT CF1
*;        JMP     GOTCF
*;GETCF4  MVI     A,BELL          *; ERROR - RING THE BELL
*;        CALL    WCC
*;        JMP     GETCF1          *; TRY AGAIN

*;MSG.CF  DB      CR,LF,'BOOT CF (0,1)? <0> ',0
*;
*;GOTCF   EQU     *
*;        CALL    WCC             *; ECHO CHARACTER TO CONSOLE
*;        MOV     A,B             *; RETRIEVE CF IDE CHANNEL
*;        CALL    SETIOAD         *; SET IDE I/O ADDRESS
        IF USESTAD
        MVI     A,IDECH1        *; H89 CF/SERIAL/PARALLEL/RTC BOARD
        STA     IDECH           *; SAVE IT FOR CP/M CBIOS
        CALL    SETIOAD         *; SET IDE I/O ADDRESS
        ENDIF
*;
*;      prompt user for logical drive
*;
GETLD   EQU     *
        LXI     H,MSG.LD        *; PROMPT USER FOR LOGICAL DRIVE
        CALL    TYPMSG
GETLD1  EQU     *
        CALL    RCC             *; READ CONSOLE CHARACTER
        CPI     CR
        JNZ     GETLD2
        MVI     A,'0'           *; DEFAULT CR TO LD0
GETLD2  CPI     '0'             *; IS IT LESS THAN 0?
        JC      GETLD3          *; YES, IT'S AN ERROR
        CPI     '7'             *; NO, IS IT LESS THAN 7?
        JC      GOTLD           *; YES, WE GOT A VALID CHOICE
GETLD3  MVI     A,BELL          *; ERROR - RING BELL
        CALL    WCC
        JMP     GETLD1          *; TRY AGAIN

MSG.LD  DB      CR,LF,'BOOT CF LOGICAL DRIVE (0-6)? <0> ',0
*;MSG.LD  DB      CR,LF,'LOGICAL DRIVE (0-6)? <0> ',0

GOTLD   EQU     *
        CALL    WCC             *; ECHO CHARACTER TO CONSOLE
        ANI     07H             *; REMOVE ASCII BIAS
        STA     DRVNUM          *; STORE THE LOGICAL DRIVE NUMBER

        XRA     A
        STA     CDRBLK+CD.DRV   *; SELECT MASTER CF

*; INIT INITIALIZE 8255 AND RESET IDE BUS
IDEINIT CALL    BUSIN
        XRA     A               *; CLEAR ALL 8255 LATCHES
        CALL    O8255A
        CALL    O8255B
        CALL    O8255C
        CALL    IDRESET         *; TOGGLE IDE BUS RESET

*; WAIT FOR BUSY TO CLEAR
*; HL IS TIMEOUT
        MVI     A,RG.STAT
        CALL    SETREG
INITLP  CALL    IN8
        ANI     ST.BSY+ST.DRDY
        CPI     ST.DRDY
        JNZ     INITLP

*; CLEAR RD512
        XRA     A
        STA     RD512

BOOTCF  EQU     *
*; READ SECTOR 0

*; THIS CONTAINS SUPERBLKA SECTOR
*;        LXI     D,SECBUF
*; READ SECTOR 0 FIRST, 1 FOR LBA
*;        LXI     H,1
*;        SHLD    CDRBLK+CD.LBAL
*;        MVI     A,0
*;        STA     CDRBLK+CD.DRV
*;        STA     CDRBLK+CD.LBAH
*; JUST ONE SECTOR
*;        MVI     A,1
*;        STA     CDRBLK+CD.NB
*;        CALL    RDSEC
*; retrieve number of logical drives.
*; Stored at offset 127
*;        LDA     SECBUF+127
*;        ANA     A
*;        JNZ     *+4
*;        INR     A               *; need at least 1
*;        STA     NUMDRV
        JMP     BOOTDRV


*;---------------------------------------------------------------------
*; Boot selected logical drive
BOOTDRV EQU     *

*; READ SECTOR 0 of selected drive
*; THIS CONTAINS SUPERBLKA SECTOR
*; OFFSET 30 IS SUPERBLKA SECTOR
*; GET SUPERBLKA SECTOR
        LXI     D,SECBUF
*; READ SECTOR 0 FIRST, 1 FOR LBA
        LXI     H,1
        SHLD    CDRBLK+CD.LBAL
        XRA     A
        STA     CDRBLK+CD.DRV
        LDA     DRVNUM
        STA     CDRBLK+CD.LBAH
*; JUST ONE SECTOR
        MVI     A,1
        STA     CDRBLK+CD.NB
        XRA     A
        STA     RD512
        CALL    RDSEC
        LHLD    SECBUF+30
*; +1 FOR LBA
        INX     H
        SHLD    CDRBLK+CD.LBAL
*; READ 2 SECTORS
        LXI     D,SECBUF
        MVI     A,1
        STA     CDRBLK+CD.NB
*; read partition name sector
        CALL    RDSEC
        LHLD    CDRBLK+CD.LBAL
        INX     H
        SHLD    CDRBLK+CD.LBAL
        LXI     D,SECBUF+256
        MVI     A,1
        STA     CDRBLK+CD.NB
*; read partition offset table sector
        CALL    RDSEC
*; GET FIRST CHAR OF PARTITION NAME
        MVI     A,'P'
        CALL    WCC
        LDA     SECBUF
        CALL    WCC
        LDA     SECBUF
        CPI     'H'
        JZ      BTHDOS
        CPI     'h'
        JZ      BTHDOS
        CPI     'C'
        JZ      BTCPM
        CPI     'c'
        JZ      BTCPM
        LXI     H,MSG.UPT       *; UNKNOWN PARTITION TYPE
        CALL    TYPMSG
        JMP     HPROMPT         *; GO BACK TO THE H: PROMPT

MSG.UPT DB      'Unknown partition type',CR,LF,0

*; READ BOOT LOADER 2 tracks
*; OFFSET 5 OF SECOND SECTOR IS SECTOR FOR BOOT LOADER
*; disable interrupts before disabling ROM
BTCPM   DI
*; turn rom off
        CALL    WCR.
        MVI     A,'C'
        CALL    WCC
*; cp/m uses all mem from 0-ffff
        MVI     A,RAM0+2
        STA     DATA
        OUT     H88CTL
        STA     .CTL2FL
*; clear lower 256 bytes
        PUSH    PSW
        MVI     A,'Z'
        CALL    WCC
        LXI     H,0
        MVI     C,0
*ZPMLP   MVI     M,0
*        INX     H
*        DCR     C
*        JNZ     ZPMLP
        MVI     A,'z'
        CALL    WCC
*; CP/M CBOOT expects state of 362Q here
        POP     PSW
        STA     000DH
        LXI     SP,0FFH
        LXI     H,0F1H
        SHLD    CDRBLK+CD.LBAL
*; READ (10000H-C500H)/4 sectors
*; we throw away the last 128 bytes of last sector
*; to prevent wrap around to 0000H
        MVI     C,16        *30            *;116/4
        LXI     H,TMPLDAD-80H       *;LOADAD
RDLP    MVI     A,'R'
        CALL    WCC
        PUSH    B
        PUSH    H
        MVI     A,1
        STA     CDRBLK+CD.NB
        STA     RD512
*; read into temp buff
        LXI     D,SECBUF
        CALL    RDSEC
        POP     H
        POP     B
*; last sector?
        MVI     A,1
        CMP     C
        PUSH    B
*; assume yes so subtract 128 from sector size
        LXI     B,512-128
        JZ      RDLP2
*; copy full sector
        LXI     B,512
*; copy secbuf to (HL)
RDLP2   LXI     D,SECBUF
RDLP3   LDAX    D
        MOV     M,A
        INX     H
        INX     D
        DCX     B
        MOV     A,C
        ORA     B
        JNZ     RDLP3
        POP     B
        XCHG                    *; save H
        LHLD    CDRBLK+CD.LBAL
        INX     H
        SHLD    CDRBLK+CD.LBAL
        XCHG                    *; restore H
        DCR     C
        JNZ     RDLP
        MVI     A,'r'
        CALL    WCC
*; compute bytes to move
*;        LDA     TMPLDAD+CPMSZ+2
*;        DCR     A       *; CBOOT is >256 bytes from start
*;        MOV     B,A
*;        XRA     A
*;        SUB     B
*;        MOV     H,A
*;        MVI     L,0
*;        LXI     D,CPMSZ     *; add size of CP/M
*;        DAD     D
*; HL = number of bytes to move
*;        MOV     C,L
*;        MOV     B,H
*; BC is now bytes to move
*; determine actual BIOS address
*; get upper byte of BIOS address
        LDA     TMPLDAD+CPMSZ+2
        DCR     A       *; CBOOT is > 256 bytes
        MOV     H,A
        MVI     L,0
*; now adjust down for CCP start
        LXI     D,-CPMSZ
        DAD     D
*; HL = CCP start
*; DE = location to move from
        MVI     A,'T'
        CALL    WCC

        LXI     D,TMPLDAD
CPMMVLP EQU     *
        LDAX    D
        MOV     M,A
        INX     H
        INX     D
        MOV     A,L
        ORA     H
        JNZ     CPMMVLP
        MVI     A,'t'
        CALL    WCC


*; restore original setting

*; JUMP TO CP/M BOOT LOADER
*;        MVI     A,3             *;3
*;        MOV     C,A
*; make sure ROM is disabled
*; H8 Flag
        XRA     A
        STA     000EH
        MVI     A,DPEDCF
        STA     BBDF
        MVI     A,058H
        STA     BBDA
*        LXI     H,BDMAP
*        LXI     D,BDMP
*        MVI     B,8
*CB2     LDAX    D
*        MOV     M,A
*        INX     D
*        INX     H
*        DCR     B
*        JNZ     CB2

*; get start of BIOS
        LDA     TMPLDAD+CPMSZ+2
        DCR     A
*; HL is entry point
        MOV     H,A
        MVI     L,0
*; HL is BIOS start
        MVI     A,3
        STA     LOGDSK
        MOV     C,A
        LDA     DRVNUM
        MOV     B,A
        LDA     IDECH
*; jump to BIOS
        SHLD    TMPLDAD-2
*        JMP     *
        PUSH    PSW
        MVI     A,'G'
        CALL    WCC
        CALL    WCR.
        POP     PSW
        PCHL

*;BDMP    DB      003H,014H,025H,036H,047H,050H,061H,072H

*; READ BOOT LOADER 10 SECTORS
*; OFFSET 5 OF SECOND SECTOR IS SECTOR FOR BOOT LOADER
BTHDOS  EQU *
        EI
        CALL    WCR.
        MVI     A,'H'
        CALL    WCC
        LDA     9
        CALL    OUTHEX
        LDA     10
        CALL    OUTHEX
        LDA     11
        CALL    OUTHEX
        LHLD    SECBUF+256+5
*; +1 FOR LBA
        INX     H
        SHLD    CDRBLK+CD.LBAL
        XRA     A
        STA     RD512
        MOV     A,H
        CALL    OUTHEX
        MOV     A,L
        CALL    OUTHEX
*; READ 10 SECTORS
        MVI     B,10
        LXI     D,USERFWA
RDLPHD  EQU     *
        MVI     A,1
        STA     CDRBLK+CD.NB
        PUSH    B
        CALL    RDSEC
        POP     B
        LHLD    CDRBLK+CD.LBAL
        INX     H
        SHLD    CDRBLK+CD.LBAL
        DCR     B
        JNZ     RDLPHD

*; new version
CLOCK17 EQU     1C19H           ; H17 timer interrupt handler
BOOTA   EQU     1F5AH           ; Boot Vectors
BOOTAL  EQU     0058H           ; Length of H17 boot vectors
DLYMO   EQU     20A3H           ; H17 ROM motor timeout
DLYHS   EQU     20A4H           ; hard sector delay???
EIEXIT  EQU     1C17H
*;STACK EQU     2280H           ; Top of Stack
MFLAG   EQU     2008H           ; User option mode bits
*;CTLFLG  EQU     2009H         ; Front Panel Control Bits
TICCNT  EQU     201BH           ; 2msec clock tic counter
UIVEC   EQU     201FH           ; Start of User Interrupt Vectors
MYCNT   EQU     2152H           ; Counter for timer interrupt
AIOUNI  EQU     2131H           ; Unit number currently selected
OSID    EQU     2156H           ; Operating System ID (H67)

*;------------------------------
*; H17 RAM Addresses
*;------------------------------
DCON    EQU     2048H           ; H17 Disk constants
DRAM    EQU     20A0H           ; H17 Work RAM area
DRAML   EQU     001FH           ; Length of RAM work area
*;DPDC    EQU     07Fh            ; Disk control port

*; zero out sections of RAM
        DI
        MVI     A,'Z'
        CALL    WCC
        
	    LXI	    H,DRAM		; Clear H17 work RAM area
	    MVI 	B,DRAML		; 31 bytes
	    CALL	$ZERO
*	    OUT	    DPDC		; Turn off H17
	    STA 	TICCNT		; Zero the timer counter
	    STA 	TICCNT+1	; Zero the timer counter
	    STA 	MYCNT		; .5 sec timer = 0
        STA     DLYMO
        STA     DLYHS
*; Zero out H67 operating system info
        LXI     H,S.OSI          ; Zero out the H67 operating sys info
        MVI     B,5
        CALL    $ZERO
        
*; Set yp H17 vectors
    	MVI     A,'V'
    	CALL    WCC
        MVI     A,1         ; 7 vectors to set
	    LXI 	H,UIVEC		; Set interrupt vectors to EI/RET
BOOT2:	MVI	    M,0C3H		; Set JP instruction
	    INX     H
*; Put address of EI, RET instructions in UIVEC
*; These are in H17 ROM
	    MVI 	M,17H
	    INX 	H
	    MVI     M,1CH
    	INX 	H
	    ADD     A               ; same as shift left
	    JP	    BOOT2			; Go until bit 7 set
*; setup H17 clock interrupt handler
	    LXI 	H,CLOCK17		*; Put new interrupt routine address
	    SHLD	UIVEC+1		*;  in the 2msec clock vector address
	    
*; copy H17 constants to RAM
    	MVI     A,'C'
    	CALL    WCC
H17:    LXI     B,BOOTAL        ; Length of constants and vectors
        LXI     D,BOOTA         ; Source
        LXI     H,DCON          ; Destination
        CALL    $MOVE           ; Move it
    	MVI     A,'V'
    	CALL    WCC
        LDA     8
        CALL    OUTHEX
        LDA     9
        CALL    OUTHEX
        LDA     10
        CALL    OUTHEX
        LDA     11
        CALL    OUTHEX
    	MVI     A,'v'
    	CALL    WCC

*; enable user timer routine
	    LDA     .MFLAG
*        ORI     UO.NFR      *EQU     CB.CLI                  ; NO REFRESH OF FRONT PANEL
*        ORI     UO.DDU      *EQU     00000010B               ; DISABLE DISPLAY UPDATE
	    ORI 	UO.CLK      ; enable private clock processiny
	    STA	    .MFLAG

* enable 2ms clock
        IF 0
    	LDA     .CTL2FL		
    	ORI     CB2.CLI
    	STA     .CTL2FL
	    OUT 	OP2.CTL
	    ENDIF

* reset 2 ms
        IF 0
    	LDA     .CTLFLG
    	ORI     CBCLI
    	STA     .CTLFLG
	    OUT 	OP.CTL		; (H8 port)
	    ELSE
*; simulate 2ms tic to get things going
	    RST     1
	    ENDIF
*; enable interrupts
        IF 0
   	    MVI     A,'E'
    	CALL    WCC
    	EI
    	ENDIF
    	IF 1
    	MVI     B,4
WTTIC   EQU     *
    	LDA     TICCNT+1
    	MOV     C,A
WTTICL  EQU     *
    	LDA     TICCNT+1
    	CMP     B
    	JZ      WTTICL
    	MVI     A,'T'
    	CALL    WCC
    	DCR     B
    	JNZ     WTTIC
*    	MVI     A,'?'
*    	CALL    WCC
*    	CALL    RCC
        ENDIF
        MVI     A,'G'
        CALL    WCC
        CALL    WCR.
        XRA     A
        STA AIOUNI
        JMP     USERFWA

*; input from 8255 port A
I8255A  IN      P8255.A
        RET

*; input to 8255 port B
I8255B  IN      P8255.B
        RET

*; output to 8255 port A
O8255A  OUT     P8255.A
        RET

*; output to 8255 port B
O8255B  OUT     P8255.B
        RET

*; output to 8255 port C
O8255C  OUT     P8255.C
        RET

*; output to 8255 control
O8255CN OUT     P8255.N
        RET

*; BUSIN PLACE IDE DATA BUS INTO INPUT
BUSIN   MVI     A,IDE.IN
        JMP     O8255CN

*; BUSOUT PLACE IDE DATA BUS INTO OUTPUT
BUSOUT  MVI     A,IDE.OUT
        JMP     O8255CN

*; TOGGLE IDE RESET
IDRESET MVI     A,PC.RST*2+1    *; SET RESET BIT
        CALL    O8255CN
        MVI     A,100
IDRSTLP DCR     A
        JNZ     IDRSTLP
        MVI     A,PC.RST*2      *; CLEAR RESET BIT
        CALL    O8255CN
        MVI     A,200
RSTLP1  DCR     A
        JNZ     RSTLP1
        RET

*; SETUP IDE REGISTER ADDRESS BITS
*; A  = REGISTER
SETREG  JMP     O8255C

*; SELECT DRIVE MASTER/SLAVE
SELDRV  CALL    BUSOUT
        MVI     A,RG.DRVH
        CALL    SETREG          *; SET REG BITS
        LDA     CDRBLK+CD.DRV   *; GET SELECTED DRIVE
        ORI     DV.LBA+0A0H     *; SET LBA MODE
        CALL    OUT8
        RET

*; READ 8 BITS FROM IDE BUS.
*; A = BITS 0-7
IN8     MVI     A,PC.CS0*2+1    *; ASSERT CS0
        CALL    O8255CN
        MVI     A,PC.IOR*2+1    *; SET IOR BIT
        CALL    O8255CN
        NOP
        CALL    I8255A
        PUSH    PSW
        MVI     A,PC.IOR*2      *; CLEAR IOR BIT
        CALL    O8255CN
        MVI     A,PC.CS0*2      *; DEASSERT CS0
        CALL    O8255CN
        POP     PSW
        RET

*; WRITE 8 BITS TO IDE BUS.
*; A = BITS 0-7
OUT8    CALL    O8255A          *; OUTPUT DATA
        MVI     A,PC.CS0*2+1    *; ASSERT CS0
        CALL    O8255CN
        MVI     A,PC.IOW*2+1    *; SET IOW BIT
        CALL    O8255CN
        NOP
        MVI     A,PC.IOW*2      ;CLEAR IOW BIT
        CALL    O8255CN
        MVI     A,PC.CS0*2      ;DEASSERT CS0
        CALL    O8255CN
        RET

*; SET IDE BUS IOR LOW
*; WE WRITE A 1 TO SET THIS LOW
IDEIOR0 MVI     A,PC.CS0*2+1    *; CS0 LOW
        CALL    O8255CN
        MVI     A,PC.IOR*2+1    *; IOR LOW
        CALL    O8255CN
        RET

*; SET IDE BUS IOR HIGH
*; WE WRITE A 0 TO SET THIS HIGH
IDEIOR1 MVI     A,PC.IOR*2      *; IOR HIGH
        CALL    O8255CN
        MVI     A,PC.CS0*2      *; CS0 HIGH
        CALL    O8255CN
        RET

        IF USESTAD
*; SETUP ALL I/O ADDRESSES
*; A = BASE ADDRESS
SETIOAD STA     I8255A+1
        STA     O8255A+1
        INR     A
        STA     I8255B+1
        STA     O8255B+1
        INR     A
        STA     O8255C+1
        INR     A
        STA     O8255CN+1
        RET
        ENDIF

*; READ A SECTOR FROM DRIVE
*; 256 or 512 BYTES controlled by RD512 flag
*; RD512 == 0, read 256 bytes
*; RD512 != 0 read 512 bytes
*; CMDBLK HOLDS LBA DATA
*; DE = DEST ADDRESS
*;      DE UPDATED

RDSEC   CALL    SELDRV          *; SELECT MASTER OR SLAVE
*; WAIT FOR BUSY=O, DRDY=1
*; SELECT STATUS REG
        CALL    BUSIN           *; BUSIN
        MVI     A,RG.STAT       *; SELECT STATUS REG
        CALL    SETREG
RDSEC1  CALL    IN8             *; READ DATA
        ANI     ST.BSY+ST.DRDY
        CPI     ST.DRDY         *; JUST DRDY
        JNZ     RDSEC1
RDSEC2  CALL    BUSOUT          *; BUSOUT
        LHLD    CDRBLK+CD.LBAL
        MVI     A,RG.LBL        *; SET LBA LOW BYTE
        CALL    SETREG
        MOV     A,L
        CALL    OUT8
        MVI     A,RG.LBM        *; SET LBA MIDDLE BYTE
        CALL    SETREG
        MOV     A,H
        CALL    OUT8
        MVI     A,RG.LBH        *; SET LBA HIGH BYTE
        CALL    SETREG
        LDA     CDRBLK+CD.LBAH
        CALL    OUT8
        MVI     A,RG.SCNT
        CALL    SETREG
        LDA     CDRBLK+CD.NB    *; NUMBER OF SECTORS
        CALL    OUT8
        MVI     A,RG.CMD
        CALL    SETREG
        MVI     A,CM.RDSC       *; RD SECTOR COMMAND
        CALL    OUT8
*; WAIT FOR BUSY=O, DREQ=1
        CALL    BUSIN           *; BUSIN
        MVI     A,RG.STAT       *; SELECT STATUS REG
        CALL    SETREG
RDSEC3  CALL    IN8
        ANI     ST.BSY+ST.DRQ
        CPI     ST.DRQ          *; JUST DREQ
        JNZ     RDSEC3
        MVI     C,128           *; 128 WORDS TOTAL, FLUSH THE OTHER 128
        LDA     RD512
        ANA     A
        JZ      RDSEC4
        MVI     C,0             *; read 512 bytes
RDSEC4  MVI     A,RG.DATA       *; SELECT DATA REG
        CALL    SETREG
RDSCLP  MVI     A,PC.CS0*2+1    *; ASSERT CS0
        CALL    O8255CN
        MVI     A,PC.IOR*2+1    *; IOR LOW
        CALL    O8255CN
        CALL    I8255A          *; READ WORD
        STAX    D
        INX     D
        CALL    I8255B
        STAX    D
        INX     D
        MVI     A,PC.IOR*2      *; IOR HIGH
        CALL    O8255CN
        MVI     A,PC.CS0*2      *; DEASSERT CS0
        CALL    O8255CN
        DCR     C               *; BUMP WORD COUNT
        JNZ     RDSCLP          *; NOPE, KEEP GOING
RDFLSH  LDA     RD512
        ANA     A
        JNZ     RDSECW
        MVI     C,128           *; SECOND HALF OF SECTOR
*; JUST PULSE IOR, WE DONT NEED TO READ IT
RDFLSHL CALL    IDEIOR0         *; IOR LOW
        CALL    IDEIOR1         *; IOR HIGH
        DCR     C
        JNZ     RDFLSHL
*; WAIT FOR BUSY=O, DRDY=1
RDSECW  MVI     A,RG.STAT       *; SELECT STATUS REG
        CALL    SETREG
RDSEC5  CALL    IN8
        ANI     ST.BSY+ST.DRDY
        CPI     ST.DRDY
        JNZ     RDSEC5
RDSECX  XRA     A
        RET
*; disable this code
    IF USEMTR
    ELSE

*; stub timer interrupt during load    
MYTMR   EQU *
    RET
    
*; Monitor 89 routines missing in Douglas' ROM.
*; Maybe Douglas moved them.
*;WCC     EQU     03C2H           *; WRITE CONSOLE CHARACTER
WCC EQU     *
*; preserve interrupts state and data to write
	PUSH	PSW
WCCLP   EQU *
	IN	355Q    *; read status
	ANI	20H     *; tx empty?
	JZ	WCCLP
	POP	PSW
	OUT	350Q    *; write data
	RET

*;RCC     EQU     03B2H           *; READ CONSOLE CHARACTER
RCC EQU     *
*        DI
RCCLP   EQU     *
    	IN	355Q    *; get status
    	ANI	01H     *; bit 0 is rx rdy
    	JZ	RCCLP
    	IN	350Q    *; read data
*    	EI
    	RET
OUTHEX  EQU     *
        PUSH    PSW
        RRC
        RRC
        RRC
        RRC
        ANI     0FH
        CPI     10
        JC      *+5
        ADI     7
        ADI     '0'
        CALL    WCC
        POP     PSW
        ANI     0FH
        CPI     10
        JC      *+5
        ADI     7
        ADI     '0'
        CALL    WCC
        RET
        
*;WCR     EQU     05F1H           *; WAIT FOR CR
WCR EQU *
        CALL    RCC
        CPI     CR
        JNZ     WCR
        RET

*;WCR.    EQU     05F8H           *; WRITE CRLF TO CONSOLE
WCR.    EQU *
        MVI     A,CR
        CALL    WCC
        MVI     A,LF
        CALL    WCC
        RET
        
*;TYPMSG  EQU     0640H           *; TYPE NULL TERMINATED MESSAGE TO CONSOLE
TYPMSG EQU *
    MOV A,M
    ANA     A   *; null?
    RZ
    CALL    WCC
    INX     H
    JMP     TYPMSG

    ENDIF        

*; USE H17 ROM versions
    IF  1
*;$MOVE   EQU     18AAH           *; MOVE MEMORY
*;BC number of bytes to move
*;DE source address
*;HL destination address
$MOVE   EQU     *
        LDAX    D
        MOV     M,A
        INX     D
        INX     H
        DCX     B
        MOV     A,B
        ORA     C
        JNZ     $MOVE
        RET

*;$ZERO   EQU     198AH           *; ZERO MEMORY
*;HL=address
*;B bytes to zero
$ZERO   EQU     *
        XRA     A
$ZLP    EQU     *
        MOV     M,A
        INX     H
        DCR     B
        JNZ     $ZLP
        RET
    ENDIF
*;========================================================================================
*; PRSROM - I/O routines to be copied into RAM for use
*;========================================================================================
PRSROM	DB	01H			; REFIND (Refresh Index)
	DB	00H			; CTLFLG (Control Flag)
	DB	00H			; _MFLAG
	DB	00H			; DSPMOD (Display Mode)
	DB	00H			; DSPROT
	DB	0AH			; REGI
	DB	0C9H			; RET opcode
*;---------------------------------------------------------------------
*;
*; RAM DATA SEGMENT
*;
RAMSEG  EQU     *
VARSTRT EQU     *
RD512   DS      1               *; tells rdsec to read 512 bytes
USRIO   DS      1
IOBASE  DS      1
IDECH   DS      1               *; IDE channel I/O address to pass to CBIOS
CDRBLK  DS      6
NUMDRV  DS      1
DRVNUM  DS      1
USRDRV  DS      1
USRDIG  DS      1
DIGBUFF DS      4
VAREND  EQU     *
SECBUF  DS      256*4
        DS      256
MYSTACK DS      *
VARLEN  EQU     VAREND-VARSTRT+1
*;---------------------------------------------------------------------

        END     START
