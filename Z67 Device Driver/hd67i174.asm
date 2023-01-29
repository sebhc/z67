***     INIT67  -  HD67 Initialization Parameters

***     Assembly Constants
*

*       H67 Physical characteristics
	XTEXT	ASCII
	XTEXT	BOODEF
	XTEXT	DDDEF
	XTEXT	DDFDEF
	XTEXT	DIRDEF
	XTEXT	ECDEF
	XTEXT	H67DEF
	XTEXT	INIDEF
	XTEXT	HOSDEF
	XTEXT	LABDEF
	XTEXT	PICDEF
	XTEXT	HOSEQU
	XTEXT	ESINT
	XTEXT	MTR
	XTEXT	ESVAL
	XTEXT	HROM
	
	
NSPTS	EQU	40		Sectors/Track
NBPS	EQU	256		Bytes/Sector

DC.GDP	EQU	100		Get Disk Parameters

MI.JMP	EQU	303Q		8080 Jump Instruction
MI.SBCB	EQU	102355A		Z80 instruction: HL <- HL - BC - Carry
MI.SBCD	EQU	122355A		Z80 instruction: HL <- HL - DE - Carry

IOPORT	EQU	274Q		Base I/O port

	ORG     USERFWA

	CODE	P,SB.BOO
	CODE	+R
	
START   JMP     PBOOT

.	SET	42203A
	ERRNZ	*-.
	DS	SB.BPE-.

*	Set up system driver call vector

PBOOT   MVI     A,MI.JMP
	STA     SYDD
	LXI     H,RODD
	SHLD    SYDD+1
	
	LXI     SP,USERFWA
	LDA     UNK55
	STA     AIO.UNI

*	Don't relocate, INIT is fixed 

	DB	MI.JMP
	DW	44200A		INIT
	
***     RODD  -  Read-only Device Driver
*
*       RODD is the read only device driver, which
*       handles requests from the boot code and the
*       HDOS boot routines
*
*       ENTRY:  A       = Function Code
*
*          if request is a READ
*               BC      = Sector Count
*               DE      = Destination Address
*               HL      = Starting Sector Number
*
*          if request is a MOUNT
*
*               if L != 0
*                       Already know info
*               else
*                       L       = Volume number
*
*       EXIT:   PSW     'C' Set if error
*                         A = Error Code
*                       'C' Clear if no error
*
*       USES:   All
*
RODD	CPI	DC.MOU+1
	CMC
	RC
	
	CALL    $TBRA
RODDA   EQU     *

	ERRNZ   *-RODDA-DC.REA
	DB	HDREAD-*
	DB	ERROR-*
	ERRNZ	*-RODDA-DC.RER
	DB	HDREAR-*
	DB	ERROR-*
	DB	ERROR-*
	DB	ERROR-*
	DB	ERROR-*
	ERRNZ	*-RODDA-DC.ABT
	DB	HDABT-*
	ERRNZ	*-RODDA-DC.MOU
	DB	HDMOU-*
ERROR   STC
	RET
	
***     READ  -  Process a Read Request
*
*       HDREAD is responsible for handling read requests
*
*       ENTRY/EXIT/USAGE conditions:
*
*               BC      = Byte Count
*               DE      = Destination Address
*               HL      = Starting Sector Number
**
HDREAD	MOV     A,B
	ORA     C
	JZ      RTN25		Byte count 0, done!

	PUSH    H		save starting no.
	MOV     H,B
	MOV     L,C		HL = byte count
	SHLD    RDPTR		save it
	LXI     B,255		round up to even sector count
	DAD     B
	MOV     B,H
	MVI     C,0
	POP     H
	CALL    ZEROCDR
	MOV     A,B		No sectors
	STA     CDRBLK+4	save in CDRBLK
	CALL    RTN28
	JC      RTN25		exit
	MVI     A,D.REA		Read command
	STA     CDRBLK
	CALL    OUTCOM		Send the command
	
RTN23   IN      IOPORT+RI.BST	Input bus status
	MOV     C,A		save the byte
	ANI     BS.REQ		Look for REQ
	JZ      RTN23		else loop...
	MOV     A,C		restore fetched byte
	ANI     BS.COM		check for COM		
	JNZ     RTN24		if COM present must be completion status
	IN      IOPORT+RI.DAT	input data from controller
	ANA     A		clear flags
	LHLD    RDPTR
	LXI     B,1
	
	DW	MI.SBCB		HL <- HL - BC - Carry (z80)

	JC      RTN23
	SHLD    RDPTR
	STAX    D
	INX     D
	JMP     RTN23

RTN24   CALL    RTN35
RTN25   RET


***     HDREAR  -  Read Regardless
*
*	Read regardless is handled the same as Read
*	on the H67 Hardware
*
HDREAR	EQU	HDREAD

***     ABORT  -  Process ABORT Request
*
HDABT   XRA     A
	RET

***     MOUNT  -  Process MOUNT Request
*
HDMOU	XRA	A
	RET


RTN28   PUSH    D
	XCHG			DE = HL
	LHLD    UNK56		load offset
UNK29   DAD     D		add it
	XCHG
	LXI     H,CDRBLK+2	address 1
	MOV     M,D
	INX     H
	MOV     M,E
	POP     D
	RET
*
*	Output bytes
*
OUTCOM	PUSH    B
	PUSH    H
*
*	Get controller's attention
*	
GETCON	IN	IOPORT+RI.BST		Wait until not BUSY
	ANI     BS.BSY
	JNZ     GETCON
	
	MVI     A,BC.SEL		Assert SEL and DATA0
	OUT     IOPORT+RI.CON
	
CBUSY   IN      IOPORT+RI.BST		Again, wait until not BUSY
	ANI     BS.BSY
	JZ      CBUSY
	
	MVI     A,BC.EDT		Allow data enable
	OUT     IOPORT+RI.CON
*
*	Output commands
*
	LXI     H,CDRBLK		Load pointer to command queue
COMREQ	IN      IOPORT+RI.BST		Sample bus status
	ANI     BS.REQ+BS.OUT+BS.COM	Wait for conditions
	JP      COMREQ
	CPI     BS.REQ+BS.OUT+BS.COM
	JNZ     OUTCOM1			Controller sending data - leave now
	MOV     A,M			fetch byte from command queue
	OUT     IOPORT+RI.DAT		Output the byte
	INX     H			point to next
	JMP     COMREQ			and continue

OUTCOM1	XRA     A
	POP     H
	POP     B
	RET


RTN35   PUSH    B
	PUSH    H
	IN      IOPORT+RI.DAT		Read data
	MOV     C,A			save in C
RTN35A	IN      IOPORT+RI.BST		Read status
	ANI     BS.REQ
	JZ      RTN35A			Loop 'til no REQ
	MOV     A,B
	IN      IOPORT+RI.DAT
	ANA     A
	JNZ     RTN35B
	MOV     A,B
	ANI     00000010B
	JNZ     RTN35B
	MOV     A,C
	ANI     00000011B
	JZ      RTN35C
RTN35B	MVI     A,BC.RST		reset
	OUT     IOPORT+RI.CON
	XRA     A
	OUT     IOPORT+RI.CON
	STC
RTN35C	POP     H
	POP     B
	RET
*
*	Zero command descriptor block
*
ZEROCDR	PUSH    B
	PUSH    D
	PUSH    H
	PUSH    PSW
	MVI     B,6
	LXI     H,CDRBLK
	CALL    $ZERO
	POP     PSW
	POP     H
	POP     D
	POP     B
	RET

RDPTR   DW	0		pointer used by READ operation

*
*	Command Descriptor Block (Class 0 commands)
*
CDRBLK	DB	0		000 | opcode
	DB	0		LUN | log addr2
	DB	0		log addr 1
	DB	0		log addr 0
	DB	0		num blocks
	DB	0		control
*
*	Fill up to 512 bytes
*
	CODE	-R
.	SET	*
	CODE	+R
*	ERRMI	SB.SDB-.
	DS	SB.SDB-.-3
*
*	Data
*
UNK55   DB	0
UNK56	DW	0
	
***     INIT
*
*       INIT Handles request made by the
*       *INIT* routine during disk initialization
*
	ERRNZ	*-SB.SDB

INIT	CPI	INI.MAX
	CMC
	RC

	CALL	$$TBRA
INITA	EQU	*
	ERRNZ	*-INITA-INI.CMV
	DB	CMV-*

	ERRNZ	*-INITA-INI.IDS
	DB	IDS-*

	ERRNZ	*-INITA-INI.DBI
	DB	DBI-*

	ERRNZ	*-INITA-INI.PAR
	DB	PAR-*

	ERRNZ	*-INITA-INI.MAX

	
***     CMV  -  Check Media Validity
*
*       CMV Checks the disk to determine
*       write protect status
*
*

CMV	LXI	B,0		0 bytes
	CALL	$$DRVR
	DB	DC.WRI		Try to write
	JNC	CMV2		OK, return
	
	CPI     EC.WP		Write protected?
	JNZ     CMV1		No, flag error and return
	
	CALL    $$TYPTX
	DB	LF,BELL
	DB	'This volume is write protected',LF+200Q

CMV1	STC
CMV2	RET

***     DBI  -  Directory Block Interleave
*
*       DBI is responsible for returning a pointer
*       to the directory block interleave table.
*       Note that the table is relative, i.e. Each
*       entry is an offset from the previous entry.
*
DBI	LXI	H,DBIA
	ANA	A
	RET
	
***     PAR  -  Volume Parameters
*
*       PAR returns a pointer to the volume parameters
*       which include volume size, sides, sector size, etc.
*
*       Note that these cells are valid only AFTER the
*       IDS routine has successfully completed.
*
PAR	LXI	H,PARAM
	ANA	A
	RET
	
***     IDS  -  Initialize disk surface
*
*       IDS formats the surface of the disk, setting
*       up the cells which determine volume size, sidedness
*       etc.
*
IDS	CALL	$$DRVR
	DB	DC.GDP		Get Disk Parameters
	
	JC      IDS6		exit if error
	
	INX     H		skip ID byte
	MOV     C,M
	INX     H
	MOV     B,M		BC = Lo/Mid of starting triple
	INX     H
	INX     H		skip Lo byte
	MOV     E,M
	INX     H
	MOV     D,M		DE = Lo/Mid of ending triple
	XCHG			HL = 	"
	XRA     A		clear 'c'
	
*	compute the size (End - Start)
	DW	MI.SBCB		HL <- HL - BC - Carry (z80)

	MOV     A,L
	ANA     A
	JNZ     IDS1
	DCX     H
IDS1	SHLD    VOLSIZ
*
*	Check if OK to clear
*
IDS2	CALL    $$TYPTX
	DB	LF,'Clear the entire disk partition? <YES> '
	DB	200Q
	CALL    $$ITL.		get answer
	JC      IDS2
	MOV     A,M
	ANA     A		Null line?
	JZ      IDS3		default ("Yes")
	CALL    $$CYS		'YES' ?
	JZ      IDS3		Yes
	
	CALL    $$CNO		'NO'
	JNZ     IDS2		neither, try again
	JMP     IDS5		NOT OK to clear, exit
*
*	OK to clear entire disk partition
*
IDS3	CALL    $$TYPTX
	DB	LF,LF
	DB	'Clearing sector number: '
	DB	ESC,'j',200Q		H19 Save Cursor Position
	
	LXI     D,0
IDS4	XCHG
	PUSH    B
	PUSH    D
	PUSH    H
	PUSH    PSW
	PUSH    H
	POP     B		Value
	MVI     A,5		5 digits
	LXI     H,DBUFF		where to put it
	PUSH    H
	CALL    $UDD		unpack it
	POP     H
	CALL    ZBLANK		Blank any leading zeros
	CNC     ZBLANK
	CNC     ZBLANK
	CNC     ZBLANK
	LXI     H,DMESG		and print it (at saved location)
	SCALL   .PRINT		

	POP     PSW
	POP     H
	POP     D
	POP     B

	PUSH    H
	LXI     B,256		1 sector
	LXI     D,DBIA
	CALL    $$DRVR
	DB	DC.WRI
	JC	IDS6

	POP     H

	INX     H
	XCHG
	LHLD    VOLSIZ
	XRA     A

	DW	MI.SBCD		HL <- HL - DE - Carry
	
	JNZ     IDS4		keep looping...
IDS5	XRA     A
IDS6	RET
*
*	ZBLANK - convert zero to blank
*
ZBLANK	MOV     A,M
	CPI     '0'
	JNZ     ZBLNK1
	MVI     M,' '
	JMP     ZBLNK2

ZBLNK1	STC
ZBLNK2	INX     H
	RET


DMESG   DB	ESC,'k'		H19 go to saved position
DBUFF   DB	0,0,0,0,0,'.'
	DB	200Q

***     Disk volume parameters
*
*       these are the parameter cells which will
*       eventually end up in the disk label
*

PARAM	EQU	*
	ERRNZ	*-PARAM+LAB.VPR-LAB.SIZ

VOLSIZ	DW	0		Sectors per volume
	ERRNZ	*-PARAM+LAB.VPR-LAB.PSS

SECSIZ	DW	NBPS		bytes per sector
	ERRNZ	*-PARAM+LAB.VPR-LAB.VFL

VOLFLG	DB	0		volume flags
	ERRNZ	*-PARAM-LAB.VPL

AUXPAR	EQU	*		Auxiliary Data
	ERRNZ	*-AUXPAR+LAB.AUX-LAB.SPT
PSPT	DB	NSPTS		sectors per track
	ERRNZ	*-AUXPAR-LAB.AXL

*	Directory block interleave (256 bytes)

DBIA	DB	000Q,001Q,002Q,003Q,004Q,005Q,006Q,007Q
	DB	010Q,011Q,012Q,013Q,014Q,015Q,016Q,017Q
	DB	020Q,021Q,022Q,023Q,024Q,025Q,026Q,027Q
	DB	030Q,031Q,032Q,033Q,034Q,035Q,036Q,037Q
	DB	040Q,041Q,042Q,043Q,044Q,045Q,046Q,047Q
	DB	050Q,051Q,052Q,053Q,054Q,055Q,056Q,057Q
	DB	060Q,061Q,062Q,063Q,064Q,065Q,066Q,067Q
	DB	070Q,071Q,072Q,073Q,074Q,075Q,076Q,077Q
	DB	100Q,101Q,102Q,103Q,104Q,105Q,106Q,107Q
	DB	110Q,111Q,112Q,113Q,114Q,115Q,116Q,117Q
	DB	120Q,121Q,122Q,123Q,124Q,125Q,126Q,127Q
	DB	130Q,131Q,132Q,133Q,134Q,135Q,136Q,137Q
	DB	140Q,141Q,142Q,143Q,144Q,145Q,146Q,147Q
	DB	150Q,151Q,152Q,153Q,154Q,155Q,156Q,157Q
	DB	160Q,161Q,162Q,163Q,164Q,165Q,166Q,167Q
	DB	170Q,171Q,172Q,173Q,174Q,175Q,176Q,177Q
	DB	200Q,201Q,202Q,203Q,204Q,205Q,206Q,207Q
	DB	210Q,211Q,212Q,213Q,214Q,215Q,216Q,217Q
	DB	220Q,221Q,222Q,223Q,224Q,225Q,226Q,227Q
	DB	230Q,231Q,232Q,233Q,234Q,235Q,236Q,237Q
	DB	240Q,241Q,242Q,243Q,244Q,245Q,246Q,247Q
	DB	250Q,251Q,252Q,253Q,254Q,255Q,256Q,257Q
	DB	260Q,261Q,262Q,263Q,252Q,265Q,266Q,267Q
	DB	270Q,271Q,272Q,273Q,274Q,275Q,276Q,277Q
	DB	312Q,313Q,314Q,315Q,316Q,317Q,320Q,321Q
	DB	322Q,323Q,324Q,325Q,326Q,327Q,330Q,331Q
	DB	332Q,333Q,322Q,323Q,324Q,325Q,326Q,327Q
	DB	330Q,331Q,332Q,333Q,334Q,335Q,336Q,337Q
	DB	340Q,341Q,342Q,343Q,344Q,345Q,346Q,347Q
	DB	350Q,351Q,352Q,353Q,354Q,355Q,356Q,357Q
	DB	360Q,361Q,362Q,363Q,364Q,365Q,366Q,367Q
	DB	370Q,371Q,372Q,373Q,374Q,375Q,376Q,377Q

	LON	G
	
	END     START
