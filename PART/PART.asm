*********************************************************************
*
*	PART67 - H/Z-67 Disk Partitioning Utility Program
*
*	This software was originally developed by Heath/Zenith
*	Data Systems.  Its purpose is to allow a user to divide
*	the H/Z-67 "Winchester" disk into separate partitions for
*	use by one or more operating systems.  The disk must have
*	been previously prepared using PREP67.  Changes made
*	with PART67 can destroy the contents of the disk so
*	appropriate backups should be done first.
*
*	The source code for this utility was never published.  This
*	listing is the result of a disassembly of the code
*	by Glenn Roberts in December 2011.  All comments and label
*	naming conventions are based on a "best guess" given the
*	description and observation of how the PART utility functions.
*
*	One of the functions of PART is to install the 10-sector H67
*	bootstrap program on track 0 of the disk.  To simplify things
*	this program assumes that the BOOT code will be assembled
*	separately and the result inserted into the PART67.ABS file.
*
*	The code has been verified to assemble to produce an executable
*	that is operationally identical to Version 1.1 of the file from 
*	Heath/Zenith.  The name has been changed to PART67 to allow
*	for additional features to be added without confusing it
*	with the original utility.
*
*	16 December 2011
*
*********************************************************************

	XTEXT	ASCII
	XTEXT	HOSDEF
	XTEXT	DIRDEF
	XTEXT	ECDEF
	XTEXT	TYPTX
	XTEXT	UDD
	XTEXT	HOSEQU
	XTEXT	INDL
	XTEXT	DDDEF
	XTEXT	ESINT
	XTEXT	ESVAL
	XTEXT	H67DEF

MI.JMP	EQU	303Q		JMP machine instruction
UCMASK	EQU	11011111B	AND to convert ASCII to upper case
*
*	Flags for superblock processing
*
E.RDSBA	EQU	10000000B	SBA Read error
E.CKSBA	EQU	01000000B	SBA CRC error
E.RDSBB	EQU	00100000B	SBB Read error
E.CKSBB	EQU	00010000B	SBB CRC error
E.CKMAT	EQU	00001000B	CRC mismatch
*
*	Version 1.1
*
V.MAJ	EQU	1
V.MIN	EQU	1

SYDVD	EQU	-1A		Dummy driver address for SY:
BOOENT	EQU	43000A		Boot entry

SAT.UA	EQU	36Q		Sector Allocation Table (Unallocated sector flag)
SAT.ET	EQU	37Q		Sector Allocation Table (End of table flag)

ENDFLG	EQU	377Q		Used to flag end of table

	ORG	USERFWA

START	JMP	PENTRY		Jump around boot record to PART Entry point

*********************************************************************
*
*	BOOT code goes here... (insert post-assembly)
*
SBCODE	DS	10*256		reserve 10 sectors...
*
*
*********************************************************************

*
* 	Entry Point for PART67
*
*	Step 1: load overlays, set memory top and CC handler
*
PENTRY	MVI	A,0		Overlay 0
	SCALL	.LOADO		Load it
	JC	ERRXIT		exit on error

	MVI	A,1		Overlay 1
	SCALL	.LOADO		Load it
	JC	ERRXIT		exit on error
	
	LXI	H,MAXMEM
	SCALL	.SETTP		Set mem top
	JC	ERRXIT		exit on error
	
	LXI	H,CCHIT	Set up CC Handler
	MVI	A,CTLC
	SCALL	.CTLC
	JC	ERRXIT		exit on error
*
*	Step 2: get handle for SY: driver and set
*		flag if drive is write protected
*
	CALL	LOADSY		load SY:

	LXI	B,0		write 0 bytes to check
	CALL	SYWRITE		for write protect
	MVI	A,0
	JNC	PMAIN1
	MVI	A,377Q		flag as write protected
PMAIN1	STA	WPFLAG
*
*	Step 3: Read first sector of existing boot track
*		(Master Control Block)
*
	XRA	A
	STA	SECNO		read sector 0 on disk
	STA	SECNO+1
	STA	SBCTMP		save a copy
	LXI	B,256		one sector
	LXI	D,SBCBUF	store in our local copy
	LXI	H,SECNO		address of sector no.
	CALL	SYREAD		read it
	JNC	HAVSBC
*
*	Ring the bell, print "FATAL ERROR ENCOUNTERED
*	WHILE READING H/Z67 MASTER CONTROL BLOCK.", 
*	then exit.
*
	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	PUSH	D
	LXI	D,P.FEMCB
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	JMP	C.QUIT
*
*	Step 4: Check the integrity of critical tables
*		(Bad Sector Tables and Superblock)
*
HAVSBC	PUSH	D
	LXI	D,P.CLS		Clear screen
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Read bad sector tables and check CRC
*	
	CALL	RDTBLS
*
*	Set up working region table buffer
*
	LDA	NREGNS		0-based (nominally 243)
	MOV	E,A
	MVI	D,0
	INX	D		DE = NREGNS+1
	LXI	H,BUFFER
	DAD	D		HL = BUFFER + NREGNS
	MVI	A,ENDFLG
	MOV	M,A		flag end region marker
	LXI	H,BUFF2		do second copy too
	DAD	D
	MOV	M,A		flag end region marker

	LXI	H,PMAIN2	push return address on stack
	PUSH	H
*
*	Read Superblocks and check CRC
*	
	CALL	RDSBS

	LXI	H,SAT.A		Sector Allocation Table A
	CALL	RTN453
*
*	Step 5: Jump to main menu loop
*
	JMP	MMLOOP1
*
*	
*
PMAIN2	CALL	GETDBS		get default boot string
	CALL	HRDCPY		process hard copy request
	LDA	WPFLAG		write protected?
	ORA	A
	JZ	PMAIN3		no, save it
*
*	PART Terminated, no changes applied
*
	PUSH	D
	LXI	D,P.PTNC
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	JMP	EXIT		exit immediately
*
*	save changes
*
PMAIN3	CALL	BLDSBK		build out the superblock
*
*	compute and store superblock checksum
*
	LXI	D,0
	LXI	B,3*256		three sectors
	LXI	H,SBLKA
	CALL	$BCRC
	XCHG
	SHLD	CKSSBA
	SHLD	CKSSBB
*
*	insert code version number
*
	MVI	A,V.MAJ
	STA	VMAJOR
	MVI	A,V.MIN
	STA	VMINOR
*
*	install boot jump in first 3 bytes
*
	MVI	A,MI.JMP
	STA	SBCBUF		first store JMP instruction
	LXI	H,BOOENT	then install
	SHLD	BOOADR		jump address for boot
	
	LXI	H,SBCBUF	Source
	LXI	D,SBCODE	Destination
	LXI	B,128		move it into our copy
	CALL	MOVEB
*
*	write superblock A
*
	LXI	D,SBLKA
	LXI	B,3*256		3 sectors
	LXI	H,SBA
	CALL	SYWRITE
	JNC	PMAIN4
*
*	Error writing superblock A
*
	PUSH	D
	LXI	D,P.EWSBA	Error message
	PUSH	PSW
	XRA	A
	CALL	PRNMSG		print it
	POP	PSW
	POP	D
*
*	write duplicate (superblock B)
*
PMAIN4	LXI	D,SBLKA
	LXI	B,3*256		3 sectors
	LXI	H,SBB
	CALL	SYWRITE
	JNC	PMAIN5
*
*	Error writing superblock B
*
	PUSH	D
	LXI	D,P.EWSBB	Error message
	PUSH	PSW
	XRA	A
	CALL	PRNMSG		print it
	POP	PSW
	POP	D
*
*	Write software boot code (10 sectors, track 0)
*
PMAIN5	XRA	A
	STA	DSKADD
	STA	DSKADD+1
	STA	DSKADD+2	Writing to sector 0
	LXI	D,SBCODE	Software boot code
	LXI	B,10*256	all 10 sectors worth...
	LXI	H,DSKADD	Write to sector 0
	CALL	SYWRITE		do the write!
	JNC	PEXIT
*
*	WARNING - Error encountered while writing SBC
*
	PUSH	D
	LXI	D,P.EWSBC	error message
	PUSH	PSW
	XRA	A
	CALL	PRNMSG		print it
	POP	PSW
	POP	D
*
*	PART Terminated - Changes in effect.
*
PEXIT	PUSH	D
	LXI	D,P.TERM	message
	PUSH	PSW
	XRA	A
	CALL	PRNMSG		print it
	POP	PSW
	POP	D

EXIT	MVI	A,0
	SCALL	.EXIT
	SPACE	4,10
***	LOADSY - Loads SY: device handler
*
LOADSY	LXI	H,SYFD		SY: File Descriptor
	SCALL	.LOADD	
	JC	ERRXIT		abort on error
	LXI	D,3
	DAD	D		3 more than HL return
	SHLD	SYLOC1		save as jump point into driver
	SHLD	SYLOC2
	RET
	SPACE	4,10
*** 	CCHIT - Control-C Handler - clear stack and exit
*
CCHIT	POP	PSW
	POP	PSW
	POP	PSW
	JMP	ABORT
	SPACE	4,10
*	Abort - exit on error
*
ERRXIT	MVI	H,EC.ILR	Illegal request
	SCALL	.ERROR
*
*	PART Aborted, no changes applied
*
C.QUIT	PUSH	D
	LXI	D,P.ABORT
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Exit with error
*
ABORT	MVI	A,1
	SCALL	.EXIT
	SPACE	4,10
*
*	Main Menu Loop
*
MMLOOP	PUSH	D
	LXI	D,P.CLS
	PUSH	PSW
	XRA	A
	CALL	PRNMSG		Clear screen
	POP	PSW
	POP	D
*
*	HEATH/ZENITH H/Z67
*	Winchester Disk Partitioning
*	Utility (PART) vers x.x
*
MMLOOP1	PUSH	D
	LXI	D,P.INTRO
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Print the main menu:
*
*	A. All CP/M
*	B. All HDOS
*	C. All UCSD PASCAL
*	D. Half CP/M, half HDOS
*	E. Half CP/M, half UCSD PASCAL
*	F. Half HDOS, half UCSD PASCAL
*	G. Third CP/M, third HDOS, third UCSD PASCAL
*	H. User-Defined Partitions
*
	PUSH	D
	LXI	D,P.MENU
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Check for write protect
*
	LDA	WPFLAG
	ORA	A
	JZ	MMLOOP2
*
*	Warn about write protection
*	(changes will not be saved)
*
	PUSH	D
	LXI	D,P.ISWP
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Prompt to enter selection
*	(or hit enter for default)
*
MMLOOP2	PUSH	D
	LXI	D,P.SEL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Read menu selection
*
	LXI	D,RDMAX
	MVI	A,1		1 character answer
	STAX	D
	CALL	RDANSW		read answer
	LDA	RDAMT		how many read?
	ORA	A
	JZ	MMLOOP4		none! - take the default
	LDA	RDBUF
	ANI	UCMASK		ensure upper case
	SUI	'A'
	JC	MMLOOP3		non alpha
	CPI	'H'-'A'+1
	JC	MMLOOP5		must be 'A' through 'H'
*
*	invalid entry (beep and try again)
*
MMLOOP3	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	JMP	MMLOOP2		try again

MMLOOP4	MVI	A,'G'-'A'	default is 'G' (1/3, 1/3, 1/3)
*
*	Entry:
*		A = 0-7 (A through G partitioning schemes)
*
MMLOOP5	PUSH	PSW
	CALL	FINDEND
	POP	PSW
	CPI	'H'-'A'
	JZ	DOUSER		7 = 'H' = User defined
*
*	0-6
*
	PUSH	PSW
	LXI	H,NLEFT
	MVI	B,0
	MOV	C,M		BC = number left (count)
	LXI	H,NXTAV
	LXI	D,RCT.A		Region Control Table (A)
	MOV	L,M
	MVI	H,0		HL = end
	DAD	D		HL = pointer to end in RCT
	XCHG			DE = pointer to end in RCT
	MVI	A,10000000B	set dirty bits
	CALL	FILLM
*
*	Blank out default boot line
*
	MVI	A,' '
	LXI	B,19
	LXI	D,DEFBOOT
	CALL	FILLM
*
*	Blank out OS ID table
*
	MVI	A,' '
	LXI	B,256
	LXI	D,OSIDT.A
	CALL	FILLM
*
*
*
	LDA	NREGNS		N regions, zero basaed (typically 243)
	MOV	C,A
	MVI	B,0
	INX	B		BC = NREGNS (correct for 0 base)
	MVI	A,0
	LXI	D,BUFFER	zero working buffer
	CALL	FILLM

	POP	PSW		A = command (0-6)
	MOV	L,A
	MVI	H,0		HL = A
	DAD	H		HL = A*2
	PUSH	H
	DAD	H		HL = A*4
	POP	D
	DAD	D		HL = A*6
	LXI	D,VECTBL	Vector table (each entry 6 bytes)
	DAD	D		HL = A*6+table addr
	MOV	E,M
	INX	H
	MOV	D,M		DE = first entry (labels)
	INX	H
	MOV	C,M
	INX	H
	MOV	B,M		BC = second entry (byte count)
	INX	H
	PUSH	H
	XCHG			HL = first entry
	LXI	D,OSIDT.A
	CALL	MOVEB		copy OS labels into table (up to 48 bytes)
	POP	H
	MOV	E,M		DE = third entry
	INX	H
	MOV	D,M
	XCHG			HL = third entry (dispatch addr)
	LXI	D,CHKRAM
	PUSH	D		save as RETurn point
	PCHL			jump to dispatch from table!
*
*	Return here after dispatch
*
CHKRAM	PUSH	D
	LXI	D,P.CLS		Clear screen
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	CALL	DSPRAM		display Region Allocation Map
*
*	Allocation Correct
*	..............<N>?
*
CHKRA1	PUSH	D
	LXI	D,P.CHKOK
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	LXI	D,RDMAX
	MVI	A,1		1 character answer
	STAX	D
	CALL	RDANSW
	LDA	RDAMT		how many read?
	ORA	A
	JZ	MMLOOP		none! - default <N>, start over
	LDA	RDBUF
	ANI	UCMASK		map to UPPER
	CPI	'Y'
	RZ			ok, done
	CPI	'N'
	JZ	MMLOOP		'N', start over!
*
*	Ring bell and try again
*
	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	JMP	CHKRA1
	
	RET			unreachable

*
*	Allocate all to one OS
*
OSONE	MVI	E,0		Start at 0
	LDA	NXTAV		next available
	DCR	A		minus 1
	MOV	C,A		ending region
	MVI	A,0		OS ID fill byte
	LXI	H,BUFFER	point to the table
	CALL	FILLID		fill from E to C w/ ID 0
	
	LDA	NXTAV		next available
	MOV	E,A		start there
	LXI	H,NREGNS
	MOV	C,M		fill to end
	MVI	A,1		all OS #1
	LXI	H,BUFFER
	CALL	FILLID
	RET
*
*	Allocate half and half
*
OSTWO	MVI	E,0		Start at 0
	LDA	NXTAV		next available
	DCR	A		minus 1
	MOV	C,A		ending region
	MVI	A,0		OS ID fill byte
	LXI	H,BUFFER	point to the table
	CALL	FILLID		fill from E to C w/ ID 0
	
	LDA	NLEFT
	MOV	C,A
	MVI	B,0		dividend = number left
	LXI	D,2		divisor = 2
	CALL	$DU66		compute the quotient
	PUSH	H
	POP	B		BC = quotient
	LDA	NXTAV
	MOV	E,A		start
	MOV	L,A
	MVI	H,0
	DCX	H
	DAD	B
	PUSH	H
	POP	B		C=start-1+slots/OS
	MVI	A,1		first half OS #1 (A)
	LXI	H,BUFFER
	CALL	FILLID

	MOV	E,C		start where we left off
	INR	E		plus one...
	LXI	H,NREGNS
	MOV	C,M		end = table end
	MVI	A,2		second half  OS #2 (B)
	LXI	H,BUFFER
	CALL	FILLID

	RET
*
*	Allocate 1/3, 1/3, 1/3
*
OSTHREE	MVI	E,0		Start at 0
	LDA	NXTAV		next available
	DCR	A		minus 1
	MOV	C,A		ending region
	MVI	A,0		OS ID fill byte
	LXI	H,BUFFER	point to the table
	CALL	FILLID		fill from E to C w/ ID 0
	
	LDA	NLEFT
	MOV	C,A
	MVI	B,0		BC = number of slots left
	LXI	D,3		divide by three
	CALL	$DU66
	PUSH	H
	POP	B		BC = slots/OS
	LDA	NXTAV		next available
	MOV	E,A		E = start
	MOV	L,A
	MVI	H,0		HL = next available
	DCX	H
	PUSH	B		save it
	DAD	B		HL = next avail + slots/OS
	PUSH	H
	POP	B		C = end
	MVI	A,1		first third OS #1 (A)
	LXI	H,BUFFER
	CALL	FILLID
	
	MOV	E,C		start where we left off
	INR	E		plus one...
	POP	B
	XRA	A
	MOV	A,C
	RAL			*2 (2/3)
	MOV	C,A
	LDA	NXTAV		next available
	MOV	L,A
	MVI	H,0
	DCX	H
	DAD	B
	PUSH	H
	POP	B
	MVI	A,2		second third OS #2 (B)
	LXI	H,BUFFER
	CALL	FILLID
	
	MOV	E,C
	INR	E
	LXI	H,NREGNS
	MOV	C,M
	MVI	A,3		third third OS #3 (C)
	LXI	H,BUFFER
	CALL	FILLID
	RET
*
*	vector table
*
VECTBL	DW	L.CPM,16*1,OSONE	All CP/M
	DW	L.HDOS,16*1,OSONE	All HDOS
	DW	L.UCSD,16*1,OSONE	All UCSD PASCAL
	DW	L.CPHD,16*2,OSTWO	Half CP/M, half HDOS
	DW	L.CPUC,16*2,OSTWO	Half CP/M, half UCSD PASCAL
	DW	L.HDUC,16*2,OSTWO	Half HDOS, half UCSD PASCAL
	DW	L.CPHU,16*3,OSTHREE	Third CP/M, third HDOS, third UCSD PASCAL
*	All CP/M
L.CPM	DB	'CPM             '
*	All HDOS
L.HDOS	DB	'HDOS            '
*	All UCSD Pascal
L.UCSD	DB	'UCSDPASCAL      '
*	Half CP/M, half HDOS
L.CPHD	DB	'CPM             '
	DB	'HDOS            '
*	Half CP/M, half UCSD Pascal
L.CPUC	DB	'CPM             '
	DB	'UCSDPASCAL      '
*	Half HDOS, half UCSD Pascal
L.HDUC	DB	'HDOS            '
	DB	'UCSDPASCAL      '
*	One third each
L.CPHU	DB	'CPM             '
	DB	'HDOS            '
	DB	'UCSDPASCAL      '
*
*	User-specified layout requested
*
DOUSER	PUSH	D
	LXI	D,P.CLS		Clear screen
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	CALL	DSPRAM		Display Region Allocation Map
*
*	Display Command menu
*
*		O = Set OS
*		P = Set Partition
*		D = Display
*		A = Abandon
*		S = Save
*	
CMDMENU	PUSH	D
	LXI	D,P.CMD
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	LXI	D,RDMAX
	MVI	A,1		1 characater answer
	STAX	D
	CALL	RDANSW
	LDA	RDAMT		how many read?
	ORA	A
	JZ	DOUSER		none! <D> (display) is default!
	LXI	H,CMDMENU
	PUSH	H
	LDA	RDBUF		get the character
	ANI	UCMASK		make it upper case
	CPI	'O'
	JZ	C.SETOS		Set OS
	CPI	'P'
	JZ	C.SETPT		Set Partition
	POP	H
	CPI	'D'
	JZ	DOUSER		Display
	CPI	'A'
	JZ	C.QUIT		Abandon
	CPI	'S'
	RZ			Save
*
*	Beep and try again...
*
	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	JMP	CMDMENU
	SPACE	4,10
*
*	SET OS - Ask user to select an OS
*
C.SETOS	LDA	OIDTMP		get back previous as default
	CALL	OIDLKU		Look up Letter ID for OS
	STA	D.OSID		set default
*
*	Operating System ID (S,U,A-P)
*
	PUSH	D
	LXI	D,P.OSID
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	LXI	D,RDMAX
	MVI	A,1		1 character answer
	STAX	D
	CALL	RDANSW
	LDA	RDAMT		how many read?
	ORA	A
	JZ	SETOSB		none!
	LDA	RDBUF
	ANI	UCMASK
	CPI	'U'
	JZ	SETOS2		unallocated
	CPI	'S'
	JZ	SETOS3		system
	CPI	'A'
	JC	SETOS1		<'A'? (bad)
	CPI	'P'+1
	JC	SETOS4		<='P'? (good)
*
*	Invalid OS ID - Beep and try again
*
SETOS1	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	JMP	C.SETOS
*
*	Unallocated
*
SETOS2	MVI	A,SAT.UA
	STA	OIDTMP
	LXI	H,P.UNALL		'Unallocated     '
	SHLD	UNK719
	JMP	SETOSB
*
*	System
*
SETOS3	MVI	A,0		0 = system
	STA	OIDTMP
	LXI	H,P.SYSTM		'System          '
	SHLD	UNK719
	JMP	SETOSB
*
*	Have valid OS ID (A thru P)
SETOS4	SUI	'A'-1		OS ID = 1 thru 16
	STA	OIDTMP
	DCR	A		make it zero-based
	MOV	L,A
	MVI	H,0
	DAD	H		*2
	DAD	H		*4
	DAD	H		*8
	DAD	H		*16
	LXI	D,OSIDT.A
	DAD	D		HL = offset in OS ID table
	SHLD	UNK719		save it
	LXI	D,D.OSNAM	where to put it
	LXI	B,16		move 16 bytes
	CALL	MOVEB
*
*	Operating System Name...
*
SETOS5	PUSH	D
	LXI	D,P.OSNAM
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	LXI	D,RDMAX
	MVI	A,16		up to 16 characters
	STAX	D
	CALL	RDANSW		read the response
	LDA	RDAMT
	ORA	A
	JZ	SETOSB		NL hit, take default
	LDA	RDBUF
	CPI	' '
	JNZ	SETOS6
	LHLD	UNK719
	XCHG
	MVI	A,' '
	LXI	B,16
	CALL	FILLM
	LDA	OIDTMP
	CALL	RTN538
	JMP	SETOSB

SETOS6	LDA	RDAMT
	MOV	C,A
	MVI	B,0
	LXI	H,RDBUF
	CALL	ISANUM		Alpha numberic?
	JNZ	SETOS9
	MVI	A,' '
	LXI	B,16
	LXI	D,STRTEMP
	CALL	FILLM
	LDA	RDAMT
	MOV	C,A
	MVI	B,0
	LXI	D,STRTEMP
	LXI	H,RDBUF
	CALL	MOVEB
	MVI	B,16
	LXI	H,OSIDT.A
	
SETOS7	PUSH	B
	PUSH	H
	LDA	RDAMT
	LXI	B,16		compare 16 characters
	LXI	D,STRTEMP
	CALL	STRCMP
	JZ	SETOS8		match?
	POP	H
	POP	B
	
	DCR	B
	JZ	SETOSA
	LXI	D,16		point to next entry
	DAD	D
	JMP	SETOS7		and loop
*
*	16 byte temp storage
*
STRTEMP	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

SETOS8	POP	D
	POP	B
*
*	Duplicate OS names not allowed
*
	PUSH	D
	LXI	D,P.DNNA
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Ring the bell and try again...
*
SETOS9	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	JMP	SETOS5

SETOSA	LHLD	UNK719
	XCHG
	MVI	A,' '
	LXI	B,16
	CALL	FILLM
	LDA	RDAMT
	MOV	C,A
	MVI	B,0
	LHLD	UNK719
	XCHG
	LXI	H,RDBUF
	CALL	MOVEB

SETOSB	LHLD	UNK719
	MOV	A,M
	CPI	' '
	RNZ
	JMP	SETOS2
	
	SPACE	4,10
*
*	C.SETPT - process set partition command
*
C.SETPT	LDA	OIDTMP
	CPI	377Q
	JNZ	SETPT1
	CALL	C.SETOS
	JMP	C.SETPT

SETPT1	LHLD	UNK719
	LXI	D,D.FRREG
	LXI	B,16
	CALL	MOVEB
	LHLD	UNK719
	LXI	D,D.TOREG
	LXI	B,16
	CALL	MOVEB
*
*	] from Region <done>?
*
SETPT2	PUSH	D
	LXI	D,P.FRREG
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	LXI	D,RDMAX
	MVI	A,2		2 character answer
	STAX	D
	CALL	RDANSW
	LDA	RDAMT
	ORA	A
	JZ	SETPTE		NL hit, take default
	LXI	H,RDBUF
	LDA	RDAMT
	MOV	C,A
	MVI	B,0
	CALL	HEXVAL
	JZ	SETPT4
*
*	Beep and try again
*
SETPT3	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	JMP	SETPT2

SETPT4	LDA	NXTAV
	DCR	A
	CMP	E
	JNC	SETPT3
	LDA	NREGNS
	CMP	E
	JC	SETPT3
	XCHG
	SHLD	UNK378
	LDA	RDBUF
	STA	DAT662
	LDA	RDAMT
	CPI	1
	MVI	A,40Q
	JZ	SETPT5
	LDA	RDBUF+1
SETPT5	STA	DAT663
*
*	[ ] to   Region ...
*
SETPT6	PUSH	D
	LXI	D,P.TOREG
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	LXI	D,RDMAX
	MVI	A,2		2 character answer
	STAX	D
	CALL	RDANSW
	LDA	RDAMT
	ORA	A
	JZ	SETPT8		NL hit, take default
	LXI	H,RDBUF
	LDA	RDAMT
	MOV	C,A
	MVI	B,0
	CALL	HEXVAL
	JZ	SETPT9
*
*	Beep and try again
*
SETPT7	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	JMP	SETPT6

SETPT8	LHLD	UNK378
	SHLD	UNK379
	JMP	SETPTB

SETPT9	MOV	A,E
	ORA	A
	JZ	SETPT7
	MOV	A,D
	ORA	A
	JNZ	SETPT7
	LDA	NREGNS
	CMP	E
	JC	SETPT7
	LHLD	UNK378
	CALL	HLCPDE
	JZ	SETPTA
	JNC	SETPT7
SETPTA	XCHG
	SHLD	UNK379
SETPTB	LXI	H,BUFFER	From
	LXI	D,BUFF2		To
	LDA	NREGNS
	MOV	C,A
	MVI	B,0
	INX	B		BC = No. regions
	CALL	MOVEB
	LHLD	UNK378
	XCHG
	LHLD	UNK379
	PUSH	H
	POP	B
	LXI	H,BUFF2
	LDA	OIDTMP
	CALL	FILLID
	INX	H
	MOV	A,M
	CPI	ENDFLG
	JZ	SETPTC
	ORI	200Q
	MOV	M,A
SETPTC	LXI	H,BUFF2
	CALL	RTN561
	MOV	A,B
	CPI	100Q
	JC	SETPTD
*
*	Bell
*
	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Error - max number of user partitions
*
	PUSH	D
	LXI	D,P.MNUP
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	JMP	SETPT2

SETPTD	LHLD	UNK378
	XCHG
	LHLD	UNK379
	PUSH	H
	POP	B
	LXI	H,RCT.A		Region Control Table (A)
	CALL	RTN536
	LXI	H,BUFF2		From
	LXI	D,BUFFER	To
	LDA	NREGNS
	MOV	C,A
	MVI	B,0
	INX	B		BC = NREGNS
	CALL	MOVEB
	JMP	SETPT2

SETPTE	RET

UNK378	DW	0
UNK379	DW	0

*	RDTBLS - Read Bad Sector Tables A & B and do CRC
*
*	Exit:
*		'C' set on error
*

*	Read Table A
*
RDTBLS	LXI	D,BSTBUFF	Transfer address
	LXI	B,2*256		two sectors
	LXI	H,BSTA		Bad Sector Table A
	CALL	SYREAD		read it!
	JNC	RDTBL1		read OK? jump
*
*	Read error on table A
*
	PUSH	D
	LXI	D,P.TABAD	Error- bad sector table A
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	JMP	RDTBL2		try B...
*
*	Table A read OK, now check CRC
*
RDTBL1	LXI	D,0
	LXI	B,2*256
	LXI	H,BSTBUFF
	CALL	$BCRC		compute CRC
	LHLD	CKSBSA		what should it be?
	CALL	HLCPDE		compare
	JZ	RDTBL2		OK!
*
*	CRC error on Table A
*
	PUSH	D
	LXI	D,P.TABAD	Error- bad sector table A
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Read Table B
*
RDTBL2	LXI	D,BSTBUFF
	LXI	B,2*256
	LXI	H,BSTB
	CALL	SYREAD
	JNC	RDTBL3		read OK?, jump
*
*	Read error on Table B
*
	PUSH	D
	LXI	D,P.TBBAD	Error- bad sector table B
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	JMP	RDTBL4		Return
*
*	Compute CRC for Table B
*
RDTBL3	LXI	D,0
	LXI	B,2*256
	LXI	H,BSTBUFF
	CALL	$BCRC
	LHLD	CKSBSB
	CALL	HLCPDE
	JZ	RDTBL4
*
*	CRC error, table B
*
	PUSH	D
	LXI	D,P.TBBAD	Error- bad sector table B
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
RDTBL4	RET
*
*	Read superblocks and check CRC
*
RDSBS	XRA	A
	STA	SBERFLG		clear superblock error flags
	LXI	D,SBLKA
	LXI	B,3*256		superblock has 3 sectors
	LXI	H,SBA
	CALL	SYREAD		read superblock A
	JNC	RDSBS1
*
*	Error reading superblock A
*
	LDA	SBERFLG
	ORI	E.RDSBA	flag read error on SBA
	STA	SBERFLG
*
*	WARNING - Read Error in Superblock A
*
	PUSH	D
	LXI	D,P.ESBA
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	JMP	RDSBS3
*
*	Have Superblock A, now do CRC.
*
RDSBS1	LXI	D,0
	LXI	B,3*256			Superblock has 3 sectors
	LXI	H,SBLKA
	CALL	$BCRC
	LHLD	CKSSBA
	CALL	HLCPDE
	JZ	RDSBS2
*
*	Checksum error!
*
	LDA	SBERFLG
	ORI	E.CKSBA	flag CRC error in SBA
	STA	SBERFLG
*
*	WARNING - Checksum Error in Superblock A
*
	PUSH	D
	LXI	D,P.CKSA
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	JMP	RDSBS3
*
*	Process Superblock B
*
RDSBS2	XCHG
	SHLD	UNK436
RDSBS3	LXI	D,SBLKB
	LXI	B,3*256			Superblock has 3 sectors
	LXI	H,SBB
	CALL	SYREAD			read Superblock B
	JNC	RDSBS4
*
*	Error reading Superblock B
*
	LDA	SBERFLG
	ORI	E.RDSBB	flag read error on SBB
	STA	SBERFLG
*
*	WARNING - Read Error in Superblock B
*
	PUSH	D
	LXI	D,P.ESBB
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	JMP	RDSBS6

RDSBS4	LXI	D,0
	LXI	B,3*256
	LXI	H,SBLKB
	CALL	$BCRC
	LHLD	CKSSBB
	CALL	HLCPDE
	JZ	RDSBS5
	LDA	SBERFLG
	ORI	E.CKSBB		flag CRC error on SBB
	STA	SBERFLG
*
*	WARNING - Checksum Error in Superblock B
*
	PUSH	D
	LXI	D,P.CKSB
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	JMP	RDSBS6

RDSBS5	LDA	SBERFLG
	ORA	A
	JNZ	RDSBS6
	LHLD	UNK436
	CALL	HLCPDE		compare checksums
	JZ	RDSBS6
	LDA	SBERFLG
	ORI	E.CKMAT		flag checksum mismatch
	STA	SBERFLG
*
*	WARNING - Verification Error between
*	Checksum A and Checksum B
*
	PUSH	D
	LXI	D,P.ERRV
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Check for errors and process accordingly
*
RDSBS6	LDA	SBERFLG
	MOV	B,A
	ANI	E.RDSBA+E.RDSBB
	CPI	E.RDSBA+E.RDSBB
	JZ	RTN426		both SBA and SBB unreadable!

	ORA	A
	JNZ	RTN422
	
	MOV	A,B
	ANI	E.CKSBA+E.CKSBB	both checksums unreadable?
	JNZ	RDSBS7
	MOV	A,B
	ANI	E.CKMAT		checksum mismatch?
	RZ
*
*	WARNING - Superblocks Readable, but
*
	PUSH	D
	LXI	D,P.WSB
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Checksums A and B Do Not Match.
*
	PUSH	D
	LXI	D,P.MATCH
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	JMP	RTN419

RDSBS7	CPI	E.CKSBA+E.CKSBB
	JNZ	RTN420
*
*	WARNING - Superblocks Readable, but
*
	PUSH	D
	LXI	D,P.WSB
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Both Superblock Checksums are in Error.
*
	PUSH	D
	LXI	D,P.SBAB
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

RTN419	CALL	RTN427
*
*	NOTE: The Region Allocation that you
*	have chosen is now in effect.
*	Press RETURN.
*
	PUSH	D
	LXI	D,P.RAMNE
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	LXI	D,RDMAX
	MVI	A,1		1 character answer
	STAX	D
	CALL	RDANSW		read any char (throw it away)
*
*	Clear screen
*
	PUSH	D
	LXI	D,P.CLS
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	RET


RTN420	CPI	E.CKSBA
	JNZ	RTN421
*
*	Bad Checksum from Superblock A, recreating
*	from superblock B
*
	PUSH	D
	LXI	D,P.BCSBA
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	CALL	RTN433		put Superblock B into A
	RET
*
*	Bad Checksum from Superblock B, recreating
*	from superblock A
*
RTN421	PUSH	D
	LXI	D,P.BCSBB
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	RET


RTN422	CPI	E.RDSBA
	JNZ	RTN424
	MOV	A,B
	ANI	E.CKSBB
	JZ	RTN423
*
*	WARNING - Superblock A is Unreadable and
*	Checksum for Superblock B is In Error.
*
*	Information in Superblock B may be corrupt.
*
	PUSH	D
	LXI	D,P.SBAU
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
RTN423	LXI	H,SBA		two low bytes of SBA triple
	CALL	FMTTRK
	JC	FMTERR
*
*	Reformat was successful
*
	PUSH	D
	LXI	D,P.RWS
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	NOTE: Region Allocation information will be
*	taken from Superblock B.
*
	PUSH	D
	LXI	D,P.RAFB
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	CALL	RTN433		Put superblock B into A
	RET


RTN424	MOV	A,B
	ANI	E.CKSBA
	JZ	RTN425
*
*	WARNING - Superblock B is Unreadable and 	
*	Checksum for Superblock A is In Error.
*
*	Information in Superblock A may be corrupt.
*
	PUSH	D
	LXI	D,P.SBBU
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

RTN425	LXI	H,SBB
	CALL	FMTTRK
	JC	FMTERR
*
*	reformat was successful
*
	PUSH	D
	LXI	D,P.RWS
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	NOTE: Region Allocation information will be
*	taken from Superblock A.
*
	PUSH	D
	LXI	D,P.RAFA
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	RET
*
*	FATAL ERROR ENCOUNTERED WHILE READING
*	H/Z67 SUPERBLOCKS
*
*	SUPERBLOCKS A AND B ARE UNREADABLE - 
*	PARTITIONING IMPOSSIBLE.
*
RTN426	PUSH	D
	LXI	D,P.ERRSB
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	JMP	C.QUIT
*
*	To avoid loss of your data,
*	choose the correct Region Allocation Map.
*
*	Press RETURN to display the first choice.
*
RTN427	PUSH	D
	LXI	D,P.WRAM
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	LXI	D,RDMAX
	MVI	A,1		1 character answer
	STAX	D
	CALL	RDANSW		read NL, throw it away
RTN428	LXI	H,SAT.A		Sector Allocation Table A
	CALL	RTN453
*
*	Clear screen
*
	PUSH	D
	LXI	D,P.CLS
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
RTN429	CALL	DSPRAM		Display Region Allocation Map
	CALL	RTN430
	RZ
	LXI	H,SAT.B		Sector Allocation Table B
	CALL	RTN453
*
*	Clear Screen
*
	PUSH	D
	LXI	D,P.CLS
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	CALL	DSPRAM		Display Region Allocation Map
	CALL	RTN430
	JNZ	RTN428
	CALL	RTN433		Put Superblock B into A
	RET
*
*	Allocation Correct
*	..............<N>?
*
RTN430	PUSH	D
	LXI	D,P.CHKOK
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Press RETURN to display the other choice.
*
	PUSH	D
	LXI	D,P.PRDOC
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	LXI	D,RDMAX
	MVI	A,1		1 character answer
	STAX	D
	CALL	RDANSW
	LDA	RDAMT
	ORA	A
	JZ	RTN431		Default is <N>
	LDA	RDBUF
	ANI	UCMASK
	CPI	'Y'
	JZ	RTN432
	CPI	'N'
	JZ	RTN431
*
*	Beep and try again
*
	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	JMP	RTN430

RTN431	XRA	A
	INR	A
	RET


RTN432	XRA	A
	RET
*
*	Copy Superblock B to Superblock A
*
RTN433	LXI	B,3*256		3 sectors
	LXI	H,SBLKB	From
	LXI	D,SBLKA	To
	CALL	MOVEB		Move
	RET

**	FMTTRK - format track
*
*	ENTRY:	HL = address of triple
*
FMTTRK	LDA	WPFLAG
	ORA	A
	STC
	RNZ			Return if write protect
*
*	HL = two low bytes of triple
*
	PUSH	D		Load HL from (HL)
	MOV	E,M
	INX	H
	MOV	D,M
	XCHG
	POP	D
	
	CALL	RDSTAT
	ANI	BS.REQ+BS.BSY+BS.INT+BS.PE
	STC
	RNZ
	MVI	A,D.FT		format track
	CALL	BLDCDR
	CALL	GETCON
	CALL	DOCMD
	CALL	CMPSTAT
	RET
*
*	FATAL ERROR ENCOUNTERED DURING REFORMAT.
*
FMTERR	PUSH	D
	LXI	D,P.FEEDR
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	JMP	C.QUIT

UNK436	DW	0
SBERFLG	DB	0		Error flags for Superblock processing
	DB	0

*	GETDBS - prompt user for default boot string, parse and stroe
*
GETDBS	LXI	H,DEFBOOT	put a copy of current default boot string
	LXI	D,D.DBS		into screen message
	LXI	B,19
	CALL	MOVEB
*
*	print "Enter default boot string"
*
	PUSH	D
	LXI	D,P.EDBS
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Read user answer
*
	LXI	D,RDMAX
	MVI	A,19		19 character answer
	STAX	D
	CALL	RDANSW
	
	LDA	RDAMT
	ORA	A
	JZ	GETDB4		default (leave empty)
*
*	' '? if so blank it
*
	LXI	H,RDBUF
	MOV	A,M
	CPI	' '
	JNZ	GETDB1
	MVI	A,' '
	LXI	B,19
	LXI	D,DEFBOOT
	CALL	FILLM		blank default boot
	JMP	GETDB4
*
*	user entered a string, parse it
*
GETDB1	LDA	RDAMT
	MOV	C,A
	MVI	B,0		BC = length
	CALL	ISANUM		Alpha numeric?
	MOV	A,E		how many bytes successfully processed?
	ORA	D		DE = 0?
	JZ	GETDB2		error, no alpha-numeric characters

	PUSH	H
	LXI	H,16
	CALL	HLCPDE
	POP	H
	JC	GETDB2		error, too long!

	MOV	A,C		did we process the whole string?
	ORA	B		BC = 0?
	JZ	GETDB3		yes, copy string from buffer
	
	MOV	A,M		no, there's more
	INX	H		check for semicolon
	DCX	B
	CPI	';'
	JNZ	GETDB2		error, only ';' allowed!
	
	MOV	A,C
	ORA	B
	JZ	GETDB3		if no more we're done
	
	CALL	GETNUM		get the number
	JNZ	GETDB2		error getting number
	MOV	A,E
	CPI	64		must be 0-63
	JC	GETDB3
*
*	Beep
*
GETDB2	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Syntax Error in Boot String
*
	PUSH	D
	LXI	D,P.SEBS
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	
	JMP	GETDBS

GETDB3	MVI	A,' '		first fill with blanks
	LXI	B,19
	LXI	D,DEFBOOT
	CALL	FILLM
	
	LXI	H,RDBUF		HL = source
	LDA	RDAMT
	MOV	C,A
	MVI	B,0		BC = length
	LXI	D,DEFBOOT	DE = dest
	CALL	MOVEB		move it
GETDB4	RET


*	Display Region Allocation Map
*
*	HEATH/ZENITH H/Z67'
*	         Winchester Disk Partitioning
*		 Utility (PART) vers 1.1
*
DSPRAM	PUSH	D
	LXI	D,P.INTRO
	PUSH	PSW
	MVI	A,6
	ORA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Print the header rows first
*
	PUSH	D
	LXI	D,P.OSMAP
	PUSH	PSW
	MVI	A,6
	ORA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	PUSH	D
	LXI	D,P.HDR
	PUSH	PSW
	MVI	A,6
	ORA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Loop over the rows
*
	XRA	A		start at 0 ('A')
	STA	ROWID
RLOOP	MVI	A,' '		First blank out the row
	LXI	B,90
	LXI	D,ROWBUFF
	CALL	FILLM

	LXI	D,ROWBUFF
	LDA	ROWID
	ADI	'A'		add alpha offset
	STAX	D		store it
	INX	D
	MVI	A,'.'		then a period
	STAX	D
	INX	D
	INX	D		blank space
	LDA	ROWID		get value
	CALL	LDOSID		fill in OS name
	INX	D
	INX	D		spaces....
	INX	D
	INX	D
	INX	D
	INX	D
	INX	D
	
	LDA	ROWID		row number
	ADI	'0'		print it as a Hex digit
	CPI	'9'+1
	JC	RLOOP1
	ADI	7		A-F
RLOOP1	STAX	D

	INX	D		spaces...
	INX	D
	MVI	A,':'		:
	STAX	D
	INX	D		spaces..
	INX	D
	LDA	ROWID
	
	CALL	LDLINE		fill in a line of data
	CALL	ADCRLF		and end it
*
*	now print the line
*
	PUSH	D
	LXI	D,ROWBUFF
	PUSH	PSW
	MVI	A,6
	ORA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	LDA	ROWID
	CPI	15
	RZ
	INR	A
	STA	ROWID
	JMP	RLOOP
*
*	Look up OSID given letter ID
*
*	ENTRY: 	DE = destination
*		A = OS ID A to P (0..15)
*
*	EXIT: 	DE = next available position
*
LDOSID	MOV	L,A
	MVI	H,0
	DAD	H		*2
	DAD	H		*4
	DAD	H		*8
	DAD	H		*16
	LXI	B,OSIDT.A
	DAD	B		HL = offset of OS ID
	LXI	B,16		each entry 16 bytes
	CALL	MOVEB		store it!
	RET
*
*	Fill in a line of the region allocation map
*
*	ENTRY: 	A = Row ID (0-15)
*		DE = destination buffer
*
*	EXIT:	'C' - error
*
LDLINE	ADD	A		*2
	ADD	A		*4
	ADD	A		*8
	ADD	A		*16
	MOV	L,A
	LDA	NREGNS
	CMP	L		too big?
	RC
	MVI	H,0		HL = Row ID * 16
	PUSH	H
	LXI	B,RCT.A		Region Control Table (A)
	DAD	B		add offset
	SHLD	RCTENT		save as pointer to RCT Entry
	POP	H
	LXI	B,BUFFER
	DAD	B
	MVI	B,16		16 entries per line
LDLIN1	MOV	A,M
	CPI	ENDFLG		END flag?
	RZ			done!
	ANI	10000000B
	JZ	LDLIN2		if not set, skip
	MVI	A,'>'		indicates beginning of a partition
	STAX	D
LDLIN2	INX	D
	MOV	A,M
	CALL	OIDLKU		Look up letter ID for OS
	STAX	D		store it
	INX	D
*
*	Check if need quote
*	
	PUSH	H
	LHLD	RCTENT
	MOV	A,M
	INX	H
	SHLD	RCTENT
	POP	H
	ANI	10000000B
	JZ	LDLIN3		if not set, skip
	MVI	A,QUOTE		single quote
	STAX	D
LDLIN3	INX	D
	INX	H
	DCR	B
	JNZ	LDLIN1		loop thru all 16 entries
	RET

ROWID	DB	0		Row ID (1..16)
RCTENT	DW	0		RCT Entry pointer

*	Entry:
*		(HL) = Sector Allocation Table
*
RTN453	SHLD	SATTMP
	INX	H		point to entry
	CALL	DIVSPR		divide by Sectors/Region (e.g. 160)
	MOV	E,A		result = region no.
	LHLD	SATTMP
	MOV	A,M		grab an entry
	INX	H		next (4 bytes/entry)
	INX	H
	INX	H
	INX	H
RTN454	CPI	SAT.ET		end?
	RZ			yes, done
	
	STA	UNK456		save allocation ID byte
	MOV	A,M
	STA	UNK457
	INX	H		point to Low addr byte
	CPI	SAT.ET		end?
	LDA	NREGNS
	JZ	RTN455		yes, end
	
	PUSH	D
	PUSH	H
	CALL	DIVSPR		divide HL by sectors/region (e.g. 160)
	DCR	A		= region no. - 1
	POP	H
	POP	D
	
RTN455	MOV	C,A		= region no. -1
	INX	H		skip triple
	INX	H
	INX	H
	LDA	UNK456
	PUSH	H
	LXI	H,BUFFER
	CALL	FILLID
	POP	H
	MOV	E,C
	INR	E
	LDA	UNK457
	JMP	RTN454
UNK456	DB	0
UNK457	DB	0
SATTMP	DW	0


*	BLDSBK - build out the superblock data structure
*
BLDSBK	MVI	A,0
	LXI	B,256
	LXI	D,SAT.A
	CALL	FILLM		Zero out Sector Allocation Table A
	
	LXI	H,SAT.A		set pointer to start of SAT A
	SHLD	SAPTR
*
*	build SAT from BUFFER?
*
	LXI	H,BUFFER
	MVI	E,0
	MVI	B,0
RTN460	MOV	A,M
	CPI	ENDFLG		end flag?
	JZ	RTN462		yes, done
	ANI	10000000B	start of partition?
	JZ	RTN461		no, go to next
*
*	found a new partition
*
	MOV	A,M
	ANI	00011111B
	INR	B
	
	PUSH	B
	PUSH	D
	PUSH	H
	LHLD	SAPTR		get SAT pointer
	MOV	M,A		store byte
	INX	H
	MOV	A,E
*
*	(HL) = starting sector (in SAT)
*	E = allocation ID byte?
*
	CALL	RTN464		compute/store starting sector #?
	LHLD	SAPTR		get pointer
	INX	H		skip 4 bytes to
	INX	H		next entry
	INX	H
	INX	H
	SHLD	SAPTR		and save it
	POP	H
	POP	D
	POP	B
	
RTN461	INR	E
	INX	H
	JMP	RTN460
*
*	Done, mark as last entry and fill in data
*	(last entry has total number of sectors on disk)
*
RTN462	LHLD	SAPTR
	MVI	M,SAT.ET	flag as end of table
	INX	H
	LDA	NSECT
	MOV	M,A		low byte
	INX	H
	LDA	NSECT+1
	MOV	M,A		mid byte
	INX	H
	LDA	NSECT+2
	MOV	M,A		high byte
	INX	H
	RET

SAPTR	DW	0

*
*	ENTRY:	(HL) = starting sector (in SAT)
*
RTN464	MOV	B,A

	MVI	M,0		zero the variable
	INX	H
	MVI	M,0
	INX	H
	MVI	M,0
	DCX	H
	DCX	H		restore (HL)

	PUSH	H
	LHLD	SPERRGN		store sector/region in triple
	SHLD	B3TMP3		(e.g. 160 for 4 heads, 240 for 6)
	XRA	A
	STA	B3TMP3+2	(top byte is zero)
	POP	H

	MOV	D,H
	MOV	E,L		(DE) = starting sector
	MVI	A,1
RTN465	PUSH	PSW
	ANA	B
	CNZ	RTN466
	POP	PSW
	RLC
	RC
	PUSH	PSW
	PUSH	H
	LXI	H,B3TMP3
	CALL	B3RAL		*2
	POP	H
	POP	PSW
	JMP	RTN465

RTN466	PUSH	B
	LXI	B,B3TMP3
	CALL	B3ADD		(HL) = sectors/region + (DE)
	POP	B
	RET

B3TMP3	DB	88,0,0		3-byte temporary storage

	SPACE	4,10
*
*	DIVSPR - divides a three-byte address by SPERRGN using
*		 repeated subtraction. returns quotient in A
*
*	ENTRY:	(HL) points to a triple
*
*	EXIT:	A = quotient
*
DIVSPR	MOV	A,M		Get low byte
	STA	B3TMP2		save it
	INX	H		next...
	MOV	A,M		A = mid byte	
	STA	B3TMP2+1	save it
	INX	H		next...
	MOV	A,M		A = hi byte
	STA	B3TMP2+2	save it
	
	LHLD	SPERRGN
	SHLD	B3TMP1
	XRA	A
	STA	B3TMP1+2
	STA	QUOTNT		zero counter
	
	LDA	B3TMP1+2	Hi byte
	LHLD	B3TMP1		Low & Mid bytes
	ORA	H
	ORA	L		A = Hi | Mid | Low
	JZ	DIVSP2		Is the triple is 0,0,0 ?
	LXI	B,B3TMP2
	LXI	D,B3TMP1
	LXI	H,B3TMP2
DIVSP1	CALL	B3SUB		3-byte subtract(HL) = (BC) - (DE)
	JC	DIVSP2		done.
	LDA	QUOTNT		increment counter
	INR	A
	STA	QUOTNT
	JMP	DIVSP1

DIVSP2	LDA	QUOTNT
	RET
B3TMP1	DB	95,0,0		3-byte temporary storage
B3TMP2	DB	0,0,0		3-byte temporary storage
QUOTNT	DB	0		quotient (result)


*
*	B3ADD - same as B3ADDA but preserve the registers
*
B3ADD	PUSH	B
	PUSH	D
	PUSH	H
	CALL	B3ADDA
	POP	H
	POP	D
	POP	B
	RET

*
*	B3ADDA - computes the sum of two three-byte quantities
*
*	(HL) = (BC) + (DE)
*
B3ADDA	LDAX	B		A = (BC)
	XCHG			DE <-> HL
	ADD	M		A = (BC) + (DE)
	XCHG
	MOV	M,A		(HL) = (BC) + (DE)
	INX	B		increment all 3
	INX	D
	INX	H
	
	LDAX	B
	XCHG
	ADC	M		do again but add Carry
	XCHG
	MOV	M,A
	INX	B
	INX	D
	INX	H
	
	LDAX	B
	XCHG
	ADC	M		third byte
	XCHG
	MOV	M,A
	RET
*
*	B3RAL - rotate all 3 bytes left one
*
*	ENTRY:	(HL) = triple byte quantity
*
*	EXIT:	bytes rotated left with carry (*2)
*
B3RAL	ANA	A		clear 'C'
	MOV	A,M		fetch a byte
	RAL			*2
	MOV	M,A		store it 
	INX	H		next byte
	MOV	A,M		fetch it
	RAL			*2
	MOV	M,A		store it
	INX	H		next byte
	MOV	A,M		fetch it
	RAL			*2
	MOV	M,A		store it
	DCX	H		fix pointer
	DCX	H
	RET
*
*	B3SUB - same as B3SUBA but preserve the registers
*
B3SUB	PUSH	B
	PUSH	D
	PUSH	H
	CALL	B3SUBA
	POP	H
	POP	D
	POP	B
	RET
*
*	B3SUBA - take the difference of two three-byte quantities
*
*	(HL) = (BC) - (DE)
*
B3SUBA	LDAX	B		A = (BC)
	XCHG			HL<->DE
	SUB	M		A = A - (HL)
	XCHG			HL<->DE
	MOV	M,A		store it
	INX	B
	INX	D
	INX	H
	
	LDAX	B
	XCHG
	SBB	M
	XCHG
	MOV	M,A
	INX	B
	INX	D
	INX	H
	
	LDAX	B
	XCHG
	SBB	M
	XCHG
	MOV	M,A
	RET

***	PART I/O routines
*


***	SYREAD - read via SY: driver
*
*	Entry:
*		HL = address of stored sector no.
*		DE = transfer address
*		BC = byte count
*
SYREAD	PUSH	D
	MOV	E,M
	INX	H
	MOV	D,M
	XCHG
	POP	D
	MVI	A,1
	STA	AIO.UNI		I/O unit no.
	MVI	A,DC.REA	READ

	CALL	SYDVD		call SY driver
SYLOC1	EQU	*-2
	RET


SYWRITE	PUSH	D
	MOV	E,M		Load DE from memory
	INX	H
	MOV	D,M
	XCHG			save in HL
	POP	D
	MVI	A,1
	STA	AIO.UNI		I/O unit no. = 1
	MVI	A,DC.WRI	WRITE
	CALL	SYDVD		call SY driver
SYLOC2	EQU	*-2
	RET
	
*	code below is nearly the same as in boot (use ACM file?)

*
*	DOCMD - complete sending the command block
*		(send last 5 bytes of CDR block)
*
DOCMD	PUSH	B
	PUSH	H
	CALL	DOCMD1
	POP	H
	POP	B
	RET
DOCMD1	LXI	H,CDRBLK+1
	MVI	C,5
DOCMD2	MOV	A,M
	CALL	SENDB		send a byte
	INX	H
	DCR	C
	JNZ	DOCMD2
	RET
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
*	READ2 = Read completion status from status port
*		and final byte from data port
*
*	EXIT: 	B = completion status
*		C = last byte
*
READ2	DI
	CALL	RDWAIT		Input completion status
	MOV	B,A		save in B
READ2A	CALL	RDSTAT		look for last REQ
	ANI	BS.REQ+BS.OUT+BS.LMB+BS.COM
	CPI	BS.REQ+BS.LMB+BS.COM	Wait for no OUT
	JNZ	READ2A		no, loop...
	CALL	RDDATA		input last byte
	MOV	C,A		save in C
	RET
*
*	Controller I/O routines
*
RDDATA	IN	BASE+RI.DAT
INPORT2	EQU	*-1
	RET

RDSTAT	IN	BASE+RI.BST
INPORT3	EQU	*-1
	RET

WRCTRL	OUT	BASE+RI.CON
OUPORT2	EQU	*-1
	RET

WRDATA	OUT	BASE+RI.DAT
OUPORT1	EQU	*-1
	RET

*
*	Wait for data-to-host mode, then read from data in port
*
RDWAIT	CALL	RDSTAT
	ANI	BS.REQ+BS.DTD+BS.COM
	JP	RDWAIT
	CPI	BS.REQ+BS.COM
	JNZ	RDWAIT
	JMP	RDDATA

**	ROUNDBC - Round BC pair up to next page boundary
*		(if C is not zero increment B and reset C to 0)
*
ROUNDBC	PUSH	PSW
	MOV	A,C
	ANA	A
	JNZ	RNDBC1
	POP	PSW
	RET

RNDBC1	INR	B
	MVI	C,0
	POP	PSW
	RET

**	WAITR - wait for buss ready then read data
*
WAITR	CALL	RDSTAT
	ANI	BS.REQ+BS.OUT+BS.COM
	JP	WAITR
	CPI	BS.REQ
	JNZ	WAITR
	JMP	RDDATA

**	INITC - Initialize controller
*
INITC	CALL	SEEK0		seek track 0
	RNC
	MVI	A,BC.RST	reset
	CALL	WRCTRL
	
	CALL	WAITS
	DB	BS.REQ+BS.OUT+BS.COM+BS.BSY
	DB	BS.OUT		Wait for REQ and BSY to clear
	RET

**	GETCON - Get controller's attention (and
*		send first byte of command)
*
GETCON	PUSH	B
	CALL	GETCN1
	POP	B
	RET

GETCN1	CALL	WAITS		Wait for BUSY bit to clear
	DB	BS.BSY		AND mask
	DB	0		expected result...
	RC			if 'C' - error!
	
	DI
	MVI	A,BC.SEL	Assert SEL and DATA0
	CALL	WRCTRL
GETCN2	CALL	RDSTAT
	ANI	BS.BSY		wait for BUSY bit to clear
	JZ	GETCN2		wait for arrival at "controller attention"
	MVI	A,BC.EDT	Allow enable data
	CALL	WRCTRL
	LDA	CDRBLK		Get command byte from CDR block
	CALL	SENDB		send command byte
	JMP	UNK72
                     
	DB	0

*
*	BLDCDR - Build 6-byte command descriptor block
*
*	ENTRY:	A = command byte
*		H = logical address byte 1
*		L = logical address byte 0
*		B = number of blocks
*
BLDCDR	PUSH	B	
	PUSH	D    
	PUSH	H    
	STA	CDRBLK		A contains the command
	LDA	S.LUN		Logical Unit Number
	RRC          
	RRC          
	RRC          		LUN in top 3 bits
	STA	CDRBLK+1	store LUN
	MOV	A,H  
	STA	CDRBLK+2	logical address 1
	MOV	A,L  
	STA	CDRBLK+3	logical address 0
	MOV	A,B  
	STA	CDRBLK+4	number of blocks
	XRA	A    
	STA	CDRBLK+5	clear control byte
	POP	H    
	POP	D    
	POP	B    
	RET          
*
*	SEEK0 - seek track address 0
*
SEEK0	MVI	A,D.SEK		Command: Seek 
	LXI	B,0  
	MOV	H,B  
	MOV	L,C  		seek address 0
	CALL	BLDCDR		build the command block
	CALL	GETCON		Get controller attention and send command byte
	CALL	DOCMD		Send last 5 bytes of CDR block
	JMP	CMPSTAT		check completion status

**	CMPSTAT - check completion status of command
*	
CMPSTAT	PUSH	B    
CMPST1	CALL	READ2		read two bytes
	MOV	A,C  		final byte (should be 0!)
	ANA	A    
	JNZ	CMPST1		wait for final zero byte
	EI           		interrupts back on!
	MOV	A,B  		now check completion status
	ANI	BS.PE    	parity error?
	POP	B    
	RZ           		no error
	STC          		error
	RET          
*
*	WAITS - Check and wait for status
*
*	ENTRY:	(SP+1) = mask byte
*		(SP+2) = desired result
*                    
WAITS	XTHL			Get stack pointer
	MOV	B,M		first byte in B
	INX	H    
	MOV	C,M		Second byte in C
	INX	H		fix stack and 
	XTHL			put it back
	             
	PUSH	D    
	CALL	WAITS1
	POP	D    
	RET          
*
*	WAITS1 - do the status check
*
*	ENTRY:	B = mask
*		C = expected result
*                     
WAITS1	LXI	D,0  
WAITS2	DCX	D		Really big loop timer...
	MOV	A,D  
	ORA	E    
	STC			Set 'C' in case we time out...
	RZ			Return if time is up
	CALL	RDSTAT		Read from status port
	ANA	B    		AND with the mask
	CMP	C    		CoMPare with exptected result
	JNZ	WAITS2		loop 'til obtained.
	RET          
*
*	SENDB - send a byte from the CDR block
*
*	ENTRY:	A = data byte
*
SENDB	PUSH	PSW  		Save data byte
SENDB1	CALL	RDSTAT		Read status
	ANI	BS.REQ+BS.OUT+BS.COM	Wait for ready
	JP	SENDB1
	CPI	BS.REQ+BS.OUT+BS.COM	
	JNZ	SENDB1
	POP	PSW  		restore data byte
	JMP	WRDATA		OK, now write to data port
*                    
*	Now wait for completion...
*                    
SENDB2	CALL	RDSTAT		Read status
	ANI	BS.REQ+BS.OUT+BS.COM	Wait for ready
	JP	SENDB2
	ANI	BS.OUT+BS.COM
	CPI	BS.COM
	JNZ	SENDB2
	RET          		OK done

UNK522	PUSH	PSW
RTN523	CALL	RDSTAT
	ANI	BS.REQ+BS.OUT+BS.COM
	CPI	BS.REQ+BS.OUT
	JNZ	RTN523
	POP	PSW
	JMP	WRDATA
                     
UNK72	CALL	RDSTAT		??? how does 'M' flag ever get cleared???
	JM	UNK72
	RET          

***	RDANSW - Read answer from the console
*
*	Entry:
*		A = number of characters
*		(DE) = max number of characters
*		(DE+1) = number read
*		(DE+2)... = characters
*

RDANSW	PUSH	B
	PUSH	H
	PUSH	D
	PUSH	PSW
	SCALL	.CLRCO			clear console
	POP	PSW
	CPI	1			one character?
	JNZ	RDANS1			no
*
*	read single character
*	set console to no echo
*
	MVI	A,I.CSLMD		Console mode
	MVI	B,CSL.CHR		Character mode
	MVI	C,CSL.ECH+CSL.CHR	Clear echo mode
	SCALL	.CONSL
	JC	ERRXIT			fatal error
RDANS1	POP	D			increment the count
	INX	D			read counter
	PUSH	D
	POP	H			(HL) = read counter
	XRA	A			zero it to start
	MOV	M,A
RDANS2	SCALL	.SCIN			read a character
	JC	RDANS2
	CPI	NL			return?
	JZ	RDANS3			yes, done
	INX	D			point to next storage location
	STAX	D			store it
	INR	M			bump the count by 1
	MOV	A,M			get the count
	DCX	H			point to max
	CMP	M			compare
	INX	H			fix pointer
	JC	RDANS2			still less - OK, keep going
RDANS3	MVI	A,1
	DCX	H
	CMP	M			were we reading only 1 byte?
	JNZ	RDANS4
*
*	if reading only one byte then need to reset console mode
*
	MVI	A,I.CSLMD		Console mode
	MVI	B,0			clear bits
	MVI	C,CSL.ECH+CSL.CHR	clear Echo and Char mode
	SCALL	.CONSL
	JC	ERRXIT
	
RDANS4	POP	H
	POP	B
	RET

***	PRNMSG - clear type ahead buffer and
*		print message
*
*	ENTRY:	DE = message buffer
*
PRNMSG	PUSH	B
	PUSH	H
	PUSH	PSW
	PUSH	D
	SCALL	.CLRCO		Clear type ahead buffer
	POP	D
	XCHG
	POP	PSW
	JZ	PRMSG1
	CALL	RTN588
	JNZ	PRMSG2
PRMSG1	SCALL	.PRINT		print message
PRMSG2	POP	H
	POP	B
	RET


***	PART Utilities
*

	
*
*	Fills the region control table with an OS ID byte
*	starting at offset specified in register E and ending
*	at offset specified in register C.  The 8th bit is set
*	on the first entry to indicate the start of a new area.
*
*	ENTRY:	A = fill byte (OS ID)
*		C = ending offset
*		E = starting offset
*		HL = buffer
*
FILLID	MVI	D,0		zero top byte
	DAD	D		HL = HL + DE
	ORI	10000000B	Set 8th bit (start of new OS)
	
FILLID1	MOV	M,A		Store OS ID byte

	PUSH	PSW
	MOV	A,E		Get the count
	CMP	C		compare count to max
	JZ	FILLID2		If equal, done
	POP	PSW

	ANI	01111111B	clear 8th bit
	INR	E		bump counter
	INX	H		bump memory pointer
	JMP	FILLID1		and loop...

FILLID2	POP	PSW
	RET


RTN536	MVI	D,0
	DAD	D
RTN537	MOV	A,M
	ORI	10000000B	set 8th bit
	MOV	M,A
	MOV	A,E
	CMP	C
	RZ
	INR	E
	INX	H
	JMP	RTN537
*
*	ENTRY: 	A = OS ID
*
RTN538	LXI	H,BUFFER
	LXI	D,RCT.A		Region Control Table (A)
	MOV	B,A		save the ID
RTN539	MOV	A,M
	CPI	ENDFLG		end marker?
	RZ			yes, exit
	ANI	00011111B	Mask out reserved bits
	CMP	B		match to OS ID?
	JNZ	RTN540		no, keep looping
*
*	match!
*
	MOV	A,M
	ANI	11100000B	clear partition ID bits
	ORI	SAT.UA		Mark as Unallocated
	MOV	M,A		store it
	LDAX	D
	ORI	10000000B	set the dirty bit
	STAX	D
RTN540	INX	D
	INX	H
	JMP	RTN539

*	ODILKU - Look up Letter ID for OS
*
*	ENTRY: A = ID (0, SAT.UA, or 1-15)
*
*	EXIT: A = letter (S, U, or A-P)
*	
OIDLKU	ANI	00011111B	mask to low 5 bits
	ORA	A		0 = System
	JZ	OIDLK1		system partition
	CPI	SAT.UA		unallocated flag
	JZ	OIDLK2		unallocated partition
	ADI	'A'-1		A through P
	RET
OIDLK1	MVI	A,'S'		System
	RET
OIDLK2	MVI	A,'U'		Unallocated
	RET

***	HEXVAL - Convert Hexadecimal string to value
*
*	Entry:
*		(HL) = hex string
*		BC = hex string length
*
*	Exit:
*		DE = result
*		'Z' = success
*
HEXVAL	LXI	D,0		zero the result
HEXVL1	MOV	A,B
	ORA	C
	RZ			BC == 0 Done!

	MOV	A,M		fetch a character
	INX	H		and point to next
	SUI	'0'		remove ASCII bias
	JC	HEXVL2		non numeric
	CPI	10		0-9?
	JC	HEXVL3		yes
	ANI	UCMASK		assume A-F
	SUI	'A'-'9'-1
	CPI	10		'A' = 10
	JC	HEXVL2
	CPI	16		'F' = 15
	JC	HEXVL3		<16 ... OK
*
*	error
*
HEXVL2	ORI	1
	RET
*
*	0-9
*
HEXVL3	PUSH	H		save string pointer
	XCHG			HL = DE (temporarily)
	DAD	H		*2
	DAD	H		*4
	DAD	H		*8
	DAD	H		*16
	XCHG			DE = cumulative result
	MOV	L,A
	MVI	H,0		HL = decimal digit (0-15)
	DAD	D		add the value
	XCHG			put it back in DE
	POP	H		restore string pointer
	DCX	B		decrement the count
	JMP	HEXVL1		and loop...

*	this code shared with boot - possibly make ACM?
*                    
*	Get numeric value from ASCII string
*                    
*	ENTRY:	BC = count
*		(HL) = string
*                    
*	EXIT:	DE = number
*                    
GETNUM	LXI	D,0		zero the value
                     
GETNM1	MOV	A,B  
	ORA	C		count = 0?
	RZ			yes, done
	MOV	A,M		get character
	INX	H		point to next
	SUI	'0'		convert to decimal
	JC	GETNM2
	CPI	10   
	JC	GETNM3
GETNM2	ORI	1		exit, non numeric found...
	RET          
*                    
*	have 0-9     
*                    
GETNM3	PUSH	H    
	LXI	H,0		HL = 0
	DAD	D		HL = DE
	DAD	H		HL = DE*2
	DAD	H		HL = DE*4
	DAD	D		HL = DE*5
	DAD	H		HL = DE*10
	MOV	E,A  
	MVI	D,0		DE = number
	DAD	D		HL = DE*10+number
	XCHG			DE = DE*10+number
	POP	H    
	DCX	B		count down
	JMP	GETNM1		and loop
	
	XTEXT	BCRC

*	this code shared with boot - possibly make ACM?
*                    
*	ISANUM - Test if string is alphanumeric
*                    
*	ENTRY:	BC = length
*		(HL) = string
*	             
*	EXIT:	DE = length processed
*		'Z' set if alphanumeric
*                    
ISANUM	LXI	D,0		length = 0
                     
ISANM1	MOV	A,B  
	ORA	C    
	RZ			count = 0? (exit)
	             
	MOV	A,M		fetch a byte
	CPI	'0'  
	JC	ISANM3		< 0 -> non alpha
	CPI	'9'+1
	JC	ISANM2		0-9 ... OK!
	CPI	'A'  
	JC	ISANM3		< 'A' -> non alpha
	CPI	'Z'+1
	JC	ISANM2		A-Z ... OK!
	CPI	'a'  
	JC	ISANM3		< 'a' -> non alpha
	CPI	'z'+1
	JNC	ISANM3		> 'z' -> non alpha
*                    
*	isalpha [0..9; A..Z; a..z]
*                    
ISANM2	INX	D		bump length count
	INX	H		point to next storage location
	DCX	B		count down
	JMP	ISANM1		and loop...
                     
ISANM3	ORI	1		
	RET          

*	this code shared with boot - possibly make ACM?
*                    
*	STRCMP - Compare two strings (case insensitive)
*                    
*	ENTRY:	BC = string length
*		(HL) = string1
*		(DE) = string2
*                    
*	EXIT:	'Z' set if same, otherwise different
*                    
STRCMP	MOV	A,B  
	ORA	C		check counter
	RZ			return if done
	MOV	A,M		character from string1
	CPI	'a'  
	JC	STRCP1		< 'a'? 
	ANI	UCMASK		map to UPPER case
STRCP1	STA	STRTMP		save it
	LDAX	D		character from string2
	CPI	'a'  
	JC	STRCP2		< 'a'?
	ANI	UCMASK		map to UPPER case
STRCP2	PUSH	H    
	LXI	H,STRTMP
	CMP	M		compare the two
	POP	H    
	RNZ			not equal? return with 'Z' clear!
	INX	H		else next string1
	INX	D		next string2
	DCX	B		decrement the count
	JMP	STRCMP		and loop...

STRTMP	DB	0		temporary st

RTN561	MVI	B,0
RTN562	MOV	A,M
	CPI	11111111B
	RZ
	ANI	10000000B
	JZ	RTN563
	INR	B
RTN563	INX	H
	JMP	RTN562

*	this code shared with boot - possibly make ACM?
*                    
*	MOVEB - Move pre-determined number of bytes from one location to another
*                    
*	ENTRY:	BC = count
*		(HL) = source
*		(DE) = destination
*                    
*	EXIT:	'Z' set if count == 0
*                    
MOVEB	MOV	A,B  
	ORA	C    
	RZ			if BC==0 return
	MOV	A,M		fetch a byte
	STAX	D		store a byte
	INX	H		next source
	INX	D		next destination
	DCX	B		count down...
	JMP	MOVEB		loop

*	XTEXT	DU66

	SPACE	4,10
**	$DU66 - UNSIGNED 16 / 16 DIVIDE.
*
*	(HL) = (BC)/(DE)
*
*	ENTRY	(BC), (DE) PRESET
*	EXIT	(HL) = RESULT
*		(DE) = REMAINDER
*	USES	ALL

$DU66	MOV	A,D		TWOS COMPLEMENT (DE)
	CMA
	MOV	D,A
	MOV	A,E		
	CMA
	MOV	E,A
	INX	D
	MOV	A,D
	ORA	E
	JZ	DU665		IF DIVIDE BY 0
	XRA	A

*	SHIFT (DE) LEFT UNTIL:
*
*	1) DE > BL
*	2 OVERFLOW

DU661	MOV	H,D
	MOV	L,E
	DAD	B
	JNC	DU662		IS TOO LARGE
	INR	A		COUNT SHIFT
	MOV	H,D
	MOV	L,E
	DAD	H
	XCHG			(DE) = (DE)*2
	JC	DU661		IF NOT OVERFLOW

*	(DE) OVERFLOWED, PUT IT BACK.

	XCHG
	DCR	A		REMOVE EXTRA COUNT

*	READY TO START SUBTRACTING. (A) = LOOP COUNT

DU662	MOV	H,B		(H,L) = WORKING VALU
	MOV	L,C
	LXI	B,0		(BC) = RESULT
DU663	PUSH	PSW		SAVE (A)
	DAD	D
	JC	DU664		IF SUBTRACT OK
	MOV	A,L		ADD BACK IN
	SUB	E
	MOV	L,A
	MOV	A,H
	SBB	D
	MOV	H,A
DU664	MOV	A,C
	RAL
	MOV	C,A
	MOV	A,B
	RAL
	MOV	B,A

*	RIGHT SHFT (DE)

	STC
	MOV	A,D
	RAR
	MOV	D,A
	MOV	A,E
	RAR
	MOV	E,A
	POP	PSW
	DCR	A
	JP	DU663		IF NOT DONE
DU665	XCHG			(D,E) = REMAINDER
	MOV	H,B		(HL) = RESULT
	MOV	L,C
	RET

	XTEXT	HLCPDE

***	FILLM - Fill memory with a byte
*
*	Entry:
*		A = fill character
*		BC = count
*		(DE) = destination
*
*	Uses:	L
*
FILLM	MOV	L,A
	MOV	A,B
	ORA	C		BC == 0?
	MOV	A,L
	RZ			Yes, done!
	STAX	D		Store byte
	INX	D
	DCX	B
	JMP	FILLM

***	ADCRLF - append CR, LF and end text (200Q)
*	to buffer
*
ADCRLF	MVI	A,CR
	STAX	D
	INX	D
	MVI	A,LF
	STAX	D
	INX	D
	MVI	A,200Q		flag end of text
	STAX	D
	INX	D
	RET

**	FINDEND - search buffer for end flag
*	(8th bit set).  keep track of that location
*	(next available) and how many OS slots are left
*
FINDEND	MVI	D,0
	LXI	H,BUFFER
FINDE1	INX	H
	INR	D
	MOV	A,M		fetch a byte
	ANI	10000000B	8th bit set?
	JZ	FINDE1		no keep looking
	SHLD	LOCTEMP		save location
	MOV	A,D		save counter
	STA	NXTAV
	LDA	NREGNS
	DCR	D
	SUB	D
	STA	NLEFT		number left to allocate
	RET

LOCTEMP	DW	0
NXTAV	DB	0
NLEFT	DB	0

HRDCPY	CALL	ASKPRN		Ask if user wants hard copy
	RZ			'Z' -> no, done
*
*	Print hard copy
*
	CALL	RTN585
	CALL	DSPRAM		Display Region Allocation Map
	CALL	RTN586
	RET
*
*	Print out region allocation map?
*
ASKPRN	PUSH	D
	LXI	D,P.PRAM
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Read answer (Y/N)
*
	LXI	D,RDMAX
	MVI	A,1		1 character answer
	STAX	D
	CALL	RDANSW
	LDA	RDAMT
	ORA	A
	JZ	RTN581		Default <N>
	LDA	RDBUF		get the answer from buffer
	ANI	UCMASK		mask to upper case
	CPI	'Y'
	JZ	RTN582
	CPI	'N'
	JZ	RTN581
*
*	Ring bell and keep trying
*
	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	JMP	ASKPRN
*
*	No region allocation map
*
RTN581	XRA	A
	RET
*
*	Yes, print region allocation map
*
RTN582	PUSH	D
	LXI	D,P.CLS		Clear screen
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
*
*	Print menu of printer options
*	'A' through 'F'
*
	PUSH	D
	LXI	D,P.PMENU
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D

	LXI	D,RDMAX
	MVI	A,1		1 character answer
	STAX	D
	CALL	RDANSW
	LDA	RDAMT
	ORA	A
	JZ	ASKPRN		NL redisplay the question
	LDA	RDBUF		get the answer
	ANI	UCMASK		map to Upper Case
	CPI	'A'		Must be 'A' - 'F'
	JC	RTN583		< 'A' (bad)
	CPI	'F'+1
	JC	RTN584		<= 'F' (good)
*
*	Bad entry, ring the bell and keep trying
*
RTN583	PUSH	D
	LXI	D,P.BELL
	PUSH	PSW
	XRA	A
	CALL	PRNMSG
	POP	PSW
	POP	D
	JMP	ASKPRN
*
*	Print region allocation map using 
*	specified print driver ('A' - 'F')
*
RTN584	STA	LPUNIT		save print driver selection
	XRA	A
	INR	A		return '1' 
	RET


RTN585	LXI	H,DAT591
	LXI	D,LPDVD
	CALL	RTN593
	MVI	A,'Y'
	STA	YESNO
	RET


RTN586	LXI	H,DAT591
	CALL	RTN602
	MVI	A,'N'	
	STA	YESNO
	RET

YESNO	DB	'N'

RTN588	LDA	YESNO
	CPI	'N'
	RZ
	XCHG
	LXI	H,DAT591
	PUSH	D
	LXI	B,SYDVD
RTN589	LDAX	D
	INX	D
	INX	B
	ANI	10000000B
	JZ	RTN589
	POP	D
	CALL	RTN608
	XRA	A
	INR	A
	RET



LPDVD	DB	'LA:',0,0,0

*	some kind of jump or flag table

DAT591	DB	1
	DB	0,166Q
	DB	76Q,166Q
	DB	76Q,166Q
	DB	76Q,166Q
	DB	100Q
	
	DB	'LX:'
LPUNIT	EQU	*-2		printer selection ('A'-'F')



	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0

	CALL	RTN595
	RNC
	JMP	RTN621

RTN593	CALL	RTN596
	RNC
	JMP	RTN621

RTN594	CALL	RTN597
	RNC
	JMP	RTN621

RTN595	MVI	A,2
	DB	1		?? LXI B,...
RTN596	MVI	A,4
	DB	1		?? LXI B,...
RTN597	MVI	A,6
	PUSH	H
	PUSH	PSW
	MOV	B,M
	PUSH	B
	INX	H
	MOV	C,A
	MOV	A,M
	ANA	A
	MOV	A,C
	JZ	RTN598
	POP	B
	POP	PSW
	POP	H
	MVI	A,31Q
	STC
	RET


RTN598	INX	H
	MOV	C,M
	INX	H
	MOV	B,M
	INX	H
	MOV	M,C
	INX	H
	MOV	M,B
	INX	H
	MOV	M,C
	INX	H
	MOV	M,B
	INX	H
	INX	H
	INX	H
	PUSH	H
	LXI	H,DAT601
	ANA	A
	JZ	RTN599
	CALL	RTN637
	MOV	A,M
RTN599	STA	UNK600
	POP	H
	POP	PSW
	SCALL	.EXIT
UNK600	EQU	*-1
	POP	D
	POP	H
	RC
	INX	H
	MOV	M,D
	DCX	H
	RET


DAT601	DW	42002A
	DW	43004A
	DW	44006A
	DB	0

RTN602	CALL	RTN603
	RNC
	JMP	RTN621

RTN603	PUSH	H
	INX	H
	MOV	A,M
	MVI	M,0
	ANA	A
	JZ	RTN607
	ANI	4
	JZ	RTN606
	CALL	$INDL
	DW	3
	
	PUSH	D
	CALL	$INDL
	DW	1
	
	POP	H
	MOV	A,L
	SUB	E
	MOV	C,A
	MOV	A,H
	SBB	D
	MOV	B,A
	ORA	C
	JZ	RTN606
	MOV	A,C
	ANA	A
	JZ	RTN605
RTN604	MVI	M,0
	INX	H
	INR	C
	JNZ	RTN604
	INR	B
RTN605	POP	H
	MOV	A,M
	PUSH	H
	SCALL	.WRITE
RTN606	POP	H
	RC
	MOV	A,M
	PUSH	H
	SCALL	.CLOSE
RTN607	POP	H
	RET


RTN608	CALL	RTN609
	RNC
	JMP	RTN621

RTN609	PUSH	H
	CALL	RTN624
RTN610	PUSH	D
	LDA	UNK630
	ANI	4
	JZ	RTN615
	MOV	A,B
	ORA	C
	JZ	RTN615
RTN611	LHLD	UNK632
	XCHG
	LHLD	UNK635
	MOV	A,L
	SUB	E
	MOV	L,A
	MOV	A,H
	SBB	D
	MOV	H,A
	MOV	A,C
	SUB	L
	MOV	A,B
	SBB	H
	JNC	RTN612
	MOV	H,B
	MOV	L,C
RTN612	MOV	A,H
	ORA	L
	JNZ	RTN613
	PUSH	B
	LHLD	UNK631
	SHLD	UNK632
	XCHG
	LHLD	UNK635
	MOV	A,L
	SUB	E
	MOV	C,A
	MOV	A,H
	SBB	D
	MOV	B,A
	LDA	UNK629
	SCALL	.WRITE
	POP	B
	JNC	RTN611
	JMP	RTN615

RTN613	MOV	A,C
	SUB	L
	MOV	C,A
	MOV	A,B
	SBB	H
	MOV	B,A
	PUSH	B
	XTHL
	POP	B
	XTHL
RTN614	MOV	A,M
	STAX	D
	INX	D
	INX	H
	DCX	B
	MOV	A,B
	ORA	C
	JNZ	RTN614
	XCHG
	SHLD	UNK632
	POP	B
	JMP	RTN610

RTN615	POP	D
	POP	H
	JMP	RTN626

RTN616	CALL	RTN617
	RNC
	JMP	RTN621

RTN617	PUSH	H
	CALL	RTN624
	CALL	RTN618
	POP	H
	CALL	RTN626
	RET


RTN618	LHLD	UNK635
	XCHG
	LHLD	UNK632
	MOV	A,E
	SUB	L
	MOV	C,A
	MOV	A,D
	SBB	H
	MOV	B,A
	ORA	C
	RZ
RTN619	MOV	A,B
	ORA	C
	JZ	RTN620
	MVI	M,0
	INX	H
	DCX	B
	JMP	RTN619

RTN620	LHLD	UNK631
	SHLD	UNK632
	XCHG
	LHLD	UNK635
	MOV	A,L
	SUB	E
	MOV	C,A
	MOV	A,H
	SBB	D
	MOV	B,A
	LDA	UNK629
	SCALL	.WRITE
	RET


RTN621	PUSH	PSW
	CALL	$TYPTX
	DB	NL,BELL
	DB	'ERROR ON FILE',' '+200Q
	
	LXI	D,10
	DAD	D
RTN622	MOV	A,M
	INX	H
	ANA	A
	JZ	RTN623
	CALL	$WCHAR
	JMP	RTN622

RTN623	CALL	$TYPTX
	DB	' -',' '+200Q
	
	MVI	H,EC.ILR		Illegal request
	POP	PSW
	SCALL	.ERROR
	JMP	ABORT

RTN624	PUSH	D
	PUSH	B
	LXI	D,UNK629
	MVI	B,5
RTN625	MOV	A,M
	STAX	D
	INX	H
	INX	D
	MOV	A,M
	STAX	D
	INX	H
	INX	D
	DCR	B
	JNZ	RTN625
	POP	B
	POP	D
	RET


RTN626	PUSH	PSW
	PUSH	D
	PUSH	B
	PUSH	H
	MVI	B,4
	LXI	D,UNK629
RTN627	LDAX	D
	MOV	M,A
	INX	D
	INX	H
	LDAX	D
	MOV	M,A
	INX	D
	INX	H
	DCR	B
	JNZ	RTN627
	POP	H
	POP	B
	POP	D
	POP	PSW
	RET


RTN628	LDA	UNK636
	RAR
	RC
	PUSH	B
	LHLD	UNK631
	SHLD	UNK632
	XCHG
	LHLD	UNK635
	SHLD	UNK633
	MOV	A,L
	SUB	E
	MOV	C,A
	MOV	A,H
	SBB	D
	MOV	B,A
	LDA	UNK629
	SCALL	.READ
	MOV	D,B
	POP	B
	RNC
	RAL
	STA	UNK636
	CPI	3
	RAR
	RNZ
	LDA	UNK634
	SUB	D
	STA	UNK634
	ANA	A
	RET

UNK629	DB	0
UNK630	DB	0
UNK631	DW	0
UNK632	DW	0
UNK633	DB	0
UNK634	DB	0
UNK635	DW	0
UNK636	DB	0


RTN637	PUSH	B
	CPI	0
	JZ	RTN639
	MOV	B,A
RTN638	MOV	A,M
	INX	H
	CMP	B
	JZ	RTN640
	ANA	A
	INX	H
	JNZ	RTN638
	DCX	H
	DCX	H
	XRA	A
RTN639	CPI	1
RTN640	POP	B
	RET

	XTEXT	RCHAR

***	PART Messages
*
P.CLS	DB	ESC,'H'		Cursor home
	DB	ESC,'J'		Erase to end of page
	DB	200Q

P.INTRO	DB	NL,'                          '
	DB	'HEATH/ZENITH H/Z67'
	DB	NL,'            Winchester Disk Partitioning'
	DB	' Utility (PART) vers '
	DB	'0'+V.MAJ
	DB	'.'
	DB	'0'+V.MIN
	DB	NL,200Q

P.OSMAP	DB	NL,'Operating Systems'
	DB	':          '
	DB	'Region Allocation Map:',NL,200Q
	

P.HDR	DB	NL,'S. System                      '
	DB	'0  1  2  3  4  5  6  7  8  9  '
	DB	'A  B  C  D  E  F',NL
	DB	'U. Unallocated              +'
	DB	'--------------------------------'
	DB	'----------------',NL,200Q

P.SEL	DB	NL,NL,'Enter Selection................'
	DB	'<G>? ',200Q

P.CMD	DB	NL,'Set (O)S, Set (P)artition,'
	DB	' (D)isplay, (A)bandon, (S)ave?'
	DB	' <D>? ',200Q
	
P.OSID	DB	NL,'Operating System ID (S,U,A-P) <'
D.OSID	DB	' '
	DB	'>? ',200Q

P.OSNAM	DB	NL,'Operating System Name <'
D.OSNAM	DB	'                '
	DB	'>? ',200Q

P.FRREG	DB	NL,NL,'['
D.FRREG	DB	'                '
	DB	'] from Region <done>? ',200Q

P.TOREG	DB	NL,'['
D.TOREG	DB	'                '
	DB	'] to   Region <'
DAT662	DB	' '
DAT663	DB	'   >? ',200Q
P.CHKOK	DB	NL,NL,'Allocation Correct'
	DB	'..............<N>? ',200Q

P.PRDOC	DB	'(Press RETURN to display the other choice.)',200Q

P.EDBS	DB	NL,NL,'Enter Default Boot String <'
D.DBS	DB	'                   '
	DB	'>? ',200Q

P.SEBS	DB	NL,NL,'Syntax error in Boot String - '
	DB	'Please retry.',NL,NL,200Q

P.DNNA	DB	NL,'Operating System error - '
	DB	'Duplicate O.S. names not allowed.'
	DB	NL,200Q

P.FEMCB	DB	'FATAL ERROR ENCOUNTERED WHILE READING H/Z67'
	DB	NL,'MASTER CONTROL BLOCK.'
	DB	NL,NL,200Q
	
P.TABAD	DB	'WARNING - Error in Bad Sector Table A.'
	DB	NL,200Q

P.TBBAD	DB	'WARNING - Error in Bad Sector Table B.'
	DB	NL,200Q
	
P.ESBA	DB	'WARNING - Read Error in Superblock A.'
	DB	NL,200Q
	
P.CKSA	DB	'WARNING - Checksum Error in Superblock A.'
	DB	NL,200Q
	
P.ESBB	DB	'WARNING - Read Error in Superblock B.'
	DB	NL,200Q

P.CKSB	DB	'WARNING - Checksum Error in Superblock B.'
	DB	NL,200Q
	
P.ERRV	DB	'WARNING - Verification Error between '
	DB	'Checksum A and Checksum B.'
	DB	NL,200Q

P.ERRSB	DB	'FATAL ERROR ENCOUNTERED WHILE READING '
	DB	'H/Z67 SUPERBLOCKS'
	DB	NL
	DB	'SUPERBLOCKS A AND B ARE UNREADABLE - '
	DB	'PARTITIONING IMPOSSIBLE.'
	DB	NL,NL,200Q

P.WSB	DB	NL,'WARNING - Superblocks Readable, but '
	DB	200Q

P.MATCH	DB	'Checksums A and B Do Not Match.'
	DB	NL,NL,200Q

P.SBAB	DB	'Both Superblock Checksums are In Error.'
	DB	NL,NL,200Q
	
P.WRAM	DB	'          To avoid loss of your data,'
	DB	' choose the correct',NL
	DB	'          Region Allocation Map.'
	DB	NL,NL
	DB	'Press RETURN to display the first choice.'
	DB	200Q
	
P.RAMNE	DB	NL,'NOTE: The Region Allocation that you '
	DB	'have chosen is now in effect. '
	DB	NL,'Press RETURN.'
	DB	NL,200Q

P.BCSBA	DB	'WARNING - Bad Checksum on Superblock A.  '
	DB	'Recreating from Superblock B.'
	DB	NL,NL,200Q

P.BCSBB	DB	'WARNING - Bad Checksum on Superblock B.  '
	DB	'Recreating from Superblock A.'
	DB	NL,NL,200Q
	

P.SBAU	DB	'WARNING - Superblock A is Unreadable and '
	DB	'Checksum for Superblock B is In Error.'
	DB	NL,'Information in Superblock B may be corrupt.'
	DB	NL,NL,200Q
	
P.SBBU	DB	'WARNING - Superblock B is Unreadable and '
	DB	'Checksum for Superblock A is In Error.'
	DB	NL,'Information in Superblock A may be corrupt.'
	DB	NL,NL,200Q

P.RWS	DB	'Reformat was Successful.'
	DB	NL,200Q
	
P.FEEDR	DB	'FATAL ERROR ENCOUNTERED DURING REFORMAT.'
	DB	NL,200Q
	
P.RAFA	DB	'NOTE: Region Allocation information will be '
	DB	'taken from Superblock A.'
	DB	NL,NL,200Q
	
P.RAFB	DB	'NOTE: Region Allocation information will be '
	DB	'taken from Superblock B.'
	DB	NL,NL,200Q
	
P.EWSBA	DB	NL,'WARNING - Error encountered while writing '
	DB	'Superblock A.'
	DB	NL,200Q
	
P.EWSBB	DB	NL,'WARNING - Error encountered while writing '
	DB	'Superblock B.'
	DB	NL,200Q

P.EWSBC	DB	NL,'WARNING - Error encountered while writing '
	DB	'SBC.'
	DB	NL,200Q
	
P.ABORT	DB	NL,NL,'PART ABORTED - No changes applied.'
	DB	NL,NL,200Q
	
P.TERM	DB	NL,NL,'PART Terminated - Changes in effect.'
	DB	NL,NL,200Q
	
P.ISWP	DB	NL,NL,'Winchester disk is write protected '
	DB	'- No changes will be made.'
	DB	NL,200Q
	
P.PTNC	DB	NL,NL,'PART Terminated - No changes applied.'
	DB	NL,NL,200Q

P.BELL	DB	BELL,200Q

P.MNUP	DB	NL,NL,'ERROR - Maximum # of User Partitions = 62.'
	DB	NL,NL,200Q
	
P.SYSTM	DB	'System          '
P.UNALL	DB	'Unallocated     '
P.MENU	DB	NL,'   The Main Menu'
	DB	NL,NL
	DB	'A. All CP/M.',NL
	DB	'B. All HDOS.',NL
	DB	'C. All UCSD PASCAL.',NL
	DB	'D. Half CP/M, half HDOS.',NL
	DB	'E. Half CP/M, half UCSD PASCAL.',NL
	DB	'F. Half HDOS, half UCSD PASCAL.',NL
	DB	'G. Third CP/M, third HDOS, third UCSD PASCAL.',NL
	DB	'H. User-Defined Partitions.',200Q

P.PRAM	DB	NL,'Would you like a printout of the '
	DB	'Region Allocation Map ? <N>',200Q
	
P.PMENU	DB	NL,NL,'         Printer        Baud',NL
	DB	'__________________________________',NL
	DB	'                   H-14 4800 - (A)',NL,NL
	DB	'            H-24/TI-810 4800 - (B)',NL,NL
	DB	'             H-34/LA-34  300 - (C)',NL,NL
	DB	'H-44, H-54/1640, 630 RO 1200 - (D)',NL,NL
	DB	'                  MX-80 4800 - (E)',NL,NL
	DB	'                 H/Z-25 4800 - (F)',NL
	DB	'__________________________________',NL,NL
	DB	'  Which Printer is to be used ? ',200Q

	
	DB	ESC,'z'				117.247A
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	200Q
*
*	2*257 byte buffer
*
BUFFER	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0
BUFF2	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0

SYFD	DB	'SY:',0

WPFLAG	DB	0
UNK719	DW	P.UNALL
OIDTMP	DB	SAT.UA
RDMAX	DB	30		length of answer to RDANSW
RDAMT	DB	0		number of characters read
RDBUF	DB	0,0,0,0,0,0,0,0,0,0	RDANSW answer stored here
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
*
*	Rob buffer (used to construct ASCII lines for display)
*
ROWBUFF	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	
S.LUN	DB	0		Logical Unit Number
SECNO	DW	0		Sector number to read
SBCTMP	DB	0
DSKADD	DB	0,0,0		Disk addressing storage
*
*	Load address for Software Boot Code
*
SBCBUF	DB	MI.JMP
BOOADR	DW	BOOENT		jump location for boot

VMAJOR	DB	0		Major Version (1)
VMINOR	DB	0		Minor Version (1)
DEFBOOT	DB	'                   '	Default Boot
BSTA	DB	0,0,0		Bad sector table A
BSTB	DB	0,0,0		Bad sector table B
SBA	DB	0,0,0		superblock A
SBB	DB	0,0,0		superblock B
	DW	0		Sector size (256)
	DW	0		Sectors/track (40)
	DW	0		Tracks/cyl. (4)
	DW	0		Cyl./volume (244)
SPERRGN	DW	0		Sectors/region (160)
NSECT	DB	0,0,0		Number of sectors (40x4x244=39,040)
NREGNS	DB	0		Number of regions (243)
CKSSBA	DW	0		Checksum for Superblock A
CKSSBB	DW	0		Checksum for Superblock B
CKSBSA	DW	0		Checksum for Bad Sector table A
CKSBSB	DW	0		Checksum for Bad Sector table B
*	reserved (70 bytes)
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0

DATEND	EQU	*
*
*	Superblock A (3 sectors)
*
SBLKA	EQU	DATEND+128	Leave room for rest of sector
OSIDT.A	EQU	SBLKA		OS ID Table (A)
SAT.A	EQU	SBLKA+256	Sector Allocation Table (A)
RCT.A	EQU	SBLKA+512	Region Control Table (A)
*
*	Superblock B (3 sectors)
*
SBLKB	EQU	126123A
OSIDT.B	EQU	SBLKB		OS ID Table (B)
SAT.B	EQU	SBLKB+256	Sector Allocation Table (B)
*
*	Bad Sector Table
*
BSTBUFF	EQU	131123A		Buffer for Bad Sector Tables

MAXMEM	EQU	133301A		mem top

	END	PENTRY
	
