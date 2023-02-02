	TITLE	'BOOT67 - Boot program for H67 Winchester Drive'
	
*******************************************************************************
*
*	BOOT67
*
*	This is the boot program that resides on the Master Boot Record
*	of the H67 ("Winchester") drive.   Its purpose is to display a
*	menu of up to 16 possible boot options and have the user select
*	which one to boot.  It then loads the OS bootstrap off that partition.
*
*	Disassembled and commented by Glenn Roberts, August 2011
*
	XTEXT	HOSEQU
	XTEXT	DIRDEF
	XTEXT	MTR
	XTEXT	H67DEF
	XTEXT	ASCII
	XTEXT	U8250
	XTEXT	U8251
	XTEXT	ESVAL
	XTEXT	ESINT
	XTEXT	UDD

BOOLEN	EQU	9*256			Length of boot code
EXORG	EQU	USERFWA+BOOLEN		load point for Extended SBC
BAU.96	EQU	000014A			BAUD divisor for 9600
WARMB	EQU	0			Jump to 0 for warm boot
SBCXLN	EQU	256			Length or Extended SBC
MI.INIR	EQU	262355A			Z80 "INIR" instruction
*
*	SYSINT storage 
*
*	These storage areas follow the Active I/O (AIO.xxx) definitions in the
*	system internals (ESINT) include file.  They are only set by this code
*	and appear to be here to provide pointers for other utilities to use.
*
S.OSID	EQU	041126A			OS ID no. (in OS table)
S.OCCR	EQU	041127A			Occurrence no. (partition)
S.BLOW	EQU	041130A			OS boot sector (low)
S.BMID	EQU	041131A			(middle)
S.BHI	EQU	041132A			(high)
S.ALOW	EQU	041133A			Next OS boot sector (low)
S.AMID	EQU	041134A			(middle)
S.AHI	EQU	041135A			(high)
	
	ORG	USERFWA

ENTRY	JMP	RELOC			Relocate the code

	STL	'Software Boot Code'
*
*	Software Boot Code (SBC)
*
*	When the user attempts to boot a partition of the Winchester Disk, the
*	Monitor 90 boot ROM (see the MTR90 manual) loads the SBC. The SBC will
*	then perform secondary boot of the hard disk.
*
*	The first 128 bytes of sector 0 contain critical information on the 
*	structure of the disk.
*
	DB	0			Major version
	DB	0			Minor version
DFLBOO	DB	'                   '	Default boot string (19)
	DB	0,0,0			Beginning sector # of bad sector table A
	DB	0,0,0			Beginning sector # of bad sector table B
SSBA	DB	0,0,0			Beginning sector # of superblock A
SSBB	DB	0,0,0			Beginning sector # of superblock B
	DW	0			Sector size
	DW	0			Sectors per track	
	DW	0			Tracks per cylinder
	DW	0			Cylinders per volume
	DW	0			Sectors per region
	DW	0			Number of sectors (cyl/vol * sectors/rgn)
	DB	0			(byte 3 of # sectors)
	DB	0			Number of regions (0 based)
CS.SBA	DW	0			Checksum for superblock copy A
CS.SBB	DW	0			Checksum for superblock copy B
	DW	0			Checksum for bad sector table copy A
	DW	0			Checksum for bad sector table copy B
	DW	0
	
	DB	0,0,0,0,0,0,0,0,0,0	Reserved
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0

	STL	'Step 1: Relocate code'
	SPACE	4,10
*
*	Entry point
*
*	Step 1: relocate the code (last to first) to make room for
*	loading the boot code from the selected partition.  Essentiall
*	need room for two boot loads - this one plus the OS-specific one.
*
RELOC	LXI	B,BOOLEN+1	byte count
	LXI	H,USERFWA+BOOLEN+BOOLEN+1	To: leave room for TWO boot loads)
	LXI	D,ENDREL+BOOLEN	From: skip over relocation code
RELOC1	LDAX	D		fetch a byte
	DCX	H		decrement To
	DCX	D		decrement From
	MOV	M,A		store it
	DCX	B		loop 'til count is zero
	MOV	A,B
	ORA	C
	JNZ	RELOC1		Exits with (HL) pointing to START!
	
	LDA	S.BDA		Boot Device Address (set by ROM)
	ORA	A		if not set then skip
	JZ	RELOC2		otherwise overwrite port numbers...
	
	STA	INPORT1		save port numbers...
	STA	INPORT2		data port...
	STA	OUPORT1
	INR	A		increment (command port)
	STA	INPORT3
	STA	OUPORT2
RELOC2	PCHL			now jump to START!

ENDREL	EQU	*		End of relocation routine
*
*	The following code is relocated up by BOOLEN to make
*	room for loading the OS boot code off the partition
*
	STL	'Step 2: initialize console and controller'
	SPACE	4,10
*
*	Step 2: initialize console and controller
*
	ORG	USERFWA+BOOLEN

START	LXI	H,0
	DAD	SP		HL = SP
	SHLD	SPSAVE		Store SP
	LXI	SP,EXT245	set up our own stack
	
	CALL	FCU		set up console
	CALL	INITC		initialize controller
	
	STL	'Step 3: Load SBC extension'
	SPACE	4,10
*
*	Step 3: Load SBC extension
*
	LXI	B,SBCXLN	Extended SBC length
	MOV	A,B		A = 0
	ORA	C			
	JZ	GETSBA		if 0 skip
	
	MOV	A,B
	RLC
	JC	GETSBA
*
*	Attempt to read extended SBC
*
	LXI	H,EXTSBC	HL = sector address of extended SBC
	LXI	D,EXORG+EXORG-ENDREL	where to load it
	CALL	DOREAD		read it!
	JNC	GETSBA		success, next step...
*
*	Unable to load extended SBC, abort.
*	
	CALL	$TYPET
	DB	BELL
	DB	0,'FATAL ERROR - EXTENDED SBC LOAD ABORTED.',200Q	
	CALL	WAITCR		prompt user
	JMP	WARMB		warm boot...
	STL	'Step 4: Read superblock'
	SPACE	4,10
*
*	Step 4: Attempt to read superblock A
*
*	(Superblock is three 256-byte tables)
*
GETSBA	LXI	H,SSBA		sector address on disk
	LXI	D,SUPBLK
	LXI	B,256*3
	CALL	DOREAD		read it!
	JNC	CHKSBA		success, next step
*
*	Error reading superblock A
*
	CALL	$TYPET
	DB	BELL
	DB	0,'ERROR - CANNOT READ SUPERBLOCK A.',200Q	
	CALL	WAITCR
	JMP	GETSBB		try superblock B...
*
*	verify checksum for Superblock A
*
CHKSBA	LXI	D,0
	LXI	B,256*3
	LXI	H,SUPBLK
	CALL	$BCRC
	LHLD	CS.SBA
	CALL	HLCPDE
	JZ	GETBS		checksum OK, proceed
*
*	Step 4a: Read or checksum failed on superblock A...
*
*	Attempt to read superblock B
*
GETSBB	LXI	H,SSBB
	LXI	D,SUPBLK
	LXI	B,256*3
	CALL	DOREAD
	JNC	CHKSBB		OK, next step
	JMP	SBERR		else fatal error
*
*	verify checksum?
*
CHKSBB	LXI	D,0
	LXI	B,256*3
	LXI	H,SUPBLK
	CALL	$BCRC
	LHLD	CS.SBB
	CALL	HLCPDE
	JZ	GETBS		checksum OK, proceed
*
*	Error both superblock A and B failed, abort!
*
SBERR	CALL	$TYPET
	DB	BELL
	DB	0,'FATAL ERROR - CANNOT READ SUPERBLOCK B.',200Q
	CALL	WAITCR
	JMP	WARMB		fatal error, warm boot
	STL	'Step 5: Get boot string'
	SPACE	4,10
*
*	Step 5: Look for default Boot String, otherwise prompt user
*
GETBS	LHLD	SPSAVE
	LXI	D,USERFWA
	CALL	HLCPDE
	JZ	GETBS1		OK, continue!

	CALL	SKIP0		Bump HL to point to next NULL
	LHLD	SPSAVE

	CALL	PARSEB		parse boot string
	JZ	OSLOOK		parse OK, look it up now
	JMP	ERRBS		else ERROR in boot string

GETBS1	MVI	C,0		count up...
	MVI	B,19		count down...
	LXI	H,DFLBOO	Default boot
GETBS2	MOV	A,M		fetch a char
	CPI	' '
	JZ	GETBS3		break out on ' '
	INR	C		bump counters and pointer
	DCR	B
	INX	H
	JNZ	GETBS2		and loop
*
*	C = length of boot string
*
GETBS3	MOV	A,C
	ORA	A
	JZ	ASKBS		Zero length (no default) - prompt user for Boot string...
*
*	Have a default boot string
*
	MVI	B,0
	LXI	H,DFLBOO	Default boot
	CALL	PARSEB		parse boot string
	JZ	OSLOOK		OK, look it up

	CALL	$TYPET
	DB	BELL
	DB	0,'ERROR - Syntax error in Default Boot String.',200Q
	CALL	WAITCR
*
*	Step 5a: Ask user for boot string
*
ASKBS	CALL	DOMENU
	CALL	$TYPET
	DB	0,0,0,'Boot String?.............>',' '+200Q
*
*	Read boot string from console
*
	LXI	H,INPSTR
	MVI	A,19		Boot string can be up to
	CALL	RDSTR		19 characters
	LDA	STRLEN		how many were read?
	MOV	C,A
	MVI	B,0		BC = string length
	LXI	H,STRTXT	HL = string text
	CALL	PARSEB		parse it
	JZ	OSLOOK
*
*	Syntax error in boot string
*
ERRBS	CALL	$TYPET
	DB	BELL
	DB	0,'Error - Syntax error in Boot String.',200Q
	CALL	WAITCR
	JMP	ASKBS		Go back and ask again for boot string
	STL	'Step 6: Look up OS in table'
	SPACE	4,10
*
*	Step 6: look for OSNAME in the OS ID Table
*
*	EXIT:	B = index of matching entry in OS ID table
*
OSLOOK	LXI	H,SB.IDT	Point to OS ID table
	MVI	B,1		count = 1
OSLK1	PUSH	B
	PUSH	H
	LXI	B,16		string length
	LXI	D,OSNAME
	CALL	STRCMP		compare the two strings
	POP	H
	POP	B
	JZ	HAVEOS		found it! next step...
	INR	B
	LXI	D,16		offset to next entry
	DAD	D		HL = HL + 16
	MOV	A,B
	CPI	16+1		done them all?
	JNZ	OSLK1		no, keep looking
*
*	no match to specified OS
*
	CALL	$TYPET
	DB	BELL
	DB	0,'Error - Operating System not found.',200Q
	CALL	WAITCR
	JMP	ASKBS		go back and ask again for Boot string...
	STL	'Step 7: Find the OS'
	SPACE	4,10
*	Step 7: find the desired occurrence of this OS
*
*		(HL) points to entry
*
HAVEOS	LDA	OCCNUM		occurrence number
	CALL	FINDO		find specified occurrence
	JZ	HAVEOC		OK, got it...
*
*	error no such partition
*
	CALL	$TYPET
	DB	BELL
	DB	0,'Error - Partition not found.',200Q
	CALL	WAITCR
	JMP	ASKBS		go back and ask again for Boot string...
	STL	'Step 8: Initialize System Internals'
	SPACE	4,10
*
*	Step 8: Fill in system internals area with key data.
*
HAVEOC	MOV	A,B
	STA	S.OSID		OS ID
	LDA	OCCNUM		user partition #
	STA	S.OCCR		occurrence number
	MOV	A,M
	STA	S.BLOW		low byte of sector no.
	INX	H		next entry
	MOV	A,M
	STA	S.BMID		middle byte of sector no.
	INX	H		next entry
	MOV	A,M
	STA	S.BHI		high byte of sector no.
	INX	H		skip ID byte 1
	INX	H		next
	MOV	A,M
	STA	S.ALOW		low byte of NEXT entry
	INX	H		next
	MOV	A,M
	STA	S.AMID		middle byte
	INX	H		next
	MOV	A,M
	STA	S.AHI		high byte
	DCX	H
	DCX	H
	DCX	H
	DCX	H
	DCX	H
	DCX	H		restore HL to low sector byte
	STL	'Step 9: Read and execute boot code'
	SPACE	4,10
*
*	Step 9: read in the boot code
*
	LXI	B,BOOLEN	how many bytes to read
	LXI	D,USERFWA	where to load it
	CALL	DOREAD		load the boot code from partition
	JNC	HAVEO1
*
*	Problem reading boot code?
*
	CALL	$TYPET
	DB	BELL
	DB	0,'Error - Unable to read Boot Code from Partition.',200Q
	CALL	WAITCR
	JMP	ASKBS		Go back and ask again for Boot string...
*
*	Now jump to the boot code!
*
HAVEO1	LXI	SP,USERFWA
	JMP	USERFWA

EXTSBC	DB	9,0,0		sector id of extended SBC
	STL	'Disk I/O Utilities'
	SPACE	4,10
***	DOREAD - read from disk
*
*	ENTRY:	BC = byte count
*		DE = destination in memory
*		HL = sector address (3 bytes: low, mid, high)
*
DOREAD	PUSH	D
	MOV	E,M		E = low
	INX	H
	MOV	D,M		D = mid
	XCHG			DE <=> HL (HL = low, mid)
	POP	D		DE = destination
	CALL	EXT133
	CALL	UNK67
	RET
	STL	'Console I/O Utilities'
	SPACE	4,10
*
*	RDSTR - Read a string from the console. Exit on
*		Carriage Return.
*
*	ENTRY:	(HL) = address of string structure
*		A = maximum string length
*
*	String format:
*		BYTE:   max
*		BYTE:   len
*		CHAR[]: string
*
RDSTR	MOV	M,A		store max
	INX	H		point to len
	MVI	M,0		set to 0
	
	PUSH	H			
	POP	D
	INX	D		(DE) = first string character
	DCX	H		(HL) = max
	
RDSTR1	PUSH	H
	PUSH	D
	CALL	$ICTT		read a character from the console
	CALL	$TYPEC.		echo it
	POP	D
	POP	H
	CPI	CR		Carriage Return?
	JZ	RDSTR3		Yes - done!
	CPI	' '		control character?
	JNC	RDSTR2		no
	INX	H		point to len
	MVI	M,0		set len to 0!
	RET			and exit!
	
RDSTR2	STAX	D		store the character
	INX	D		point to next location
	INX	H		point to counter
	INR	M		increment it
	MOV	A,M		get it
	DCX	H		point to max
	CMP	M		count = max?
	JNZ	RDSTR1		no, keep reading
RDSTR3	RET			exit!
*
*	Data area
*
S.LUN	DB	0		LUN = 0
SS.CDB	DB	0		Console descriptor 1 = 8250, 0 = 8251	
X.CONTY	DB	0		UART config
X.BAUD	DW	0		BAUD
*
*	BC = byte count
*	DE = destination
*	HL = sector address
*
EXT133	CALL	RDSTAT
	ANI	BS.REQ+BS.BSY+BS.INT+BS.PE
	STC			prepare for possible error return
	RNZ			return if error
	
	PUSH	B
	CALL	UNK59
	MVI	A,D.REA		Read
	CALL	BLDCDR
	POP	B
	CALL	GETCON		get controller's attention and send command byte
	CALL	DOCMD		complete the command
	XCHG			HL <=> DE
	MOV	D,C
	MOV	E,B
	CALL	UNK49
	MOV	B,E
	MOV	C,D
	XCHG
	RET
*
*	Process read of data
*
*	ENTRY:	HL = buffer to read
*		DE = byte count?
*
UNK49	MVI	C,BASE
INPORT1	EQU	*-1		port no

	MOV	A,E
	ANA	A
	JZ	UNK49B
UNK49A	CALL	RDHALF		read half
	CALL	RDHALF		other half...
	DCR	E
	JNZ	UNK49A
UNK49B	MOV	A,D
	ANA	A
	RZ
	CPI	129
	JC	UNK49C		jump if 0..128
	CALL	RDHALF
	MOV	A,D
	SUI	128
UNK49C	MOV	B,A
	CALL	EXT140
	MVI	D,0
	RET
*
*	RDHALF - read half a sector (128 bytes)
*
RDHALF	MVI	B,128
EXT140	CALL	WAITR		read a byte
	MOV	M,A		store it
	INX	H		up pointer
	DCR	B		down counter
	RZ
*
*	Z80 "INIR": input to memory from port (C); loop 'til B=0
*
	DW	MI.INIR
	RET
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
CDRBLK	DB	94		000 | opcode
	DB	0		LUN | log addr2
	DB	0		log addr 1
	DB	0		log addr 0
	DB	0		num blocks
	DB	0		control

*
*	READ2 = Read two bytes from data port
*
*	EXIT: 	B = first byte
*		C = second byte
*
READ2	DI
	CALL	RDWAIT		Wait for data available, then read it
	MOV	B,A		save in B
READ2A	CALL	RDSTAT		read status port
	ANI	BS.REQ+BS.OUT+BS.LMB+BS.COM
	CPI	BS.REQ+BS.LMB+BS.COM	Wait for no OUT
	JNZ	READ2A		no, loop...
	CALL	RDDATA		read from data port
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
	ANI	BS.REQ+BS.OUT+BS.COM
	JP	RDWAIT
	CPI	BS.REQ+BS.COM		Wait for Data to Host mode
	JNZ	RDWAIT
	JMP	RDDATA		now do the read operation
*
*	If C is not zero increment B and reset C to 0
*	
UNK59	PUSH	PSW
	MOV	A,C
	ANA	A
	JNZ	UNK59A
	POP	PSW
	RET
UNK59A	INR	B
	MVI	C,0
	POP	PSW
	RET
*
*	WAITR - wait for buss ready then read data
*
WAITR	CALL	RDSTAT
	ANI	BS.REQ+BS.OUT+BS.COM
	JP	WAITR
	CPI	BS.REQ
	JNZ	WAITR
	JMP	RDDATA
*
*	Initialize controller
*
INITC	CALL	SEEK0		seek track 0
	RNC
	MVI	A,BC.RST	reset
	CALL	WRCTRL
	CALL	WAITS		wait for result
	DB	BS.REQ+BS.OUT+BS.COM+BS.BSY
	DB	BS.OUT		Wait for REQ and BSY to clear
	RET
*
*	GETCON - Get controller's attention (and
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
	JMP	UNK67
                     
UNK67	PUSH	B    
EXT171	CALL	READ2		read two bytes
	MOV	A,C  		second byte
	ANA	A    
	JNZ	EXT171		wait for 0
	EI           		interrupts back on!
	MOV	A,B  		first byte
	ANI	2    
	POP	B    
	RZ           
	STC          
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
                     
UNK72	CALL	RDSTAT		??? how does 'M' flag ever get cleared???
	JM	UNK72
	RET          
                     
***	Console Routines
*                    
*                    
	XTEXT	XICTT 
	XTEXT	XTYPET
	SPACE	4,10 
**	FCU - FIND CONSOLE USART.
*                    
*	FCU FINDS AND CONFIGURES THE CONSOLE USART.
*                    
*	ENTRY	NONE 
*	EXIT	NONE 
*	USES	A,F,(BC),(HL)
*                    
                     
FCU	XRA	A    
	OUT	SC.ACE+UR.IER	interrupts off (8250)
	OUT	SC.UART+USR	and also (8251)
	MVI	A,UC.8BW	8 bits
	OUT	SC.ACE+UR.LCR	try setting it (assuming 8250)
	IN	SC.ACE+UR.LCR	try to read it back
	CPI	UC.8BW		see if unchanged
	MVI	A,CDB.H85
	LXI	H,0  
	JNZ	FCU1		readback failed, must be 8251?
	             
	LXI	H,BAU.96	BAUD divisor hard wired for 9600
                     
	MOV	A,H  
	ANI	10000000B
	JZ	FCU0 
	LDA	X.CONTY
	ORI	CTP.2SB
	STA	X.CONTY
FCU0	MVI	A,CDB.H84	must be 8251
                     
*	Have type and BAUD rate.
*	(A) = S.CDB value
*	(HL) = BAUD rate (0 if 8251)
                     
FCU1	SHLD	X.BAUD
	STA	SS.CDB
	JMP	SCU  
                     
	XTEXT	XSCU		Setup console USART
*                    
*	Read/parse boot string
*                    
*	ENTRY:	BC = string length
*		(HL) = string text
                     
PARSEB	MOV	A,C  
	ORA	B    
	JZ	PARSB2		done?
*
*	Skip leading colon if present
*
	MOV	A,M		fetch a char
	CPI	':'  
	JNZ	PARSB1		jump if not ':'
	DCX	B		else skip over ':'
	INX	H    
                     
	MOV	A,C  
	ORA	B    
	JZ	PARSB2		done?
*
*	now parse the string
PARSB1	SHLD	BSPTR		store pointer to boot string
	PUSH	B    
	POP	H		HL = BC (length?)
	SHLD	BSLEN		store boot string length
	XRA	A    
	STA	OCCNUM		default user partition = 0
	             
	MVI	A,' '
	LXI	B,16		length
	LXI	D,OSNAME
	CALL	FILLC		blank out OS name entry
	             
	LHLD	BSLEN
	PUSH	H    
	POP	B		BC = length
	LHLD	BSPTR		HL = string
	CALL	ISANUM		see if it's alphanumeric
	MOV	A,E  
	ORA	D    
	JZ	PARSB2		not alphauneric - abort
                     
	LXI	H,16 
	CALL	HLCPDE
	JC	PARSB2		< 16 alphanumeric characters? - abort
*                    
*	have valid 16 character alphanumeric boot string
*	             
	PUSH	B    
	POP	H		HL=BC
	SHLD	BSLEN		save it
	PUSH	D    
	POP	B		BC=DE
	LHLD	BSPTR
	LXI	D,OSNAME
	CALL	MOVEB
	SHLD	BSPTR
	LHLD	BSLEN
	PUSH	H    
	POP	B    
	MOV	A,B  
	ORA	C    
	JZ	EXT200
	LHLD	BSPTR
	MOV	A,M  
	INX	H    
	DCX	B    
	CPI	';'  		Optionally followed by occurrence number
	JNZ	PARSB2		only ';' allowed!
	MOV	A,B  
	ORA	C    
	JZ	EXT200
	CALL	GETNUM		get the numeric portion (occurrence number)
	JNZ	PARSB2		error in user-defined partition # - abort!
	MOV	A,E  
	CPI	64   
	JNC	PARSB2		must be less than 64
	STA	OCCNUM		save user-defined occurrence number
EXT200	XRA	A    
	RET          
                     
PARSB2	ORI	1    
	RET          
                     
BSPTR	DW	0    		pointer to boot string
BSLEN	DW	0    		length of boot string

*                    
*	WAITCR - prompt user for CR and wait
*                    
WAITCR	CALL	$TYPET
	DB	0,0,'press return to continue...',200Q
	LXI	H,INPSTR
	MVI	A,1		read a single character
	CALL	RDSTR
	RET          
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
                     
	XTEXT	HLCPDE
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
*                    
*	FILLC - Fills a location with a character
*                    
*	ENTRY:	A = fill character
*		BC = count
*		DE = location
*                    
FILLC	MOV	L,A  
	MOV	A,B  
	ORA	C    
	MOV	A,L  
	RZ           
	STAX	D    
	INX	D    
	DCX	B    
	JMP	FILLC
*                    
*	DOMENU - print screen menu
*                    
*	(uses H19/H89 escape sequences)
*                    
DOMENU	CALL	$TYPET
	DB	ESC,'H'		Cursor home
	DB	ESC,'J'		Erase to end of page
	DB	0,'                               '
	DB	'HEATH/ZENITH H/Z67'
	DB	0    
	DB	'                       '
	DB	'Software Boot Code (SBC) vers '
	DB	'1.1',200Q
	CALL	$TYPET
	DB	0,'                              '
	DB	'Boot Option(s) Menu',0
	DB	0    
                     
	DB	'Operating systems'
	DB	':     Maximum occurrence number'
	DB	':',0
	DB	'------------------     --'
	DB	'------------------------',200Q	
	             
	LXI	H,SB.IDT	OS ID Table
	MVI	B,16		loop over 16 entries (count down)
	MVI	C,1		OS count (count up)
EXT218	PUSH	B    
	PUSH	H    
	MOV	A,M  
	CPI	' '		is it blank?
	JZ	EXT220		yep, next...
*                    
*	non blank OK entry
*                    
	CALL	OCOUNT		Count how many of these there are
	MOV	A,B  		A = OS count
	ORA	A    		none?
	JZ	EXT220
	DCR	A    
	STA	UNK98
	             
	MVI	A,' '
	LXI	B,60 
	LXI	D,BUFFER
	CALL	FILLC		blank out 60 bytes
	             
	POP	H		Source
	PUSH	H    
	LXI	D,BUFFER	Dest
	LXI	B,16		16 bytes
	CALL	MOVEB		move 'em!
	LXI	H,20 
	DAD	D    
	LDA	UNK98
	MOV	C,A  
	MVI	B,0  
	MVI	A,2  
	CALL	$UDD 
	MVI	M,200Q
	LXI	H,BUFFER
	CALL	$TYPET.
*                    
*	next...      
*                    
EXT220	POP	H    
	POP	B    
	LXI	D,16		jump 16 bytes to next entry
	DAD	D    
	INR	C		increment counter
	DCR	B		done all of them?
	JNZ	EXT218		nope, loop
	RET          
                     
                     
UNK98	DB	0    
*                    
*	FINDO - Find an OS occurrence in the partition table
*                    
*	ENTRY:	A = OS occurrence number (zero-based)
*		B = OS number from OS ID table
*                    
FINDO	MOV	C,A  
	INR	C    		Use C as count-down counter
	LXI	H,SB.SAT	Sector allocation table
FINDO1	MOV	A,M  		fetch a byte
	INX	H    		and point to next...
	CPI	1FH		end of table?
	JZ	FINDO3		yes return
	CMP	B    		is it the OS we want?
	JNZ	FINDO2		no
	DCR	C    		yes! count down
	RZ           		when zero we've found the desired occurrence no.
FINDO2	INX	H		elxe skip to next entry
	INX	H 
	INX	H    
	JMP	FINDO1		and loop...
*
*	end reached without finding match.
*
FINDO3	ORI	1    		clear 'Z'
	RET          
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
	ANI	337Q		map to UPPER case
STRCP1	STA	STRTMP		save it
	LDAX	D		character from string2
	CPI	'a'  
	JC	STRCP2		< 'a'?
	ANI	337Q		map to UPPER case
STRCP2	PUSH	H    
	LXI	H,STRTMP
	CMP	M		compare the two
	POP	H    
	RNZ			not equal? return with 'Z' clear!
	INX	H		else next string1
	INX	D		next string2
	DCX	B		decrement the count
	JMP	STRCMP		and loop...
STRTMP	DB	0		temporary storage
                     
*                    
*	SKIP0 - Skip to first null
*                    
*	ENTRY:	(HL) = character array
*                    
*	EXIT:	HL points to next NULL entry
*                    
*	USES:	A, BC
*                    
SKIP0	LXI	B,0  
SKIP1	MOV	A,M		fetch a byte
	ORA	A		test it
	RZ			if 0 -> done!
	INX	B		else up the count
	INX	H		point to next
	JMP	SKIP1		and loop
*                    
*	OCOUNT - Scan Sector allocation Table
*	and count occurrences of an OS
*                    
*	ENTRY:	C = OS ID we're counting
*	EXIT:	B = OS repetition count
*
OCOUNT	MOV	A,C  
	MVI	B,0  		repetition count
	LXI	H,SB.SAT	Sector allocation table
OCNT1	MOV	A,M  		is this an entry for the
	CMP	C  		OS ID we're seeking? 
	JNZ	OCNT2		no, don't count it
	INR	B    		yes, bump the count
OCNT2	CPI	1FH		end of table?
	RZ           		done!
	INX	H		each entry is 4 bytes
	INX	H		skip to next
	INX	H    
	INX	H    
	JMP	OCNT1
                     
	DB	200		not referenced ??

	DS	137		pad to 10 sectors...
*                    
*	String storage
*
INPSTR	DS	1
STRLEN	DS	1
STRTXT	DS	30
OSNAME	DS	16		OS ID Table Entry
OCCNUM	DS	1		User-defined occurrence no to boot from
SPSAVE	DS	2
BUFFER	DS	60
                     
SUPBLK	EQU	*		Superblock 
SB.IDT	DS	256		OS Ident table
SB.SAT	DS	256		Sector allocation table
	DS	256		Region control table
	             
	DS	612		stack area
EXT245	EQU	*			
                     
	END	ENTRY
