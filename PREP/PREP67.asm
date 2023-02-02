*********************************************************************
*
*	PREP67 - H/Z-67 Disk Preparation Utility Program
*
*	This software was originally developed by Heath/Zenith
*	Data Systems.  Its purpose is to initialize the "Winchester"
*	disk, perform a media check on the disk surface, and
*	initialize the disk tables for subsequent use with the
*	partitioning utility PART. PREP67 is normally run only once on
*	a new disk drive and of course it destroys all existing
*	content on the drive.
*
*	The source code for this utility was never published.  This
*	listing is the result of a disassembly of the code
*	by Glenn Roberts in November 2011.  All comments and label
*	naming conventions are based on a "best guess" given the
*	description and observation of how the PREP67 utility functions.
*
*	The code has been verified to assemble to produce an executable
*	that is operationally identical to the original Version
*	of the file from Heath/Zenith.
*
*	28 November 2011
*
*	The following mod's have been made to the baseline code:
*
*	1. User can specify port on command line,e.g.:
*		PREP67 170
*	2. set for 6 tracks/cyl. (from default 4)
*	3. fixed CTL-C handler bug
*	4. ORG to USERFWA
*
*	Modified to remove 8080 code		/gfrJan. 23/
*
*********************************************************************
*
*	Physical disk characteristics
*
SECSIZ	EQU	256		Bytes per sector
SPTRK	EQU	40		Sectors per track
TPCYL	EQU	6		Tracks per cylinder
SPCYL	EQU	240		Sectors per cylinder (SPTRK*TPCYL)
TOTCYL	EQU	244		Total cylinders in volume
TOTSEC	EQU	58560		Total # sectors (SPCYL*TOTCYL)
*
*	Table parameters
*
MAXBS	EQU	170		Maximum # bad sectors in bad sector table
SAT.UA	EQU	36Q		Sector Allocation Table (Unallocated sector flag)
SAT.ET	EQU	37Q		Sector Allocation Table (End of table flag)

PDFLT	EQU	170Q		Default port to use (if none specified
*
*	Class 0 Command data structure offsets
*
C0.OPC	EQU	0		Op code
C0.LUN	EQU	1		LUN (3 MSB bits)
C0.AD2	EQU	1		adr2 (5 LSB bits)
C0.AD1	EQU	2		adr1
C0.AD0	EQU	3		adr0
C0.NB	EQU	4		Number of blocks
C0.CTL	EQU	5		Control
C0.LEN	EQU	6		Length of command data block

*MI.OTIR	EQU	263355A	Z80 "OTIR" instruction	/gfr Jan. 23/
*MI.INIR	EQU	262355A	Z80 "INIR" instruction 	/gfr Jan. 23/

	XTEXT	ASCII
	XTEXT	H67DEF
	XTEXT	COMP
	XTEXT	TYPTX
	XTEXT	HOSEQU
	XTEXT	HOSDEF
	XTEXT	DIRDEF
	XTEXT	ESINT

	ORG	USERFWA

START	LXI	H,EXIT		set up CC handler
	MVI	A,3
	SCALL	.CTLC
*
*	get port parameter from command line (if any)
*
	LXI	H,0
	DAD	SP		HL = SP (points to cmd line parameters)
	MOV	A,L		Get first byte
	CPI	200Q		anything there?
	JNZ	GETPRM		Yes, get it
*
*	no port specified,use default
*
	MVI	A,PDFLT		default port
	STA	BASE67		store it
	JMP	INTRO
*
*	Get port parameter from command line
*
GETPRM	MOV	A,M		get a byte
	CPI	' '		space?
	INX	H		and point to next
	JZ	GETPRM		if space, keep looking
	DCX	H		non space (fix pointer)
*
*	Convert ASCII string to octal port value
*
	LXI	D,BASE67	DE = location to store port
	CALL	GETVAL


INTRO	CALL	$TYPTX
	DB	LF,'PREP67 for the Winchester disk.'
	DB	LF,'Copyright (c) 1981 Heath/Zenith Data Systems.'
	DB	LF,LF,'This routine is used to:'
	DB	LF,LF,TAB,'1. Initialize the Winchester disk.'
	DB	LF,TAB,'2. Perform media check on the Winchester disk surface.'
	DB	LF,TAB,'3. Initialize tables for use with the '
	DB	'partitioning utility PART.'
	DB	LF,LF,'PREP67 is a stand-alone utility. '
	DB	'It will destroy all files on'
	DB	LF,'the Winchester disk.'
	DB	LF,LF,'Do not use PREP67 until you have made a backup '
	DB	'of the files'
	DB	LF,'currently on the Winchester disk.'
	DB	LF,LF,'Proceed(yes/no)?',' '+200Q

	LXI	H,RTBUFF		point to buffer
	CALL	$RTL.			read text line
	LXI	D,ASCYES		is it "YES"?
	MVI	C,3			compare 3 characters
	CALL	$COMP
	JNZ	EXIT			no, EXIT immediately...
	
	CALL	$TYPTX
	DB	LF,'Using port <',200Q
	LXI	D,BASE67
	CALL	PROVAL
	CALL	$TYPTX
	DB	'Q>',NL,ENL
	CALL	$TYPTX			else, prompt to proceed...
	DB	LF,'Please type P to proceed:',' '+200Q
	
	LXI	H,RTBUFF		point to buffer
	CALL	$RTL.			read text line
	LXI	D,ASCIIP		is it "P"?
	MVI	C,1			compare 1 character
	CALL	$COMP
	JNZ	EXIT			no, EXIT immediately
*
*	Initialize buffers and data structures
*
	LDA	BASE67			get base port number
	STA	RDDATA+1		set port to read data from
	STA	WRDATA+1		set port to write data to
	INR	A			control/status is one more than r/w
	STA	WRCTRL+1		set port for control status write
	STA	RDSTAT+1		set port for reading status
	
	XRA	A			A = 0
	STA	CDRBLK+C0.AD2		logical addr 2 = 0
	MVI	A,11000000B		disable data error correction & retry
	STA	CDRBLK+C0.CTL		control
	
	CALL	FILPAT			fill buffer with 011011...

	XRA	A			Zero error counts
	STA	ERRCT1
	STA	ERRCT2
	STA	ERRCT3
	
	STA	SECCNT			Zero sector counter

	MOV	B,A			Fill byte = 0
	LXI	H,BUFF6
	LXI	D,6*SECSIZ		6 sectors
	CALL	FILBYT			Fill with NUL
	
	LXI	H,WRKBUF		set up buffer pointers
	SHLD	UNK191
	LXI	H,BUFF6
	SHLD	BSTPTR			next entry in bad sector table
	
	LXI	H,0
	SHLD	BSCOUNT			zero counters
	SHLD	CURSEC
	
	CALL	RESET			reset the controller
	CALL	TSTRDY			drive ready?
	CALL	DD.REC			recalibrate
	CALL	S3200			Seek sector #3200
	CALL	DD.REC			recalibrate

	CALL	$TYPTX
	DB	LF,'Initialize the disk...',LF,LF+200Q
	
	CALL	FORDRV			format drive
	CALL	DD.CTF			Check track format (0x05)
	JNC	MAIN2			normal return is for 'C' to be set! (no jump)
	CALL	DD.RSE			process request sense (to look up error)
	LDA	DSENSE			4-byte error data structure
	CPI	00100000B+T2.ILC	Illegal Command? (Type II error)
	JZ	MAIN1			yes! (that's good - should be! ??)
	XRA	A			no, clear 'C'
	JMP	MAIN2			wrong drive type, abort.

MAIN1	STC			
	JMP	MAIN2
*
*	if 'C' set then we got an "illegal command" from DT.CTF (0x05) call
*
MAIN2	JC	MAIN3			'C' expected, jump to proceed
*
*	Wrong drive type, abort
*
	CALL	$TYPTX
	DB	LF,'Wrong drive type.'
	DB	LF,'Test aborted.',LF,LF+200Q
	JMP	EXIT
*
*	Media test
*
MAIN3	CALL	$TYPTX
	DB	'Media test in progress...'
	DB	LF,LF,'Testing cylinder 000',CR+200Q

MAIN4	MVI	A,1			One block
	CALL	RTN57			load block address
	CALL	PREAD			read it
	JNC	MAIN5			OK, continue

	CALL	HAVEBS			read error - bad sector!
	JMP	MAIN8			keep looping...

MAIN5	CALL	PWRITE			write it
	JNC	MAIN6			OK, continue
	
	CALL	HAVEBS			write error - bad sector!
	JMP	MAIN8			keep looping...

MAIN6	CALL	PREAD			read it
	JNC	MAIN7			OK, continue
	
	CALL	HAVEBS			read error - bad sector!
	JMP	MAIN8			keep looping...

MAIN7	CALL	CMPSEC			compare (and mark bad if mismatch)

MAIN8	LHLD	CURSEC			increment sector counter
	INX	H
	SHLD	CURSEC
	LXI	D,TOTSEC		total number of sectors
	CALL	HLCPDE
	JNC	MAIN9			yes.
	CALL	UPDCYL			update "testing cylinder..." (if needed)
	JMP	MAIN4			and continue...
*
*	now prepare and write critical tables to disk
*	
MAIN9	CALL	RTN123
	CALL	ZTBLS			Zero out key tables
	CALL	GENBST			Create and write Bad Sector Tables
	CALL	GENSBK			Create and write Superblocks
	CALL	WRTSBC			write SBC sector
*
*	Finished! print message and exit normally
*	
	CALL	$TYPTX
	DB	LF,LF,'PREP67 complete.'
	DB	LF,LF+200Q
*
*	EXIT program
*
EXIT	XRA	A
	SCALL	.EXIT
	
	
*
*	UPDCYL - update "testing cylinder..." message (if needed)
*
UPDCYL	LDA	SECCNT			increment sector count
	INR	A
	STA	SECCNT
	CPI	SPCYL			compare to sectors/cylinder
	RC				ok, keep going
*
*	Increment cylinder count in message
*
	LXI	H,TSTCYL		point to cylinder number (ASCII)
UPDCY1	INR	M			increment it
	MOV	A,M			get the count
	CPI	'9'+1
	JC	UPDCY2
	MVI	M,'0'
	DCX	H
	JMP	UPDCY1

UPDCY2	XRA	A
	STA	SECCNT
	CALL	$TYPTX
	DB	'Testing cylinder 00'
TSTCYL	DB	'0',CR+200Q


*
*	Fill 256 bytes with pattern 011011011011011011011011
*
FILPAT	LXI	H,WRKBUF		buffer
	MVI	B,0			count = 256
FILLP1	MVI	A,01101101B		first 1/3 of pattern
	CALL	STAINC			store it
	MVI	A,10110110B		second 1/3 of pattern
	CALL	STAINC			store it
	MVI	A,11011011B		third 1/3 of pattern
	CALL	STAINC			store it
	JMP	FILLP1			loop 'til done
*
*	STAINC - Store A in memory and increment
*
*	ENTRY:	A = data value
*		B = counter
*		HL = memory location
*
*	EXIT:	B = count (decremented)
*		HL = location (incremented)
*		'Z' set if done
*
STAINC	MOV	M,A
	INX	H
	DCR	B
	RNZ
	POP	H			tricky return
	RET
*
*	Seek logical address 3200
*
S3200	MVI	A,D.SEK			Seek
	STA	CDRBLK+C0.OPC		Set op code
	MVI	A,14Q			Logical address hi byte
	STA	CDRBLK+C0.AD1		
	MVI	A,200Q			Logical address lo byte
	STA	CDRBLK+C0.AD0		
	MVI	A,1
	STA	CDRBLK+C0.NB		1 block
	CALL	DOCMD
	RNC				normal exit
*
*	error
*
	CALL	TSTRSE
	CALL	S3200
	RNC
*
*	again
*
	CALL	TSTRSE
	CALL	S3200
	RNC
*
*	give up!
*
	JC	ERRIE			Error - Internal error

*
*	Test Request Sense
*
TSTRSE	CALL	DD.RSE			process request sense
	LDA	DSENSE			check first byte
	ANI	01111111B		clear Block Address Valid bit
	MOV	B,A
	CPI	T1.IDNF			ID not found
	RZ
	MOV	A,B
	CPI	27Q
	JC	000025A			??
	MOV	A,B
	CPI	00010000B+T1.CDE	correctable data field error
	RZ
	MOV	A,B
	CPI	00010000B+T1.BBF	bad block found
	RZ
	JMP	ERRIE			Error - Internal error

	XRA	A			unreachable?
	RET
*
*	TSTRDY - test if drive is ready
*
TSTRDY	MVI	A,D.TDR			Test for Drive Ready
	CALL	CMD00			issue command...
	RNC
	
	CALL	DELAY			wait a bit...
	CALL	DELAY
	CALL	DELAY
	
	CALL	RESET			try again
	MVI	A,0
	CALL	CMD00
	RNC
*
*	second time didn't work either, error!
*
	CALL	DD.RSE			process request sense
	LDA	DSENSE
	ANI	60Q
	JZ	ERRDNR
	JMP	ERRIE			Error - Internal error
*
*	delay counter, count down from max
*
DELAY	LXI	B,377377A
DELAY1	DCX	B
	MOV	A,B
	ORA	C
	JNZ	DELAY1
	RET
*
*	Format drive
*
FORDRV	CALL	ZEROAD			Block zero, nblocks = 0
	MVI	A,D.FOR			Format
	STA	CDRBLK+C0.OPC		op code = format
	STA	CDRBLK+C0.NB		A = number of blocks
	CALL	DOCMD
	RNC
*
*	error in formatting
*
	CALL	DD.RSE			process request sense
	LDA	DSENSE
	CPI	3
	JNZ	ERRFMT			Error - during formatting
	CALL	DD.REC
	JNC	ERRWP			Error - write protected
	JMP	ERRIE			Error - internal error
*
*	CMD00 - issue command to the controller using
*		block zero and 0 block count
*
*	ENTRY:	A = command byte
*
CMD00	STA	CDRBLK+C0.OPC
	CALL	ZEROAD			Block zero, nblocks = 0
	JMP	DOCMD
*
*	Zero block address and # blocks
*
ZEROAD	LXI	H,0
	SHLD	CDRBLK+C0.AD1		store 2 bytes (hi and lo add)
	XRA	A
	STA	CDRBLK+C0.NB		num. blocks
	RET


RTN57	STA	CDRBLK+C0.NB		num. blocks
	LHLD	CURSEC
	CALL	HLSWAP			H <-> L
	SHLD	CDRBLK+C0.AD1		hi address
	RET
*
*	DOREC - recalibrate
*
DOREC	CALL	RESET			reset
	CALL	DD.REC			recalibrate
	RNC
	JC	ERRIE			Error - Internal error
*
*	Recalibrate.
*
*	Positions the R/W arm to Track 00, clears possible
*	error status in the drive.
*
DD.REC	MVI	A,D.REC			recalibrate
	CALL	CMD00
	RET
*
*	Process Request Sense
*
*	This command normally issued immediately after an error.  It returns
*	4 bytes of drive and controller sense for the specified LUN
*	(see copy block for exception)
*
DD.RSE	MVI	A,D.RSE			Request sense
	CALL	CMD00
	RET
*
*	Check track format
*
DD.CTF	MVI	A,D.CTF			Check track format
	CALL	CMD00
	RET
*
*	BEEEEP - Make six consecutive beeps on console
*
BEEEEP	CALL	$TYPTX
	DB	BELL,BELL,BELL,BELL,BELL,BELL,LF+200Q
	RET
*
*	SAVCDB - save command block
*
SAVCDB	MVI	B,C0.LEN		block length
	LXI	H,CDRBLK		from here
	LXI	D,TMPBLK		to here
	JMP	MOVEM			move 'em!
*
*	RESCDB - restore command block
*
RESCDB	MVI	B,C0.LEN		block length
	LXI	H,TMPBLK		from here
	LXI	D,CDRBLK		to here
	JMP	MOVEM			move 'em!
*
*	Process read (and handle error)
*
PREAD	CALL	DOREA			Read
	RNC				if no error, return
*
*	error handling
*	
	CALL	SAVCDB			save command block
	CALL	DORSER			request sense after read
	JNC	RTN66
	CALL	DOREC			recalibrate
	CALL	RESCDB			restore command block
	CALL	DOCMD			re-try the command
	JNC	RTN66
	CALL	DORSER
	JC	ERRIE			Error - Internal error
RTN66	CALL	RESCDB			restore  command block
	STC				indicate error
	RET
*
*	DOREA - read
*
DOREA	MVI	A,D.REA			Read
	STA	CDRBLK+C0.OPC		opcode = read
	JMP	DOCMD
*
*	Process write (and handle error)
*
PWRITE	CALL	DOWRI
	RNC
	CALL	SAVCDB			save 6 byte command block
	CALL	DORSEW			request sense after write
	JNC	RTN69
	CALL	DOREC			recalibrate
	CALL	RESCDB			restore 6 byte command block
	CALL	DOCMD			re-try the command
	JNC	RTN69
	CALL	DORSEW
	JC	ERRIE			Error - Internal error
RTN69	CALL	RESCDB
	STC
	RET
*
*	DOWRI - write
*
DOWRI	MVI	A,D.WRI			Write
	STA	CDRBLK+C0.OPC		op code = write
	JMP	DOCMD
*
*	Process request sense after error (read)
*	
DORSER	CALL	DD.RSE			process request sense
	LDA	DSENSE
	ANI	01111111B		clear Block Address Valid bit
	MOV	B,A
	CPI	00010000B		Test error
	RC				none?
	MOV	A,B
	CPI	00010111B
	JNC	RTN72
	XRA	A			clear flags
	RET

RTN72	MOV	A,B
	CPI	00011000B
	RZ				OK
	MOV	A,B
	CPI	00011001B
	RZ				OK
	
	STC				flag a problem
	RET
*
*	Process request sense after error (write)
*
DORSEW	CALL	DD.RSE			process request sense
	LDA	DSENSE
	ANI	01111111B
	MOV	B,A
	CPI	00010000B
	RZ
	MOV	A,B
	CPI	00010010B
	RZ
	MOV	A,B
	CPI	00010100B
	RZ
	MOV	A,B
	CPI	00010101B
	RZ
	MOV	A,B
	CPI	00011001B
	RZ
	STC
	RET
	
*
*	CMPSEC - compare two sectors.  if mismatch mark as bad.
*
CMPSEC	MVI	B,0			256 bytes
	LXI	H,TMPBUF
	LXI	D,WRKBUF
CMPSC1	LDAX	D
	CMP	M
	INX	D
	INX	H
	JNZ	CMPSC2			mismatch!
	DCR	B			decrement counter
	JNZ	CMPSC1			and loop 'til done
	RET
*
*	Mismatch, mark as bad
*
CMPSC2	CALL	HAVEBS			bad sector
	RET
	
	
*
*	FILBYT - Fills memory with contents of B
*
*	Entry:	(HL) = destination
*		B = fill byte
*		DE = count
*
FILBYT	MOV	M,B			Store it
	INX	H			point to next storage location
	DCX	D			decrement counter
	MOV	A,D
	ORA	E
	JNZ	FILBYT			loop 'til done
	RET
	
	
*
*	HAVEBS - process bad sector.  Bad sectors in the first
*		track are considered a fatal error, otherwise mark it
*		in bad sector table.  If too many errors abort.
*
HAVEBS	LHLD	CURSEC			bad sector no.
	MOV	A,H
	ANA	A
	JNZ	HAVBS1			> 256? (OK, flag it)
	MOV	A,L
	CPI	SPTRK			error on first track?
	JC	ERRTK0			that's considered fatal...
	
HAVBS1	LHLD	BSCOUNT			bad sector count?
	INX	H			increment it
	SHLD	BSCOUNT			and save it
	LXI	D,MAXBS+1		too many for bad sector table to hold?
	CALL	HLCPDE
	JNC	ERRTBS			Error, too many bad sectors
*
*	Add entry to Bad Sector Table
*
*
	LHLD	CURSEC
	XCHG				DE = current (bad) sector
	LHLD	BSTPTR			HL = next BST entry
	MOV	M,E			low byte
	INX	H
	MOV	M,D			mid byte
	INX	H
	XRA	A			(hi byte always 0)
	MOV	M,A
	INX	H			point to next
	SHLD	BSTPTR			and save it
	
	RET
*
*	ERRWP - Error - drive is Write Protected
*
ERRWP	CALL	BEEEEP
	CALL	$TYPTX
	DB	LF,'The Wincherster disk is write protected.'
	DB	LF,'Test aborted.',LF,LF+200Q

	JMP	EXIT
*
*	ERRFMT - Error during formatting
*
ERRFMT	CALL	$TYPTX
	DB	LF,'Error during formatting the drive.'
	DB	LF,'Test aborted.'
	DB	LF,LF+200Q
	JMP	EXIT
*
*	ERRIE - Error - internal error
*
ERRIE	CALL	BEEEEP
	CALL	$TYPTX
	DB	LF,'Internal error.'
	DB	LF,'Test aborted.'
	DB	LF,LF+200Q
	JMP	EXIT
*
*	ERRDNR - Error - drive not ready
*
ERRDNR	CALL	BEEEEP
	CALL	$TYPTX
	DB	LF,'Drive is not ready.'
	DB	LF,'Test aborted.'
	DB	LF,LF+200Q
	JMP	EXIT
*
*	ERRTK0 - Error - track 0 bad sector
*
ERRTK0	CALL	BEEEEP
	CALL	$TYPTX
	DB	LF,'Track 0 contains bad sector.'
	DB	LF,'Test aborted.'
	DB	LF,LF+200Q
	JMP	EXIT
*
*	ERRTBS - Error - too many bad sectors
*
ERRTBS	CALL	BEEEEP
	CALL	$TYPTX
	DB	LF,'Too many bad sectors in this drive.'
	DB	LF,'Test aborted.'
	DB	LF,LF+200Q
	JMP	EXIT
	
	
*
*	RDSTAT - Read status
*
RDSTAT	IN	BASE+RI.BST		Bus Status
	RET
*
*	WRCTRL - Write to control bus
*
WRCTRL	OUT	BASE+RI.CON		Control
	RET
*
*	RDDATA - Read data
*
RDDATA	IN	BASE+RI.DAT		Data In
	RET
*
*	WRDATA - Write data
*
WRDATA	OUT	BASE+RI.DAT		Data Out
	RET

*************************************************************************
*
*	DOCMD - Issue command to the controller
*
*	The command data structure (CDRBLK) should have
*	already been filled out with all key information 
*
DOCMD	DI				NO INTERRUPTS
*
*	First get controller's attention.  
*
	LXI	B,-1			loop counter		
DOCMD1	CALL	RDSTAT
	ANI	BS.BSY			Busy?
	JZ	DOCMD2			no...
	CALL	DCRBC			count down ...
	JNZ	DOCMD1			keep trying 
	JMP	ERRDNR			Error - Drive not ready
*
*	Next assert SELect and DATA0
*
DOCMD2	MVI	A,BC.SEL		Assert select
	CALL	WRCTRL			to control register
*
*	Now wait for controller to respond with BUSY...
*	
	LXI	B,-1			loop counter
DOCMD3	CALL	RDSTAT
	ANI	BS.BSY			Busy?
	JNZ	DOCMD5			yes...
	CALL	DCRBC			count down ...
	JNZ	DOCMD3			keep trying
*
*	timed out! try one more time
*
	LXI	B,-1			loop counter
DOCMD4	CALL	RDSTAT
	ANI	BS.BSY			Busy?
	JNZ	DOCMD5			yes...
	CALL	DCRBC			count down ...
	JNZ	DOCMD4			keep trying
	JMP	ERRDNR			Failed twice - Drive not ready
*
*	controller now has control of the buss
*
DOCMD5	MVI	A,BC.EDT		Assert enable data
	CALL	WRCTRL			to control register
*
*	now we're ready to output the command to the controller
*	using REQ/ACK handshaking
*	
	LXI	H,CDRBLK		point to command block
DOCMD6	CALL	RDSTAT			read status
	MOV	C,A			save it ...
	ORA	A
	JP	DOCMD6			loop 'til REQ
	ANI	BS.COM			command in progress?
	JZ	RTN118			no
	MOV	A,C			yes, get back saved byte
	ANI	BS.DTD			see if controller switched direction
	JZ	DOCMD7			is sending data, jump ...
	MOV	A,M			get next byte of command
	CALL	WRDATA			write byte to controller
	INX	H			point to next
	JMP	DOCMD6			and loop
*
*	done! - read completion status
*
DOCMD7	CALL	RDSTAT			Looking for last REQ
	ANI	BS.REQ+BS.DTD+BS.COM
	CPI	BS.REQ+BS.COM
	JNZ	DOCMD7			Wait for REQ and COM
	
	CALL	RDDATA			Input completion status
	MOV	C,A			save it..
	
DOCMD8	CALL	RDSTAT			read status
	MOV	B,A
	ANI	BS.REQ+BS.DTD+BS.COM+BS.LMB
	CPI	BS.REQ+BS.COM+BS.LMB	last message byte?
	JNZ	DOCMD8			no, keep looping
	
	CALL	RDDATA			input last byte
	MOV	D,A			save copy
	
	EI				INTERRUPTS ON
	
	MOV	A,C			Look at completion status
	ANI	ST.ERR			error?
	JZ	DOCMD9
	
	CALL	CHKRSE			if Request Sense check error count
	JMP	DOCMDD			Return with error

	MOV	A,C

*	have error
*
DOCMD9	ANI	ST.PER			is it a parity error?
	JZ	DOCMDA			no

	CALL	CHKPER			parity error
*
*	no parity error
*
DOCMDA	MOV	A,D			get the last data byte
	ORA	A			should be NUL
	JZ	DOCMDB			Good, jump
	CALL	CHKNUL

DOCMDB	MOV	A,B			last buss status
	ANI	ST.ERR			error?
	JZ	DOCMDC			not set, done
	CALL	CHKERR			host adapter parity error
*
*	Normal return ('C' clear)
*
DOCMDC	STC
	CMC
	RET
*
*	Error return ('C' set)
*
DOCMDD	STC
	RET
*
*	completion byte not NUL
*
CHKNUL	LDA	ERRCT1			error count
	INR	A			increment it
	STA	ERRCT1			and save
	CPI	2			max error?
	JNC	ERRCC			max, abort! Error - completion code non-zero
	CALL	RETRY			recalibrate
	MVI	A,0	
	STA	ERRCT1			reset error count
	RET
*
*	ERRCC - Error - Completion Code non zero
*
ERRCC	CALL	BEEEEP
	CALL	$TYPTX
	DB	LF,'Completion byte is non-zero.'
	DB	LF,'Test aborted.'
	DB	LF,LF+200Q
	JMP	EXIT
*
*	Count how many times Request Senese is called
*
CHKRSE	LDA	CDRBLK			first byte of command descriptor
	CPI	D.RSE			Request sense command?
	RNZ				no, exit
	
	LDA	ERRCT2			error count?
	INR	A			increment it and store
	STA	ERRCT2
	CPI	2
	JZ	ERRSSE			max, abort! Error - Sense status
	CALL	RETRY			recalibrate
	MVI	A,0
	STA	ERRCT2			reset error count
	RET
*
*	ERRSSE - Error - Sense Status error
*
ERRSSE	CALL	BEEEEP
	CALL	$TYPTX
	DB	LF,'Request sense status error.'
	DB	LF,'Test aborted.'
	DB	LF,LF+200Q
	JMP	EXIT
*
*	Count how many times parity error occurs
*
CHKPER	LDA	ERRCT0			get error count
	INR	A			increment
	STA	ERRCT0			save
	CPI	2			compare to max
	JZ	ERRPE			at max, abort! Error - status parity
	CALL	RETRY			else recalibrate
	MVI	A,0
	STA	ERRCT0			reset error count
	RET
*
*	ERRPE - Error - Status parity error
*
ERRPE	CALL	BEEEEP
	CALL	$TYPTX
	DB	LF,'Status parity error.'
	DB	LF,'Test aborted.'
	DB	LF,LF+200Q
	JMP	EXIT
*
*	Count how many times host adapter parity error occurs
*
CHKERR	LDA	ERRCT3			get error count
	INR	A			increment
	STA	ERRCT3			and save
	CPI	2			at max?
	JZ	ERRHAP			yes, abort! Error - Host adapter parity
	CALL	RETRY			else recalibrate and re-issue
	MVI	A,0
	STA	ERRCT3			reset error
	RET
*
*	ERRHAP - Error - Host Adapter parity
*
ERRHAP	CALL	BEEEEP
	CALL	$TYPTX
	DB	LF,'Host adapter parity error.'
	DB	LF,'Test aborted.'
	DB	LF,LF+200Q
	JMP	EXIT
*
*	Recalibrate and re-execute
*
*	Save command block, recalibrate, then restore and execute command
*
RETRY	CALL	SAVCDB			Save the command block
	CALL	DOREC			recalibrate...
	CALL	RESCDB			Restore the command byte
	JMP	DOCMD			and re-execute

	
*	do request sense?
	
RTN118	LXI	H,TMPBUF
	LDA	BASE67			get base port
	MOV	C,A
	MVI	B,0			BC = base port
	LDA	CDRBLK			first byte of command descriptor
	CPI	D.REA			read?
	JZ	RTN120
	CPI	D.WRI			write?
	JNZ	RTN119			not read or write
	LHLD	UNK191
	JMP	RTN120

RTN119	MVI	B,4
	LXI	H,DSENSE
	
RTN120	CALL	RDSTAT
	MOV	D,A
	ANI	10000000B
	JZ	RTN120
	MOV	A,D
	ANI	00010000B
	JNZ	DOCMD7
	MOV	A,D
	ANI	01000000B
	JZ	RTN121
*	Z80 "OTIR"	output from memory to port (C); loop 'til B=0
*
*	converted to 8080 equivalent	/gfr Jan. 23/
*	DW	MI.OTIR
*
*	8080 equivalent			/gfr Jan. 23/
*
	PUSH	PSW
	MOV	A,C			fetch the port number
	STA	OPORT			patch OUT instruction
OLOOP	MOV	A,M			fetch the byte to be output
	OUT	0			output the byte
OPORT	EQU	*-1
	INX	H			point to next
	DCR	B			decrement the count
	JNZ	OLOOP			loop 'til 0
	POP	PSW
	
	JMP	RTN120

*	Z80 "INIR"	input to memory from port (C); loop 'til B=0*
*
*	converted to 8080 equivalent	/gfr Jan. 23/
*RTN121	DW	MI.INIR
*	
*	8080 equivalent:		/gfr Jan. 23/
*
RTN121	PUSH	PSW
	MOV	A,C			fetch the port number
	STA	INPORT			patch IN instruction
INLOOP	IN	0			read the port
INPORT	EQU	*-1
	MOV	M,A			store in (HL)
	INX	H			next location
	DCR	B			decrement the count
	JNZ	INLOOP			loop 'til 0
	POP	PSW

	JMP	RTN120

*
*	RESET - reset the controller
*
RESET	MVI	A,BC.RST		reset
	CALL	WRCTRL
	RET
*
*	some kind of processing of bad sector table
*	create second copy? if needed?
*
RTN123	LHLD	BSCOUNT
	SHLD	UNK165
	LXI	B,BUFF6			buffer for bad sector table
	LXI	D,80
	
	LXI	H,UNK159
	SHLD	UNK158
	MVI	A,9
RTN124	MVI	M,0			fill 9 bytes with 0
	INX	H
	DCR	A
	JNZ	RTN124
	
	MVI	M,377Q			-1
	STA	UNK164
RTN125	XRA	A
	STA	UNK163
RTN126	CALL	RTN130
	CALL	HLCPDE
	JNC	RTN127
	MVI	A,1
	STA	UNK163
	JMP	RTN126

RTN127	LDA	UNK163
	ANA	A
	JNZ	RTN128
	CALL	RTN132
	MOV	A,M
	INR	A
	RZ

RTN128	LDA	UNK164
	ANA	A
	JNZ	RTN129
	DCX	B
	DCX	B
	DCX	B
	LHLD	UNK165		inc bscount
	INX	H
	SHLD	UNK165
RTN129	MVI	L,40
	MVI	H,0
	DAD	D		HL = DE + 40
	XCHG
	JMP	RTN125

RTN130	LHLD	UNK165
	MOV	A,L
	ORA	H		HL == 0?
	JNZ	RTN131		no, load HL from BC
	MVI	A,1
	STA	UNK164
	LXI	H,377377A	-1
	RET
*
*	Load HL from triple pointed to by BC
*	(third byte is ignored)
*
RTN131	DCX	H
	SHLD	UNK165
	LDAX	B
	MOV	L,A
	INX	B
	LDAX	B
	MOV	H,A		
	INX	B
	INX	B
	RET


RTN132	PUSH	D
	MVI	A,40
	CMA
	MOV	L,A			
	MVI	H,377Q
	INX	H		HL = -40
	XCHG
	DAD	D
	XCHG
	LHLD	UNK158
	XRA	A
	MOV	M,A		zero high byte
	INX	H
	MOV	M,D		mid byte
	INX	H
	MOV	M,E		low byte
	INX	H
	SHLD	UNK158
	POP	D
	RET
*
*	zero out key tables on the disk
*
ZTBLS	LXI	H,WRKBUF		buffer
	LXI	D,SPTRK*SECSIZ		40 sectors (10K!)
	MVI	B,0			fill with zero
	CALL	FILBYT			fill it!
	
	LXI	H,WRKBUF
	SHLD	UNK191
	LXI	H,0			boot/SBC
	CALL	WTRACK			write the track
	LHLD	SUPERA
	CALL	WTRACK
	LHLD	SUPERB
	CALL	WTRACK
	LHLD	BADSTB
	CALL	WTRACK
	RET
*
*	WTRACK - write a track (40 sectors)
*
*	ENTRY:	HL=block address to write to
*
WTRACK	MVI	A,SPTRK			40 sectors
	STA	CDRBLK+C0.NB
	SHLD	CDRBLK+C0.AD1		address
	CALL	PWRITE			write the track
	RET


*
*	GENBST - generate and write Bad Sector Table(s)
*
GENBST	LHLD	BSCOUNT
	SHLD	UNK190
	MOV	A,H
	ORA	L			no bad sectors?
	RZ				great, we're done!
	
	MOV	A,L			store as triple
	STA	UNK177
	MOV	A,H
	STA	UNK177+1
	XRA	A
	STA	UNK177+2

	INX	H			HL=c+1
	PUSH	H
	POP	D			DE=c+1
	DAD	D			*2
	DAD	D			*4
	SHLD	UNK190
	MOV	A,L
	ANA	A
	MOV	A,H			A = n blocks
	JZ	GNBST1
	INR	A			bump up if L!=0
GNBST1	STA	CDRBLK+C0.NB		# blocks
	LHLD	BADSTA
	SHLD	CDRBLK+C0.AD1
GNBST2	LXI	H,UNK177
	SHLD	UNK191
	CALL	PWRITE
	LHLD	CDRBLK+C0.AD1
	XCHG
	LHLD	BADSTB
	CALL	HLCPDE			BADSTB=BADSTA?
	RZ
	SHLD	CDRBLK+C0.AD1
	JMP	GNBST2
	
	
*
*	GENSBK - generate (and write) the Superblock
*
GENSBK	LXI	H,WRKBUF		First blank the OS ID table
	LXI	D,SECSIZ			one sector
	MVI	B,' '
	CALL	FILBYT			fill with blanks
	
	LXI	H,SATBUF		Now blank the sector allocation table
	LXI	D,SECSIZ			one sector
	MVI	B,0
	CALL	FILBYT			fill with NUL
*
*	Initially SAT has only 3 entries:
*
*	0   0   0   0
*	UA  0   0   0
*	ET  TOTSEC  0
*	
	MVI	A,SAT.UA		unallocated
	STA	SATBUF+4
	MVI	A,SPTRK*TPCYL
	STA	SATBUF+5
	MVI	A,SAT.ET		end of table
	STA	SATBUF+8
	LXI	H,TOTSEC
	SHLD	SATBUF+9
	
	LXI	H,WRKBUF
	SHLD	UNK191
*
*	write superblocks
*
	MVI	A,2			two blocks each
	STA	CDRBLK+C0.NB
	LHLD	SUPERA
	SHLD	CDRBLK+C0.AD1		address of buffer
	CALL	PWRITE			write Superblock A
	
	LHLD	SUPERB
	SHLD	CDRBLK+C0.AD1
	CALL	PWRITE			write Superblock B

	RET

*
*	Update and write out the Software Boot Code block
*
WRTSBC	LXI	H,SECSIZ		Sector size
	SHLD	S.SSZ
	LXI	H,SPTRK			Sectors per track
	SHLD	S.SPT
	LXI	H,TPCYL
	SHLD	S.TPC			Tracks/cylinder
	LXI	H,TOTCYL
	SHLD	S.CPV			Cylinders/volume
	LXI	H,SPCYL
	SHLD	S.SPC			Sectors per cylinder (region)
	LXI	H,TOTSEC
	SHLD	S.STOT			Total number of sectors

	XRA	A			zero third bytes
	STA	S.STOT+2
	STA	S.BSTB+2
	STA	S.SBA+2
	STA	S.SBB+2
	
	LHLD	BADSTB			Store Bad Sector Table B sector
	CALL	HLSWAP
	SHLD	S.BSTB			Bad sector table B
	
	LHLD	SUPERA			Store superblock A sector
	CALL	HLSWAP
	SHLD	S.SBA			Superblock A
	
	LHLD	SUPERB			Store superblock B sector
	CALL	HLSWAP
	SHLD	S.SBB			Superblock B
	
	LHLD	SUPERA
	LXI	B,3*SECSIZ			superblocks are 3 sectors
	MVI	A,3			3 sectors
	CALL	GETCRC
	SHLD	S.CSBA			Checksum for Superblock A

	LHLD	SUPERB			now do superblock B
	LXI	B,3*SECSIZ
	MVI	A,3
	CALL	GETCRC
	SHLD	S.CSBB			Checksum for Superblock B
	
	LHLD	BADSTB
	LXI	B,2*SECSIZ			2 sectors
	MVI	A,2
	CALL	GETCRC
	SHLD	S.CBSA			Checksum for bad sector table A
*
*	Compute checksum for Bad Sector Table B
*
	MVI	H,20			block 20
	MVI	L,0
	LXI	B,2*SECSIZ			512 bytes
	MVI	A,2			2 sectors
	CALL	GETCRC
	SHLD	S.CBSB			Checksum for bad sector table B
	
	LXI	D,128			fill second half with NUL
	LXI	H,SBCTBL+128
	MVI	B,0
	CALL	FILBYT

	LXI	H,0
	SHLD	CDRBLK+C0.AD1		Write to sector 0
	MVI	A,1			one sector
	STA	CDRBLK+C0.NB
	LXI	H,SBCTBL		point to the table
	SHLD	UNK191
	CALL	PWRITE			and write it!
	RET

*
*	GETCRC - read bytes from disk and compute CRC
*
*	ENTRY:	HL = block address
*		A = number of blocks
*		BC = byte count

GETCRC	PUSH	B			rtn140
	SHLD	CDRBLK+C0.AD1
	STA	CDRBLK+C0.NB
	CALL	PREAD
	POP	B
	LXI	H,TMPBUF
	LXI	D,0
	CALL	$BCRC
	XCHG
	RET

	XTEXT	BCRC
*
*	HLCMI - Complement HL and then increment it
*
HLCMI	MOV	A,H
	CMA
	MOV	H,A
	MOV	A,L
	CMA
	MOV	L,A
	INX	H
	RET
*
*	HLSWAP
*	
*	L <=> H
*
HLSWAP	MOV	A,L
	MOV	L,H
	MOV	H,A
	RET
*
*	MOVEM - move bytes from source to destination
*
*	ENTRY:	BC = byte count
*		HL = pointer to source
*		DE = pointer to destination
*
MOVEM	MOV	A,M			Fetch byte
	STAX	D			store it
	INX	H			next source
	INX	D			next destination
	DCR	B			count down
	JNZ	MOVEM			loop 'til zero
	RET

	XTEXT	HLCPDE

*
*	STORDE - store DE via HL
*
STORDE	MOV	M,D
	INX	H
	MOV	M,E
	RET

*
*	DCRBC - decrement BC and set flags
*
*	EXIT:	'Z' set if BC = 0
*
DCRBC	DCX	B
	MOV	A,B
	ORA	C
	MVI	A,0
	RET

	XTEXT	RTL
	XTEXT	MLU
	XTEXT	MCU
	XTEXT	RCHAR

*	GETVAL -- Get octal value from string
*
*	ENTRY:	HL = address of string value
*		DE = address of result
*
*	USES:	B, PSW
*
GETVAL	MOV	A,M		get a byte
	ORA	A		set flags
	JZ	GVDONE		if NULL then done!
	SUI	'0'		remove ASCII offset
	JC	GVDONE		If <'0' then done
	CPI	8
	JNC	GVDONE		If >'7' then also done
	MOV	B,A		keep a copy
	LDAX	D		get the working value
	ADD	A		*2
	ADD	A		*4
	ADD	A		*8
	ADD	B		+ digit
	STAX	D		save it
	INX	H		point to next digit
	JMP	GETVAL
GVDONE	RET
*
*	PROVAL - Print Octal Value
*
*	ENTRY:	DE = address of byte
*
PROVAL	LDAX	D		get byte
*
*	process first digit
*
	RLC			get top 2 bits
	RLC			(high digit)
	MOV	B,A		save working copy
	ANI	00000011B	mask to low 2 bits
	ADI	'0'		add ASCII offset
	STA	OCTVAL		store first byte in string
*
*	process second digit
*
	MOV	A,B		get back working value
	RLC			move the next 3
	RLC			bits into the low 3
	RLC
	MOV	B,A		save this copy
	ANI	00000111B	mask to low digit
	ADI	'0'		add ASCII offset
	STA	OCTVAL+1	store second byte in string
*
*	process third digit
*
	MOV	A,B		get working value
	RLC			move the last 3 bits
	RLC			into the low 3
	RLC
	ANI	00000111B	mask to low digit
	ADI	'0'		add ASCII offset
	STA	OCTVAL+2	store third byte in string
*
*	print the string
*
	CALL	$TYPTX
OCTVAL	DB	'000',200Q
	
	RET			done!
*
*	Data and buffers here ...
*
BASE67	DB	0			Base port (normally 170Q)
UNK158	DW	UNK159
UNK159	DB	0
SUPERA	DB	0,0,0			location of superblock A
SUPERB	DB	0,0,0
BADSTB	DB	0,0,377Q		??
UNK163	DB	0
UNK164	DB	0
UNK165	DW	0
*
*	Text Buffer for user q&a
*
RTBUFF	DB	0,0,0,0,0,0,0,0,0,0
ASCYES	DB	'YES'
ASCIIP	DB	'P'
	DB	0
*
*	error counters
*
ERRCT0	DB	0			parity error
ERRCT1	DB	0			non zero completion byte
ERRCT2	DB	0			request sense error
ERRCT3	DW	0			host adapter parity

BADSTA	DB	0,20			BADSTA is always at block 20

CURSEC	DW	0
BSCOUNT	DW	0			bad sector count
BSTPTR	DW	0			next entry in BST
UNK177	DB	0,0,0
*
*	6 sector fill area
*
*	1538 space buffer (6*256) +2 
*
BUFF6	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	
	DB	0,0		??

SECCNT	DB	0		Sector counter (within cylinder)

*
*	Command Descriptor Block (Class 0 commands)
*
*	Byte	| 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
*		---------------------------------
*	0         0   0   0 |      opcode
*	1            LUN    |   Logical adr 2
*	2		  Logical adr 1
*	3		  Logical adr 0
*	4		number of blocks
*	5		     control
*
CDRBLK	DB	0,0,0,0,0,0
*
*	Temporary saving space for CDR block
*
TMPBLK	DB	0,0,0,0,0,0
*
*	??
*
	DB	41Q,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0
*
*	Drive and Controller Sense data structure
*
*	Byte 0	| 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
*		  |   |   |   |
*		  |   |   |   |   <- Error Code >
*		  |   |   |   |
*		  |   |   ------- Error Type
*		  |   ----------- Spare (set to zero)
*		  --------------- Block Address Valid
*
*		| 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
*		---------------------------------
*	Byte 1      LUN     |    logical adr 2
*	Byte 2            Logical adr 1
*	Byte 3		  Logical adr 0
*
DSENSE	DB	0,0,0,0


UNK190	DW	0
UNK191	DW	0

*
*	Software Boot Code (SBC)
*
*	The first 128 bytes of sector 0 contain critical information on the 
*	structure of the disk.
*
SBCTBL	DB	0,0,0			JMP to entry
	DB	0			Major INIT version
	DB	0			Minor version
	DB	'                   '	Default boot string (19)
	DB	20,0,0			Beginning sector # of bad sector table A
S.BSTB	DB	0,0,0			Beginning sector # of bad sector table B
S.SBA	DB	0,0,0			Beginning sector # of superblock A
S.SBB	DB	0,0,0			Beginning sector # of superblock B

S.SSZ	DW	0			Sector size
S.SPT	DW	0			Sectors/track
S.TPC	DW	0			Tracks/cyl
S.CPV	DW	0			cyl/volume
S.SPC	DW	0			sectors/regn (cyl)
S.STOT	DB	0,0,0			total # sectors
S.NRGN	DB	TOTCYL-1		# regions (zero based)
S.CSBA	DW	0			Checksum superblock A
S.CSBB	DW	0			Checksum superblock B
S.CBSA	DW	0			Checksum bad sector table A
S.CBSB	DW	0			Checksum bad sector table B
	
	DB	1,377Q,0,0,0,0,0,0,0,0	Reserved (70)
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0
	
	DW	0
*
*	working buffer space
*
WRKBUF	EQU	*
SATBUF	EQU	WRKBUF+SECSIZ
*
*	leave space for 40 sectors
*
TMPBUF	EQU	SPTRK*SECSIZ+WRKBUF	177062A (0r 177061A?)

	END	START
