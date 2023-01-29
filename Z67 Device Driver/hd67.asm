***     HD67.DVD  -  H67 DEVICE DRIVER
*
*	HD67.DVD is the device driver for the hard drive portion
*	of the H67 mass storage device.
*
*	This code is based on a disassembly from the Heath DVD file.
*	Commenting informed by HUG37 device driver.
*
*       G. Roberts		17 September 2011
*

	XTEXT	ASCII
	XTEXT	ECDEF
	XTEXT	DDDEF
	XTEXT	DEVDEF
	XTEXT	MTR
	XTEXT	PICDEF
	XTEXT	DVDDEF
	XTEXT	DIRDEF
	XTEXT	HOSDEF
	XTEXT	HOSEQU
	XTEXT	ESINT
	XTEXT	ESVAL
	XTEXT	SETCAL
	XTEXT	H67DEF
	XTEXT	HROM
	
DC.GDP	EQU	100		Get Disk Parameters
DC.101	EQU	101
DC.102	EQU	102

HDMNU	EQU	8		Maximum number of units
HDCAP	EQU	DT.CW+DT.CR+DT.DD+DT.RN		Read, Write, Directory, Random

SAT.ET	EQU	1FH		End marker for Sector Allocation Table
MASK.WP	EQU	10000000B	Write Protect flag mask

MI.SBCB	EQU	102355A		Z80 instruction: HL <- HL - BC - Carry
MI.SBCD	EQU	122355A		Z80 instruction: HL <- HL - DE - Carry

IOPORT	EQU	274Q		Base I/O port

	CODE	PIC
RELZERO	EQU	*-PIC.COD
	ERRNZ	RELZERO

	DB	DVDFLV		Is a device driver
        DB	HDCAP		Device Capability
        DB	00000000B	Mounted Unit Mask
        DB      HDMNU		Max units
        DB	HDCAP		0: Full Capability
        DB	HDCAP		1: Full Capability
        DB	HDCAP		2: Full Capability
        DB	HDCAP		3: Full Capability
        DB	HDCAP		4: Full Capability
        DB	HDCAP		5: Full Capability
	DB	HDCAP		6: Full Capability
        DB	HDCAP		7: Full Capability
        DB	DVDFLV		Process Set Options

.	SET	023Q
	ERRNZ	*-.
	DS	DVD.STE-.
	
	
*       SET ENTRY
*
*       ENTRY:  A  = UNIT NO.
*               DE = LINE POINTER
*
*       EXIT:   CY SET IF ERROR
*                       A = ERROR CODE
*
*       USES:   ALL
        CPI	HDMNU-1		> Max unit?
        JNC     BADUNIT
	
        MOV     B,D		BC = line ptr
        MOV     C,E
        LXI     D,PRCTAB	Point to processor table
        LXI     H,OPTTAB	and options table
        CALL    $SOP		Process set option
        JC      SEXIT
        CALL    $SNA		Make the setting
        JZ      SEXIT		exit if no error

        MVI     A,EC.ILO	Illegal option
        STC
        JMP     SEXIT

BADUNIT	MVI     A,EC.UUN	Unknown unit number
        STC
SEXIT   RET

***	Process HELP Option
*

HELP	LXI     H,HLPMSG
        SCALL   .PRINT
        XRA     A
        RET

HLPMSG	DB	NL,NL,'H67 Hard Disk Driver Set Options:'
        DB	NL,NL,'Help'
	DB	TAB,TAB,'Type this message'
	DB	NL,NL
	DB	NL+200Q

***	SET Option Table
*

OPTTAB	DW	OPTABE
	DB	1
	DB	'HEL','P'+200Q,HELPI
OPTABE	DB	0

PRCTAB	EQU	*
HELPI	EQU	*-PRCTAB/2
	DW	HELP

	DS	DVD.ENT+RELZERO-*

**      Drive Dispatch
*
*       ENTRY:  A = Driver Function Code
*		Other registers dependent on function
*
*       EXIT:   'C' set if error
*                   A   = error code
*               'C' Clear if no error
*
*       USES:   All
*
HD67DD	PUSH    PSW
        XRA     A		first clear 'raw' mode
        STA     DC.RAW
        POP     PSW
*
*	Check for special function codes
*
        CPI     DC.GDP		get disk parameters
        JZ      GDP
*
*	these functions appear to be for debugging
*	allowing a "raw" i/o mode?  It would seem
*	this is currently disabled by the clear operation
*	above?
*
        CPI     DC.101
        CZ      RAWOFF
        CPI     DC.102
        CZ      RAWON
	
        CPI     DC.MAX
        JNC     HDILRQ
*
*	Jump to appropriate handler through table
*
        CALL    $TJMP
	CODE	-REL
.	SET	*
	CODE    +REL
	ERRNZ	*-.


	ERRNZ	*-./2-DC.REA
	DW	HDREAD		Read

	ERRNZ	*-./2-DC.WRI
	DW	HDWRIT		Write
	
	ERRNZ	*-./2-DC.RER
	DW	HDREAR		Read Regardless

	ERRNZ	*-./2-DC.OPR
	DW	HDOPNR		Open for Read
	
	ERRNZ	*-./2-DC.OPW
	DW	HDOPNW		Open for Write

	ERRNZ	*-./2-DC.OPU
	DW	HDOPNU		Open for Update

	ERRNZ	*-./2-DC.CLO
	DW	HDCLOS		Close

	ERRNZ	*-./2-DC.ABT
	DW	HDABRT		Abort

	ERRNZ	*-./2-DC.MOU
	DW	HDMOUN		Mount

	ERRNZ   *-./2-DC.LOD
	DW	HDLOAD		Load

	ERRNZ	*-./2-DC.RDY
	DW	HDREDY		Ready

	ERRNZ	*-./2-DC.MAX	Ensure all entries present 


**      HDILRQ  -  Handle illegal requests
*
*       ENTRY:  NONE
*
*       EXIT:   NONE
*
*       USES:   A,F

HDILRQ	MVI	A,EC.ILR
	STC
        RET

**      HDNOP  -  Ignore request
*
*       Functions which do not require specific
*       handling enter here
*
*       ENTRY:  NONE
*
*       EXIT:   NONE
*
*       USES:   A,F

HDNOP	EQU	*
	XRA	A
        RET
	
**      These functions are not processed by
*       the driver, But must be handled since
*       control is passed here.
*
*       ENTRY:  NONE
*
*       EXIT:   NONE
*
*       USES:   A,F
*
HDOPNR	EQU	HDNOP

HDOPNW	EQU	HDNOP

HDOPNU	EQU	HDNOP

HDCLOS	EQU	HDNOP

**      HDREDY  -  Check for device ready
*
*       ENTRY:  NONE
*
*       EXIT:   PSW     = 'C' Set if not ready
*                         'C' Clear if ready
*

HDREDY	CALL	ZEROCDR		Set up command block
        MVI     A,D.TDR		Test drive ready
        STA     CDRBLK		store in command block
        CALL    OUTCOM		send the command
        CALL    CMDRES		process the command result
        JC      HDRDY1		oops.
        LDA     .TICCNT		check for timeout??
        ANI     3
        JNZ     HDRDY1
        STC			timeout?
HDRDY1	RET

**      HDABRT  -  Process Abort request
*

HDABRT	XRA	A
        RET


**      HDREAD  -  Read from HD67 Device
*
*       HDREAD Reads sectors from the unit in AIO.UNI
*
*       ENTRY:  HL      = Sector Number
*               DE      = Transfer Address
*               BC      = Byte Count

HDREAD	MOV     A,B
        ORA     C
        JZ      HDREA3		Byte count = 0, exit
	
        PUSH    H
        MOV     H,B
        MOV     L,C		HL = byte count
        SHLD    BCOUNT		save it
        LXI     B,255		round up sector 
        DAD     B
        MOV     B,H
        MVI     C,0		BC = rounded
        POP     H
*
*	Prepare command block and issue READ
*
*	B = block count (BC = rounded byte count)
*	HL = sector number
*	DE = transfer address
*
        CALL    ZEROCDR		clear command block
        MOV     A,B		get block count
        STA     CDRBLK+4	store it
        CALL    ADTRAN		translate & save addresses
        JC      HDREA3
        MVI     A,D.REA		READ
        STA     CDRBLK
        CALL    OUTCOM		issue the command

***	HDREADX - execute the read
*
*
HDREADX	IN      IOPORT+RI.BST	input bus status
        MOV     C,A		save it for later
        ANI     BS.REQ		look for REQ
        JZ      HDREADX		else loop
	
        MOV     A,C		recall status
        ANI     BS.COM		check for COM
        JNZ     HDREA2		set, exit loop
        IN      IOPORT+RI.DAT	read the data
        ANA     A		set flags
        LHLD    BCOUNT		load byte count
        LXI     B,1
	DW	MI.SBCB		HL <- HL - BC - Carry (z80)
        JC      HDREADX
        SHLD    BCOUNT		store byte count
        STAX    D		store byte
        INX     D		up pointer
        JMP     HDREADX		and loop
*
*	finish command processing and exit
*
HDREA2	CALL    CMDRES		fininsh command processing
HDREA3	RET

**      HDREAR  -  Read Regardless
*
*       HDREAR reads from the specified device
*

HDREAR	EQU	HDREAD

**      HDWRIT  -  Process Write Request
*
*       HDWRITE Writes data to the specified device
*
*       ENTRY:  HL      = Sector Number
*               DE      = Transfer Address
*               BC      = Byte Count
*
*       EXIT:   PSW     'C' set if Error
*                            A  = EC.WF or EC.WP
*                           BC  = Bytes not transfered
*                           DE  = Next Unused Address
*                       'C' clear if no error
*                           HL  = Sector of error
*                            A = 0
*                           BC  = 0
*                           HL  = Next Logical Sector
*                           DE  = Next Unused Address
*       USES:   ALL
*
HDWRIT	MOV     A,B
        ORA     C
        JZ      HDWRI3		zero bytes

*
*	set up command block
*
*	BC = byte count
*
        PUSH    H		save sector no.
        CALL    ZEROCDR		clear the command block
        LXI     H,255		round up to sector count
        DAD     B			
        MOV     B,H		B = sector count
        MVI     C,0
        MOV     A,B		A = sector (block) count
        STA     CDRBLK+4	# blocks
        POP     H		restore sector no.

        CALL    RTN76
        CALL    ADTRAN		translate & save address
        JC      HDWRI8
        LDA     FLAGB		check for write protect
        ANI     MASK.WP
        JNZ     HDWRI6
*
*	issue command
*
        MVI     A,D.WRI		Write
        STA     CDRBLK
        CALL    OUTCOM		Issue the command
*
*	data write loop...
*
HDWRI1	IN      IOPORT+RI.BST	input from bus status
        MOV     C,A		save
        ANI     BS.REQ
        JZ      HDWRI1		wait for REQ
        MOV     A,C		get back status
        ANI     BS.COM		check for COM
        JNZ     HDWRI2		command completion status rec'd
        LDAX    D		else grab a byte
        OUT     IOPORT+RI.DAT	output it
        INX     D		point to next
        JMP     HDWRI1		and repeat...
*
*	command completion
*
HDWRI2	CALL    CMDRES		finish command processing
        JMP     HDWRI8
*
*	writing zero bytes (check for write protect by
*	reading sector 0, then attempting to write it back!)
*
*	first read sector 0...
*
HDWRI3	LXI     B,256		1 sector
        LHLD    S.SCR		point to buffer
        XCHG			(DE) = buffer
        LXI     H,0		sector 0
        CALL    HDREAD		read it
        JC      HDWRI7		error?
*
*	now try to write it back!
*
        LXI     B,256		1 sector
        LHLD    S.SCR		point to buffer
        XCHG			(DE) = buffer
        LXI     H,0		Sector 0
        CALL    HDWRIT		write it
        JC      HDWRI4
*
*	success!
*
        LHLD    FLAGS
        MOV     A,M
        ANI     MASK.WP		Clear other flags
        MOV     M,A
        JMP     HDWRI5
*
*	handle errors
*
HDWRI4	CPI     EC.WP		write protect error?
        JNZ     HDWRI7		no, other error.
        LHLD    FLAGS
        MOV     A,M
        ORI     MASK.WP		SET wp flag
        MOV     M,A
        MVI     A,1
        STA     WPROT
        JMP     HDWRI6
*
*	Exit normally
*
HDWRI5	XRA     A
        JMP     HDWRI8
*
*	Exit with write protect error
*
HDWRI6	MVI     A,EC.WP		write protected
HDWRI7	STC
HDWRI8	RET

**      HDMOUN  -  Process the MOUNT request
*
*       ENTRY:  HL      = Sector Number

HDMOUN  LDA     AIO.UNI
        CALL    UNILKUP
        CMP     M
        JNZ     HDMOU1
	
        LDA     WPROT
*	ORA	A			missing statement???
        JZ      HDMOU2
        MVI     A,BC.RST
        OUT     IOPORT+RI.CON
        XRA     A
        OUT     IOPORT+RI.CON
        XRA     A
        JMP     HDMOU2

HDMOU1	MVI     A,EC.UUN		Unknown Unit Number
        STC
HDMOU2	RET

**      HDLOAD  -  Process LOAD request

HDLOAD	XRA     A
        STA     WPROT
*
*	Read block 0 on the disk
*
        CALL    ZEROCDR		Zero out address
        MVI     A,D.REA		Read
        STA     CDRBLK
        MVI     A,0
        STA     BCOUNT
        MVI     A,1		1 block
        STA     BCOUNT+1
        STA     CDRBLK+4	read 1 block
        CALL    OUTCOM		issue the command

        LHLD    S.SCR		scratch buffer
        XCHG			DE = transfer address
        CALL    HDREADX		Read it!
        JC      HDLOA7		error?
*
*	Now look up the SuperBlock (A) and read
*	first 2 blocks of it.
*
        LHLD    S.SCR		Point to the sector
        LXI     D,30		Address of super block A
        DAD     D		HL = address of SBA address
        LXI     D,SBAADR	local storage for it	
        XCHG
        LXI     B,3		3 bytes (low, mid, high bytes)
        CALL    $MOVE		move address into local storage
        LHLD    SBAADR		HL = SBA address
        MOV     A,H
        STA     CDRBLK+2	mid byte
        MOV     A,L
        STA     CDRBLK+3	low byte
        MVI     A,0
        STA     BCOUNT
        MVI     A,2		2 blocks
        STA     BCOUNT+1
        STA     CDRBLK+4
        CALL    OUTCOM		issue the command
*
*	Read 2 blocks (OD ID table and Sector Alloc. Table)
*
        LHLD    S.SCR
        XCHG
        CALL    HDREADX
        JC      HDLOA7
*
*	This code scans for partitions named HDOSx where 0<=x<8 and
*	builds the Unit Table.  Each entry consists of 8 bytes:
*
*		UnitID
*		partition start (low, med, hi)
*		partition end+1 (low, med, hi)
*		flag byte (used for write protect; initally 0)
*
*	The outer loop scans the OS ID table to find HDOS partitions.
*	The inner loop scans the sector allocation table to find
*	the starting and ending point for each.
*
        LHLD    S.SCR
        MVI     B,1		partition no (start at 1; 0 reserved for system)
HDLOA1	SHLD    HLTEMP
        MVI     C,4		match 4 characters
        LXI     D,HDOS		to 'HDOS'
        CALL    $COMP		is it a match?
        JNZ     HDLOA5
*
*	Have a match to 'HDOS'
*
*	This code assumes OS ids are 'HDOS0', 'HDOS1', ... 'HDOS7'
*
        MOV     A,M		fetch next byte
        CPI     ' '
        JNZ     HDLOA2
        MVI     A,'0'		make 'HDOS ' be 'HDOS0'
HDLOA2	CPI     '0'
        JC      HDLOA5
        CPI     '8'
        JNC     HDLOA5
        SUI     '0'		remove ASCII bias
        STA     OSID		store it
*
*	Scan Sector Allocation Table for partition
*
*	ENTRY:	B = partition no.
*
        PUSH    B
        PUSH    D
        PUSH    H
        PUSH    PSW
        LHLD    S.SCR		point to 2 blocks
        LXI     D,256+4		offset to SAT (skipping over system partition)
        DAD     D
        LXI     D,4		4 bytes per entry
HDLOA3	MOV     A,M		Fetch allocation ID byte
        CPI     SAT.ET		end of table?
        JZ      HDLOA6		error - end reached w/o finding it!
        ANI     00011111B	mask to lower 5 bits
        CMP     B		is it the one we want?
        JZ      HDLOA4		Yes - we have a match!
        DAD     D		no, add 4 and keep trying
        JMP     HDLOA3
*
*	"HDOSx" named partition found
*
HDLOA4	INX     H		point to 3-byte address
        XCHG			put it in DE
        LDA     OSID		Partition ID
        CALL    UNILKUP		(HL) = table entry
        MOV     M,A		store partition ID #
        INX     H		point to next
        LXI     B,3		store address triple of
        CALL    $MOVE		the partition
	
        LXI     B,3
        INX     D		skip next (allocation ID byte)
        CALL    $MOVE		store address (+1) of end triple

        MVI     M,0		last byte is 0
        POP     PSW
        POP     H
        POP     D
        POP     B
*
*	do next entry in OS ID table...
*
HDLOA5	INR     B		next ID
        MVI     A,16+1
        CMP     B
        JZ      HDLOA7		done - scanned 16 entries!
        LHLD    HLTEMP
        LXI     D,16
        DAD     D		point to next
        JMP     HDLOA1

HDLOA6	POP     PSW
        POP     H
        POP     D
        POP     B
        MVI     A,EC.ILV
        STC
HDLOA7	RET

***	GDP  -  Get Disk Parameters
*
*
GDP	LDA     AIO.UNI
        CALL    UNILKUP			Look up entry in parameters table
        CMP     M			check Unit # for successful lookup
        JZ      GDPA			OK got it!, done...
        STC				not there, error
        MVI     A,EC.ILV		Illegal value
GDPA	RET


RAWOFF	MVI     A,0
        JMP     RAWX
*
*	It looks like if DC.RAW==1 then
*	all I/O opreations are absolute?
*
RAWON   MVI     A,1
RAWX	PUSH    PSW
        MVI     A,1
        STA     DC.RAW
        POP     PSW
        RET

***	ADTRAN - translate sector number into absolute
*		disk address and insert that into command
*		block.
*
*	HL = sector number

*	NOTE: debug mode: if DC.RAW is 1 then skip directly to
*	storing HL values in the command block...
*
ADTRAN	LDA     DC.RAW
        ANA     A
        JNZ     ADTR1

        PUSH    B
        PUSH    H			save sector no.
        LDA     AIO.UNI
        CALL    UNILKUP			Lookup entry in param. table
        CMP     M			check for success
        JNZ     ADTR4			nope - unit not avail!

        INX     H			skip over unit ID
        MOV     C,M
        INX     H
        MOV     B,M			BC = sector address of start
        MOV     A,B
        ORA     C
        JZ      ADTR2			can't be at 0 (reserved for MBR)
        INX     H			skip Hi byte (assumed 0)
        INX     H			skip ID of next entry
        SHLD    HLTEMP			(HL) = end triplet
        POP     H			restore sector no.
	
        DAD     B			HL = sector address of desired block
        MOV     A,H
        STA     CDRBLK+2
        MOV     A,L
        STA     CDRBLK+3		save block address in cmd block
        POP     B			BC = rounded byte count
	
        MOV     A,C
        ANA     A
        JNZ     ADTR3			make sure it is rounded (C==0)
        MOV     C,B
        MVI     B,0			BC now = sector count
        DAD     B			add to start address
        PUSH    H			save a copy of end address
        LHLD    HLTEMP			(HL) = end triplet
        MOV     C,M
        INX     H
        MOV     B,M			BC = end triplet
        INX     H			skip Hi
        INX     H			skip ID
        MOV     A,M
        SHLD    FLAGS
        STA     FLAGB
        POP     H			HL = end address
        PUSH    H
        PUSH    B
        POP     H
        POP     B
        XRA     A
*
*	BC = end address
*	HL = end of partition
*
*	make sure we're not trying to read beyond the end of 
*	the partition!
*
	DW	MI.SBCB		HL <- HL - BC - Carry (z80)
        JC      ADTR3		too big a request!
*
*	otherwise we're done!
*
        RET


ADTR2   POP     H
        POP     B
        MVI     A,EC.UNA	Unit Not Available
        JMP     ADTR5

ADTR3   MVI     A,EC.ILV
        JMP     ADTR5

ADTR4   POP     H
        POP     B
        MVI     A,EC.UNA	Unit Not Available
ADTR5   STC
*
*	come here in 'raw' mode - just directly store
*	the HL values in the command block address!
*
ADTR1   MOV     A,H
        STA     CDRBLK+2
        MOV     A,L
        STA     CDRBLK+3
        XRA     A
        STA     FLAGB
        RET

***	OUTCOM - Issue a command to the controller
*
*	First get controller's attention, then output the
*	6-byte command block structure
*
OUTCOM	PUSH    B
        PUSH    H
*
*	Get controller's attention
*
GETCON	IN      IOPORT+RI.BST		Check status
        ANI     BS.BSY			wait for not BUSY
        JNZ     GETCON
*
*	Assert SEL and DB0 on the host bus, then wait
*	for the controller to assert BUSY
*	
        MVI     A,BC.SEL		Assert SEL and DB0
        OUT     IOPORT+RI.CON
CBUSY   IN      IOPORT+RI.BST		Check status
        ANI     BS.BSY			wait for BUSY
        JZ      CBUSY
*
*	Assert Command/Data to indicate command mode transfer
*
        MVI     A,BC.EDT
        OUT     IOPORT+RI.CON
*
*	Have controller's attention, now output command
*
        LXI     H,CDRBLK		Load pointer to command queue
COMREQ	IN      IOPORT+RI.BST		sample bus status
        ANI     BS.REQ+BS.OUT+BS.COM	Wait for conditions
        JP      COMREQ
        CPI     BS.REQ+BS.OUT+BS.COM
        JNZ     OUTCOM1			Controller sending data - leave now
        MOV     A,M			fetch byte from command queue
        OUT     IOPORT+RI.DAT		output the byte
        INX     H			point to next
        JMP     COMREQ			and continue
*
*	Exit normally
*
OUTCOM1	XRA     A
        POP     H
        POP     B
        RET

***	CMDRES - Complete the response to a command
*
*
CMDRES	PUSH    B
        PUSH    H
*
*	Get completion status and check for errors
*
        IN      IOPORT+RI.DAT		input completion status byte
        MOV     C,A			save for later...
        STA     C.STAT			and in memory
CMDRE1	IN      IOPORT+RI.BST		looking for last REQ
        ANI     BS.REQ
        JZ      CMDRE1			loop 'til found
	
        MOV     B,A			save status byte for now
        IN      IOPORT+RI.DAT		Input last byte
        STA     C.LAST			save in memory
        ANA     A			see if it's zero
        JNZ     CMDERR			if not, error
        MOV     A,B			get back status byte
        ANI     BS.PE			parity error?
        JNZ     CMDERR			if so, error
        MOV     A,C			now check completion status
        ANI     ST.PER			parity error?
        JNZ     CMDERR			if not 0, error
        MOV     A,C
        ANI     ST.ERR			other error?
        JZ      CMDRE7			no, normal exit
*
*	Error - do a more extensive diagnostic via
*	Request Sense command
*
        CALL    ZEROCDR
        MVI     A,D.RSE			Request sense
        STA     CDRBLK
        CALL    OUTCOM			Issue the command
*
*	read 4-byte response
*
        LXI     D,DSENSE		4 byte response
CMDRE2	IN      IOPORT+RI.BST
        MOV     C,A
        ANI     BS.REQ			Wait for REQ from controller
        JZ      CMDRE2
	
        MOV     A,C			get back status byte
        ANI     BS.COM			Command or data?
        JNZ     CMDRE3			COM off - exit loop...
	
        IN      IOPORT+RI.DAT		Read a byte
        STAX    D			store it
        INX     D			point to next
        JMP     CMDRE2			and loop...
*
*	Interpret error
*
CMDRE3	IN      IOPORT+RI.DAT		input completion status
        MOV     C,A			save for later...
CMDRE4	IN      IOPORT+RI.BST		looking for last REQ
        ANI     BS.REQ
        JZ      CMDRE4			loop 'til found...
	
        MOV     B,A			save status byte for now
        IN      IOPORT+RI.DAT		input last byte
        ORA     A			check for 0
        JNZ     CMDERR			not 0, error
*
*	Check status byte
*
        MOV     A,B
        ANI     BS.PE			parity error?
        JNZ     CMDERR			no, other error
*
*	check completion status
*	
        MOV     A,C
        ANI     ST.ERR+ST.PER		
        JNZ     CMDERR			no, other error
*
*	examine the 4-byte packet
*
        LDA     DSENSE
        ANI     00111111B		mask to error type & code
        CPI     T0.WFT			write fault?
        JZ      CMDRE5
        CPI     00010000B+T1.WP		write protected?
        JNZ     CMDERR
CMDRE5	MVI     A,EC.WP			Write protected
        STC
        JMP     CMDRE6			reset
*
*	Error - set 'C' and reset the controller
*
CMDERR	XRA     A
        STC
CMDRE6	PUSH    PSW
        MVI     A,BC.RST		reset bit on
        OUT     IOPORT+RI.CON
        XRA     A			reset bit off
        OUT     IOPORT+RI.CON
        POP     PSW
*
*	Restore and exit
*
CMDRE7	POP     H
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
*
*	UNILKUP - Look up Unit in table
*
*	ENTRY:	A = UNIT
*
*	EXIT:	HL points to entry in table based on A (AIO.UNI)

UNILKUP	PUSH    PSW
        PUSH    B
        RLC
        RLC
        RLC			A=A*8
        MVI     B,0
        MOV     C,A		BC = A*8
        LXI     H,UNITBL
        DAD     B
        POP     B
        POP     PSW
        RET

*       ENTRY:  HL      = Sector Number
*               DE      = Transfer Address
*               BC      = Byte Count (rounded up)
*
RTN76   PUSH    B
        PUSH    H
        PUSH    PSW
        PUSH    D
        LDA     DC.RAW
        ANA     A
        JNZ     RTN77		if in Raw mode just leave

        XRA     A		clear 'C'
        LXI     D,1
        XCHG			HL = 1; DE = sector no.
	DW	MI.SBCD		HL <- HL - DE - Carry
        JC      RTN77		jump if 2 or more?
        XCHG
        MOV     A,B
        CALL    $DADA		HL = HL + 0,A
        XRA     A
        LXI     B,2
	DW	MI.SBCB		HL <- HL - BC - Carry (z80)
        JC      RTN77
        MOV     D,E
        MVI     E,0
        POP     H
        PUSH    H
        DAD     D
        LXI     D,256-3
        DAD     D
        XCHG

        LDA     AIO.UNI
        CALL    UNILKUP
        CMP     M		Compare unit
        JNZ     RTN77
        STAX    D		store unit
        INX     D
        INX     H
        MOV     A,M		store lo address
        STAX    D
        INX     H
        INX     D
        MOV     A,M		store mid address
        STAX    D
RTN77   POP     D
        POP     PSW
        POP     H
        POP     B
        RET

***	Working storage
*
*	UNITBL: Table to hold values for up to 8 devices
*
*	Format for each entry:
*
*		Byte 	0:	Device ID no
*		Bytes 1-3: 	triple for start of partition
*		Bytes 4-6: 	triple for end partition + 1
*		Byte 	7:	Flags (e.g. write protect)
*
UNITBL	DB      '0',0,0,0,0,0,0,0
	DB      '1',0,0,0,0,0,0,0
	DB      '2',0,0,0,0,0,0,0
	DB      '3',0,0,0,0,0,0,0
	DB      '4',0,0,0,0,0,0,0
	DB      '5',0,0,0,0,0,0,0
	DB      '6',0,0,0,0,0,0,0
	DB      '7',0,0,0,0,0,0,0

FLAGB   DB	0		flags byte
WPROT   DB	0		=1 if write protected
OSID	DB	0
HDOS	DB	'HDOS'
DC.RAW	DB	0		Driver function called
SBAADR	DB	0,0,0		triple for super block
HLTEMP	DW	0
FLAGS   DW	0		address of flags byte
BCOUNT	DW	0		byte count
C.STAT	DB	0		Command completion status
C.LAST	DB	0		Last data byte
DSENSE	DB	0,0,0,0		Result from Request Sense
*
*	Command Descriptor Block (Class 0 commands)
*
CDRBLK	DB	0		000 | opcode
	DB	0		LUN | log addr2
	DB	0		log addr 1
	DB	0		log addr 0
	DB	0		num blocks
	DB	0		control

	LON	G
	
        END
