/*  SPD.C -- CPU speed change utility for Rev 3 and up Z80
**	CPU board, as well as NC Super89. Can be compiled for
**  HDOS2, HDOS3, CP/M 2.2.0x or CP/M 3. Not supported on MP/M.
**
**  Usage: SPD { 2 | 4 | 8 | M }
**
**  Arguments correspond to CPU speed in megahertz. If 'M' is
**  specified the board is set to the max speed as determined
**  by the installed crystal oscillator (typically 10Mhz or
**  16Mhz). Arguments are not case sensitive.
**
**  The CPU speed is controlled via port 362 octal (0xF2),
**  however a shadow copy of the port settings is maintained
**  in the OP2.CTL Control Byte in RAM. The program must parse
**  the command line, set the appropriate speed bits, output to
**  the control port and update the shadow copy in RAM. If no
**  arguments are given the current speed setting (in RAM) is
**  simply reported.
**
**  Compiled with Software Toolworks C/80 V. 3.1
**
**  NOTE: Add -qHDOS=1 when compiling for HDOS. Also, HDOS files
**  must be saved in UNIX [nl-terminated] form but CP/M
**  in MSDOS [cr-lf terminated) form.
**
**  Buildsequence:
**    cc spd
**    m80 = spd
**    l80 spd,pio,printf,clibrary,spd/n/m/e
**
**	uses outp() from PIO.ASM
**
**	Glenn Roberts		16 September 2021
**	with input from Ken Owen and Douglas Miller.
** 
**  /gfr/ - Updated for NC Super89, CP/M 3 and other fixes
**  September 2025
*/
#include "printf.h"

/* control port for speed and other functions */
#define CTLPRT	0xF2

/* mask 11101011to clear old speed bits */
#define	SMASK	0xEB

/* speed masks */
#define	MASK2		0x00
#define MASK4		0x10
#define	MASK8		0x04
#define	MASKMAX	0x14
#define MASKUNK 0x7F

/* control byte location */
char *ctl2fl;

#ifndef HDOS
/* offset from CP/M 3 BIOS warm boot for ctlflg */
#define CTLFLG  97

/* address of BIOS warm boot in low RAM */
char **bptr = 0x0001;

/********************************************************
**
**  *** Valid for use only in CP/M ***
**
** bdoshl - Call CP/M BDOS function with given values
**    for registers C and DE.  Return the value
**    from the HL register.
**
********************************************************/
int bdoshl(c,de)
  int c, de;
{
#asm
        POP     H
        POP     D
        POP     B
        PUSH    B
        PUSH    D
        PUSH    H
        CALL    5
#endasm
}
#endif


main(argc,argv)
int	argc;
char *argv[];
{
	int spd;
	char newspd, spdmask;

#ifdef  HDOS
  ctl2fl = 0x2036;
#else
  if (bdoshl(0x0C, 0) < 0x31)
    /* cpm 2.2 */
    ctl2fl = 0x000D;
  else {
    /* assume CP/M 3 */
    ctl2fl = *bptr + CTLFLG;
  }
#endif

  if (argc > 1) {
    /* set mask according to argument. MASKUNK indicates
    ** missing or invalid argument.
    */
    switch (*argv[1]) {
      case '2':
        spdmask = MASK2;
        break;
      case '4':
        spdmask = MASK4;
        break;
      case '8':
        spdmask = MASK8;
        break;
      case 'm':
      case 'M':
        spdmask = MASKMAX;
        break;
      default:
        spdmask = MASKUNK;
        break;
    }
    if (spdmask == MASKUNK)
      printf("Usage: SPD { 2 | 4 | 8 | M )\n");
    else {
      /* Ready to set the speed. First we need to
      ** update the shadow RAM value. Disable interrupts
      ** so that the 2 ms clock update doesn't clobber
      ** the value we're modifying
      */
#asm
  di
#endasm
      /* clear old speed bits and OR in the new ones */
      *ctl2fl = *ctl2fl & SMASK | spdmask;
#asm
  ei
#endasm
      /* Now output the new speed setting to the control port */
      outp(CTLPRT, *ctl2fl);
    }
  }
	
	/* Verify current CPU speed and report it, then exit */
	spd = 2 + ((*ctl2fl & MASK4) / 8) + ((*ctl2fl & MASK8) /2 * 3);
	printf("You are running at ");
  if (spd == 10)
    printf("maximum speed.\n");
  else
    printf("%d MHz.\n", spd);
}
