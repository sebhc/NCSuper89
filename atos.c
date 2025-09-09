/* atos 
**
** Converts HDOS ".ABS" (absolute) image to a .SYS file.
** Format is defined in http://www.cpm.z80.de/manuals/cpm3-sys.pdf
** p. 115.
**
** After opening the input and output files the program reads the
** contents of the ABS file into RAM, storing each 128-byte record
** in an indexed array. Next the size in pages of the image is computed
** and size and location information is stored in the header record,
** which is written to the output file, along with a 128-byte print
** record. Finally the records are written to the output file but in
** reverse order.
**
** G. Roberts
** February 2023
**
*/

#define RECSIZE	128
#define PAGESIZE	256
#define BUFFSIZE	256

#define MAXR	128			/* maximum number of pages */
#define TRUE	1
#define FALSE	0
#define NUL		0
#define LF		012
#define CR		015
#define EOF		-1

#include "printf.h"

/* Header (first) record of CP/M 3 .SYS file */
struct hrec {
  char restop;			/* top of resident code page + 1 */
  char ressize;			/* length (pages) of resident code */
  char bnktop;			/* top of banked code page + 1 */
  char bnksize;			/* length (pages) of banked code */
  int entry;				/* cold boot entry point */
  char reserved[10];
  char copyright[36];	/* copyright notice */
  char xxx1;				/* reserved */
  char serno[6];		/* serial no */
  char xxx2[69];		/* reserved */
	char printrec[RECSIZE]; /* print record */
} header;

/* .ABS header format  */
struct abshdr {
	char	id;
	char	ft;
	int 	load;
	int 	length;
	int 	start;
	char 	data[BUFFSIZE-8];
};

union b_tag {
	struct abshdr hdr;
	char buf[BUFFSIZE];
} u;


/* A fixed-size array of pointers to dynamically-
** allocated PAGESIZE-byte records. This is hard wired
** to a maximum size which limits the executable
** to MAXR*PAGESIZE bytes in size.
*/
char *record[MAXR];

/* input and output channels */
char infile[20], outfile[20];
int fhin, fhout;
int nextin, nextout;

/* openfiles - using the arguments passed on the command line,
** open the input file and output file. if only a base name is
** passed (no '.') then assume the input file is a .ABS and the
** output is a .SYS.  If no output file is specified use the
** same base as the input file.  If output file is specified with
** a different 3-letter extension, force it to be .SYS.
**
** upon success the globals fhin and fhout are set to the input and
** output file handles, respectively, and infile[] and outfile[]
** arrays contain the respective file names.
**
** return 0 on success, -1 on error
**
*/
int openfiles(argc, argv)
int argc;
char *argv[];
{
	int rc, dot;
	
	rc = 0;
	strcpy(infile, argv[1]);
	if ((dot = index(infile, ".")) == -1)
		/* no file type specified, make it ABS */
		strcat(infile, ".ABS");
		
	if (argc < 3 ) {
		/* no outfile specified, use infile base */
		strcpy(outfile, argv[1]);
		if ((dot = index(outfile, ".")) != -1) {
			/* user specified a file type on input. Truncate
			** the string at that point.
			*/
			outfile[dot] = '\0';
		}
		/* now make sure the ouput type is SYS */
		strcat(outfile, ".SYS");
	}
	else {
		/* output file specified. append .SYS if
		** no file type specified
		*/
		strcat(outfile, argv[2]);
		if ((dot = index(outfile, ".")) == -1)
			strcat(outfile, ".SYS");
	}
	if ((fhin = fopen(infile, "rb"))==0) {
		printf("Unable to open input file %s\n", infile);
		rc = -1;
	}
	else if ((fhout = fopen(outfile, "wb"))==0) {
		printf("Unable to open output file %s\n", outfile);
		rc = -1;
	}
	
	return rc;
}


main(argc, argv)
int argc;
char *argv[];
{
	int i, nbytes, np, error, done;
	char ch;
	char *newrec;
	
	/* values to save load address, record length and start point
	** from ABS header
	*/
	int lda, len, ent;

  if (argc < 2)
		printf("usage: %s <file>\n", argv[0]);
	else if ((openfiles(argc, argv)) == -1)
		printf("Error opening file(s)\n");
	else {
		/* input and output files opened successfully... */
		printf("Converting %s to %s ...\n", infile, outfile);

		/* read header */
		for (i=0; i<8; i++)
			u.buf[i] = getc(fhin) & 0xFF;

		lda = u.hdr.load;
		len = u.hdr.length;
		ent = u.hdr.start;

		printf("Load address : %04x\n", lda);
		printf("Record length: %04x\n", len);
		printf("Entry point  : %04x\n", ent);
			
		/* loop to read and write data bytes. bytes
		** are stored in PAGESIZE records, however
		** they must be later written out in RECSIZE
		** records and in *reverse* order.
		*/
		done = FALSE;
		error = FALSE;
		nbytes = 0;
		
		for (np = 0; np<MAXR && !done; np++) {
			/* allocate a page */
			if ((newrec = alloc(PAGESIZE)) == 0) {
				printf("Out of memory!\n");
				error = TRUE;
				break;
			}
			else {
				/* now read PAGESIZE bytes into this buffer.  Pad
				** trailing bytes with NUL if necessary
				*/
				for (i=0; i<PAGESIZE && !done; i++) {
					if ((ch=getc(fhin)) == -1) {
						done = TRUE;
						break;
					}
					else {
						/* store the records in reverse order within
						** the page. the first record in the highest
						** half of the page and the second one in the
						** lowest. once 'len' bytes have been processed
						** set the rest to NUL.
						*/
						newrec[(i+RECSIZE)%PAGESIZE] = ch;
						if (++nbytes == len)
							done = TRUE;
					}
				}
				/* if exited before hitting PAGESIZE (e.g. EOF)
				** then fill the balance with NUL
				*/
				while (i<PAGESIZE) {
					newrec[(i+RECSIZE)%PAGESIZE] = NUL;
					i++;
				}
				/* add this to the array of records */
				record[np] = newrec;
			}
		}
		printf("%d pages read\n", np);

		if (error || np==MAXR)
			printf("Aborting\n");
		else {
			/* we have now successfully read in the entire
			** ABS file, saved the header information, and 
			** created a series of records to be written to
			** the SYS file. Next we create the SYS file by
			** writing the header information and then writing
			** the records, in *reverse* order...
			*/
			printf("Writing %s\n", outfile);
			
			/* header record */
			header.restop = (lda >> 8 & 0xFF) + np;
			header.ressize = np;
			header.bnktop = 0x40;
			header.bnksize = 0;
			header.entry = ent;
			header.copyright[0] = 'U';
			header.printrec[0] = '$';
			for (i=1; i<RECSIZE; i++)
				header.printrec[i] = NUL;
			
			write(fhout, &header, PAGESIZE);
		
			/* output the pages in reverse */
			for (i=np-1; i>=0; i--)
				write(fhout, record[i], PAGESIZE);

			fclose(fhout);
		}
	}
}

