/* dsrtc - Reads DS1302 real time clock and sets the
** internal OS time & date to agree, then displays the
** time and date. Supports HDOS 2, HDOS 3 and CP/M 2.2.0x
** There is no need for a CP/M 3 version as it has its own
** DATE command to perform this function.
**
** Syntax is matched to the CP/M 3 DATE command:
**
**  DSRTC {CONTINUOUS}
**  DSRTC {time-specification}
**  DSRTC SET
**  DSRTC
**
**  If just DSRTC is typed the program reads the real
**  time clock, sets the internal OS time to correspond
**  and displays the resulting time/date.
**
**  time specification is of the form:
**    MM/DD/YY HH:MM:SS
**
**  After the time specification is supplied the system
**  responds with:
**    "Press any key to set time"
**  which allows for very precise time setting.
**
**  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
**
**  Compiled with Software Toolworks C/80 V. 3.1
**
**  NOTE: Add -qHDOS=1 when compiling for HDOS:
**
**    cc -qHDOS=1 dsrtc
**
**  Also, HDOS files must be saved in UNIX [nl-terminated] form
** but CP/M in MSDOS [cr-lf terminated) form.
**
**  DSRTC calls four externally-specified functions which are 
**  assembly routines defined in rtc1302.mac: dsin(), dsout(),
**  dsend(), and dscmd().
**
**  Recommended build sequence:
**
**    m80 =rtc1302
**    cc dsrtc (add -qHDOS=1 if building for HDOS)
**    m80 =dsrtc
**    l80 dsrtc,rtc1302,printf,scanf,clibrary,dsrtc/n/m
**
**	Glenn Roberts		September 2025
*/
#include "printf.h"
#include "scanf.h"

#define TRUE  1
#define FALSE 0

#define VERSION "1.0(beta)"

/* command for burst read */
#define R_BURST  0xBF
#define W_BURST  0xBE
#define NBURST  7

/* command to read seconds register */
#define R_SECS   0x81

/* disable write protect */
#define W_WPOFF 0x8E

static char cbuff[NBURST];

#ifdef HDOS
/* key date/time locations in HDOS */
#define S_DATE  0x20BF
#define S_DATC  0x20C8
#define S_TIME  0x20CA

/* sdate[9] = ASCII date representation
** cdate    = coded date (Y2K format)
** stime[3] = BCD time representation
*/
static char *sdate = S_DATE;
static int *cdate  = S_DATC;
static char *stime = S_TIME;
#else
/* key time/date locations in CP/M */

/* pointer to 2ms ISR in low RAM */
static char **ckptr = 0x009;
#endif

/* the day of week register internally goes from 1-7.
** 1=Monday, 2=Tuesday... 7=Sunday.
*/
static char *dname[] = {
  "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"
};

static char *mname[] = {
  "???", "Jan", "Feb", "Mar", "Apr",
  "May", "Jun", "Jul", "Aug", "Sep",
  "Oct", "Nov", "Dec"
};

/* our internal data structure for time/date */
struct tdate {
  int seconds;
  int minute;
  int hour;
  int day;
  int month;
  int dow;    /* 1=Mon, 2=Tue ... 7=Sun */
  int year;   /* 2 digits, assuming 21st century */
};

/********************************************************
**
** btod
**
** convert a BCD byte to its decimal equivalent
**
********************************************************/
int btod(b)
char b;
{
  return ((b & 0xF0) >> 4) * 10 + (b & 0x0F);
}

/********************************************************
**
** dtob
**
** convert a Decimal byte to its BCD equivalent
**
********************************************************/
int dtob(b)
char b;
{
  return ((b/10) << 4) | (b % 10);
}


/********************************************************
**
** storetime - store the time and date in the appropriate
** OS storage locations. 
**
** For HDOS date is stored in two formats:
** internal coded date (16 bit integer) and ASCII representation.
** Time is stored in binary-coded decimal format.
**
** For CP/M 2.2.0x time and date are stored as six sequential
** bytes: SS MM HH DD MM YY, at a known location whose address
** is in low RAM.
**
** Just to be on the safe side interrupts are disabled
** while updating OS time fields to avoid any potential conflict
** with the 2-ms clock update.
**
** If day or month are zero the whole time/date entry is considered
** invalid and is not stored. Routine returns with -1 error code.
** The most likely cause of this is a missing or non-functioning
** DS1302 chip
**
********************************************************/
int storetime(td)
struct tdate *td;
{
  char *s;
  int idate;
  
  /* validity check */
  if ((td->month == 0) || (td->day == 0)) {
    printf("\nInvalid date! System time/date not updated\n");
    return -1;
  }
  
#ifdef HDOS
/* HDOS version... */

  /* first store time */
  s = stime;
#asm
  di
#endasm
  *s++ = dtob(td->hour);
  *s++ = dtob(td->minute);
  *s   = dtob(td->seconds);
#asm
  ei
#endasm
  
  /* Now store date. First store ASCII date in format:
  ** dd-mmm-yy where mmm is "Jan", "Feb", etc..
  */
  sprintf(sdate, "%02d-%3s-%02d", td->day, mname[td->month], td->year-2000);

  /* next store internal date as 16-bit integer */
  idate = ((td->year-2000) << 9) | (td->month << 5) | td->day;
  *cdate = idate;
  
#else

  /* CP/M version... */
  s = *ckptr - 11;
#asm
  di
#endasm
  *s++ = td->seconds;
  *s++ = td->minute;
  *s++ = td->hour;
#asm
  ei
#endasm
  *s++ = td->day;
  *s++ = td->month;
  *s++ = td->year-2000;
  
#endif
  return 0;
}

/********************************************************
**
** readtime - read time from the RTC and store in tdate
** data structure;
**
********************************************************/
int readtime(td)
struct tdate *td;
{
  static int i;
  static char *s;

  /* issue burst read command */
  dscmd(R_BURST);
  
  /* read the resulting character burst */
  s = cbuff;
  for (i=0; i<NBURST; i++)
    *s++ = dsin();
  /* end communication */
  dsend();
 
  /* convert from BCD and interpret */
  s = cbuff;
  td->seconds = btod(*s++);
  td->minute  = btod(*s++);
  td->hour    = btod(*s++);
  td->day     = btod(*s++);
  td->month   = btod(*s++);
  td->dow     = btod(*s++);
  td->year    = 2000 + btod(*s++);
}

/********************************************************
**
** writetime - write time to RTC from tdate data structure 
**
********************************************************/
int writetime(td)
struct tdate *td;
{
  static int i;
  static char *s;

  /* convert to BCD */
  s = cbuff;
  *s++ = dtob(td->seconds);
  *s++ = dtob(td->minute);
  *s++ = dtob(td->hour);
  *s++ = dtob(td->day);
  *s++ = dtob(td->month);
  *s++ = dtob(td->dow);
  *s++ = dtob(td->year - 2000);

  /* disable write protect */
  dscmd(W_WPOFF);
  dsout(0);
  dsend();
  
  /* issue burst write command */
  dscmd(W_BURST);
  
  /* send the character burst */
  s = cbuff;
  for (i=0; i<NBURST; i++)
    dsout(*s++);
  
  /* enable write protect */
  dsout(0x80);
  
  /* end communication */
  dsend();
}


/********************************************************
**
** readreg - read a single register and return its value
**
********************************************************/
int readreg(cmd)
int cmd;
{
  static int result;

  /* issue command, read the resulting value,
  ** then close out communication
  */
  dscmd(cmd);
  result = dsin() & 0xFF;
  dsend();

  return result;
}


/********************************************************
**
** showtime - display time stored in tdate data structure;
**
** (terminate with \r - no new line)
**
********************************************************/
int showtime(td)
struct tdate *td;
{
  printf("%s %02d/%02d/%04d %02d:%02d:%02d\r",
    dname[td->dow], td->month, td->day, td->year,
    td->hour, td->minute, td->seconds);
}


/********************************************************
**
** is_leap
**
** return TRUE if year is a leap year 
**
********************************************************/
int is_leap(year) {
  return (((year%4==0)&&((year%100)!=0)) || ((year%400)==0));
}

/********************************************************
**
** dytd
**
** return number of days since 1 January of the specified
** year. 
**
********************************************************/
int dytd(m, d, y)
int m, d, y;
{
  static int days;
  
  days = d - 1;
  while (--m > 0)
    days += modays(m, y);
  
  return days;
}

/********************************************************
**
** depoch
**
** return number of days since 1 January 1978 (represented
** as 1 in CP/M 3 epoch date field).
**
********************************************************/
int depoch(m, d, y)
int m, d, y;
{
  static int days;

  /* start with days in the specified year */  
  days = dytd(m, d, y);
  
  /* now add in the previous years ... */
  while (--y > 1977) {
    days += is_leap(y) ? 366 : 365;
  }
  
  return days;
}

/********************************************************
**
** modays
**
** return days in given month and year 
**
********************************************************/
int modays(month, year)
int month;
int year;
{
  int days;
  
  switch(month)
  {
    /* 30 days hath september, april, june and november */
    case 9:
    case 4:
    case 6:
    case 11:
      days = 30;
      break;

    case 2:
      days = is_leap(year) ? 29 : 28;
      break;

    /* all the rest have 31! */
    default:
      days = 31;
      break;
  }
  
  return days;
}

/********************************************************
**
** prsdate - parse date from the supplied string
** and validate the date.  Fill in the fields in the
** tdate structure. Return 0 on success, -1 for error.
**
** date string expected to be in the format:
**
**  MM/DD/YY
**
********************************************************/
int prsdate(s, td)
char *s;
struct tdate *td;
{
  static int na, rc, mm, dd, yy;
  
  rc = 0;
  na = (sscanf(s, "%d/%d/%d", &mm, &dd, &yy));
  if (na != 3) {
/*    printf("Only %d values found\n", na); */
    rc = 1;
  }
  else {
    /* assume 21st century */
    yy += 2000;
    if ((mm > 0) && (mm <= 12)) {
      td->month   = mm;
      td->year    = yy;
      if ((dd > 0) && (dd <= modays(mm, yy))) {
        td->day  = dd;
        td->dow   = wkday(mm, dd, yy);
      } else
        /* day out of range */
        rc = -1;
    } else
      /* month out of range */
      rc = -1;
  }
  
  return rc;
}

/********************************************************
**
** wkday
**
** return day of the week (1=Mon, 2=Tue ... 7=Sun) given
** month, day and year. Uses the fact that 1/1/78
** was a Sunday (0).
**
** 0 or 7 are both considered valid values for Sun.
**
********************************************************/
wkday(m, d, y)
int m, d, y;
{
  int dow;
  
  /* compute number of days since epoch day and return
  ** the remainder when divided by 7. First compute zero-based
  Day Of Week
  */
  dow = depoch(m, d, y) % 7;
  
  if (dow == 0)
    dow += 7;
  
  return dow;
}

/********************************************************
**
** prstime - parse time from the supplied string and fill
** in the fields in the tdate structure. Perform bounds
** checking on time values. Return 0 on success, -1 for error.
**
** time string expected to be in the format:
**
**  HH:MM:SS
**
********************************************************/
prstime(s, td)
char *s;
struct tdate *td;
{
  static int na, rc, hh, mm, ss;
  
  rc = 0;
  na = (sscanf(s, "%d:%d:%d", &hh, &mm, &ss));
  if (na != 3) {
    /* error parsing time - need all three values */
    rc = -1;
  }
  else {
    /* rudimentary range checking */
    if ((ss >= 0) && (ss < 60)) {
      td->seconds = ss;
      if ((mm >= 0) && (mm < 60)) {
        td->minute = mm;
        if ((hh >= 0) && (hh < 24))
          td->hour = hh;
        else
          /* invalid hour */
          rc = -1;
      } else
        /* invalid minute */
        rc = -1;
    } else
      /* invalid seconds */
      rc = -1;
  }
  
  return rc;
}


/********************************************************
**
** dohelp - print help message
**
**
********************************************************/
dohelp()
{
  printf("DSRTC V.%s  Usage:\n", VERSION);
  printf("   DSRTC {CONTINUOUS}\n");
  printf("   DSRTC {MM/DD/YY HH:MM:SS}\n");
  printf("   DSRTC SET\n");
}

main(argc, argv)
int argc;
char *argv[];
{
  static int mode, sec0, sec1, i;
  static struct tdate t;
  static char tbuff[30];
  
  /* mode: 0: (default) read RTC chip, display & store time
  **       1: prompt to set time
  **       2: read & set time from command line
  **       3: display time continuously
  **       4: display help
  */
  mode = 0;
  
  /* process commmand line options (if any) */
  if (argc > 1) {
    if (*argv[1] == 'S')
      /* SET */
      mode = 1;
    else if (*argv[1] == 'C')
      /* CONTINUOUS */
      mode = 3;
    else if(argc == 3) {
      /* check for valid time/date */
      if ((prsdate(argv[1], &t)) == 0 &&
          (prstime(argv[2], &t)) == 0)
          mode = 2;
    }
    else
      /* unknown - print help and exit */
      mode = 4;
  }
  
  switch(mode) {
    case 0:
      /* print the time, update internal fields and exit */
      readtime(&t);
      showtime(&t);
      storetime(&t);
      putchar('\n');
      break;
      
    case 1:
      /* prompt user for time/date and set rtc */
      printf("Enter today's date (MM/DD/YY): ");
      scanf("%s", tbuff);
      if (prsdate(tbuff, &t) != 0) {
        printf("ERROR: Illegal date specification.\n");
        break;
      }
      
      /* have valid date, now do time */
      printf("Enter the time (HH:MM:SS):     ");
      scanf("%s", tbuff);
      if (prstime(tbuff, &t) != 0) {
        printf("ERROR: Illegal time specification.\n");
        break;
      }
      /* fall through to case 2 to set rtc ... */
      
    case 2:
      /* have valid time and date */
      printf("Press any key to set time");
      getchar();
      /* now write to RTC and internally */
      writetime(&t);
      storetime(&t);
      showtime(&t);
      putchar('\n');
      break;
      
    case 3:
      /* display time continuously until key press
      **
      ** get an initial value for seconds, then keep polling
      ** until it changes and print an updated time when it does,
      ** then continue looping until interrupted.
      */
      printf("Current time & date (Ctrl-C to exit)\n");
      sec0 = readreg(R_SECS);
      do {
        if ((sec1 = readreg(R_SECS)) != sec0 ) {
          sec0 = sec1;
          readtime(&t);
          showtime(&t);
        }
        /* in CP/M must check for ^C or console will hang */
#ifndef HDOS
        CtlCk();
#endif
        /* pause a bit so we don't hammer the clock */
        for (i=0; i<1000; i++)
          ;
      } while (TRUE); /* ctrl-c to exit */
      break;
      
    case 4:
      /* unexpected input or syntax error - print help */
      dohelp();
      break;
  }
}