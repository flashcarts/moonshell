
#include "_console.h"
#include "memtool.h"

#if 0

#ifndef plugin_def_h
#define plugin_def_h

#undef clearerr
#define clearerr (blockfunc)
#define fdopen (blockfunc)
#undef feof
#define feof (blockfunc)
#undef ferror
#define fopen (blockfunc)
#define fclose (blockfunc)
#define ferror (blockfunc)
#define fflush (blockfunc)
#define fgetc (blockfunc)
#define fgetpos (blockfunc)
#define fgets (blockfunc)
#define fileno (blockfunc)
#define fprintf (blockfunc)
#define fpurge (blockfunc)
#undef fputc
#define fputc (blockfunc)
#define fputs (blockfunc)
#define freopen (blockfunc)
#define fscanf (blockfunc)
#define fsetpos (blockfunc)
#define fwrite (blockfunc)
#undef getc
#define getc (blockfunc)
#undef getchar
#define getchar (blockfunc)
#define gets (blockfunc)
#define getw (blockfunc)
#define mktemp (blockfunc)
#define perror (blockfunc)
#define printf (blockfunc)
#undef putc
#define putc (blockfunc)
#undef putchar
#define putchar (blockfunc)
#define puts (blockfunc)
#define putw (blockfunc)
#define remove (blockfunc)
#define rewind (blockfunc)
#define scanf (blockfunc)
#define setbuf (blockfunc)
#define setbuffer (blockfunc)
#define setlinebuf (blockfunc)
#define setvbuf (blockfunc)
#define sscanf (blockfunc)
#define strerror (blockfunc)
#define sys_errlist (blockfunc)
#define sys_nerr (blockfunc)
#define tempnam (blockfunc)
#define tmpfile (blockfunc)
#define tmpnam (blockfunc)
#define ungetc (blockfunc)
#define vfprintf (blockfunc)
#define vfscanf (blockfunc)
#define vprintf (blockfunc)
#define vscanf (blockfunc)
#define vsprintf (blockfunc)
#define vsscanf (blockfunc)

#define _consolePrint (pStdLib->_consolePrint)
#define _consolePrintf (pStdLib->_consolePrintf)
#define _consolePrintSet (pStdLib->_consolePrintSet)
#define ShowLogHalt (pStdLib->ShowLogHalt)

#define MemCopy8CPU (pStdLib->MemCopy8CPU)
#define MemCopy16CPU (pStdLib->MemCopy16CPU)
#define MemCopy32CPU (pStdLib->MemCopy32CPU)
#define MemSet16CPU (pStdLib->MemSet16CPU)
#define MemSet32CPU (pStdLib->MemSet32CPU)
#define safemalloc (pStdLib->safemalloc)
#define safefree (pStdLib->safefree)

#define calloc (pStdLib->calloc)
#define malloc (pStdLib->malloc)
#define free (pStdLib->free)
#define realloc (pStdLib->realloc)

#define rand (pStdLib->rand)

#define fread (pStdLib->fread)
#define fseek (pStdLib->fseek)
#define ftell (pStdLib->ftell)

#define sprintf (pStdLib->sprintf)
#define snprintf (pStdLib->snprintf)

#define memchr (pStdLib->memchr)
#define memcmp (pStdLib->memcmp)
#define memcpy (pStdLib->memcpy)
#define memmove (pStdLib->memmove)
#define memset (pStdLib->memset)

#define abs (pStdLib->abs)
#define labs (pStdLib->labs)
#define llabs (pStdLib->llabs)
#define fabs (pStdLib->fabs)
#define fabsf (pStdLib->fabsf)

#define atof (pStdLib->atof)
#define atoi (pStdLib->atoi)
#define atol (pStdLib->atol)
#define atoll (pStdLib->atoll)

#define strcat (pStdLib->strcat)
#define strchr (pStdLib->strchr)
#define strcmp (pStdLib->strcmp)
#define strcoll (pStdLib->strcoll)
#define strcpy (pStdLib->strcpy)
#define strcspn (pStdLib->strcspn)
#define strdup (pStdLib->strdup)
#define strlen (pStdLib->strlen)
#define strncat (pStdLib->strncat)
#define strncmp (pStdLib->strncmp)
#define strncpy (pStdLib->strncpy)
#define strpbrk (pStdLib->strpbrk)
#define strrchr (pStdLib->strrchr)
#define strsep (pStdLib->strsep)
#define strspn (pStdLib->strspn)
#define strstr (pStdLib->strstr)
#define strtok (pStdLib->strtok)
#define strxfrm (pStdLib->strxfrm)

#endif

#endif

