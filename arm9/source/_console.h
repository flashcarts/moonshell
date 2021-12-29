
#ifndef _console_h
#define _console_h

#define ConsoleMaxLen (126)

extern void _consoleInitDefault(u16* map, u16* charBase);
extern void _consoleClear(void);
extern void _consolePrintSet(int x, int y);
extern int _consoleGetPrintSetY(void);

extern void _consolePrint(const char* s);
extern void _consolePrintf(const char* format, ...);

extern void _consoleSetLogFile(void *_pf);
extern bool _consoleGetLogFile(void);
extern void _consoleSetLogOutFlag(bool f);

extern void _consoleLogPause(void);
extern void _consoleLogResume(void);

#endif
