
//#define _reg_console_h // ignore

#ifndef _reg_console_h
#define _reg_console_h

extern void _consoleInit(u16* font, u16* charBase, u16 numCharacters, u8 charStart, u16* map, u8 pal, u8 bitDepth);
extern void _consoleInitDefault(u16* map, u16* charBase, u8 bitDepth);

extern void _consolePrint(const char* s);
extern void _consolePrintf(const char* format, ...);

extern void _consolePrintSet(int x, int y);

extern void _consolePrintChar(char c);

extern void _consoleClear(void);

#define consoleInit (_consoleInit)
#define consoleInitDefault (_consoleInitDefault)

#define consolePrint (_consolePrint)
#define consolePrintf (_consolePrintf)

#define consolePrintSet (_consolePrintSet)

#define consolePrintChar (_consolePrintChar)

#define consoleClear (_consoleClear)

#endif
