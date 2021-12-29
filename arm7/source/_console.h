
#ifndef _console_h
#define _console_h

//#define EnableConsoleGBALog

#ifdef EnableConsoleGBALog

extern void _consolePrint(const char* s);
extern void _consolePrintf(const char* format, ...);
extern void _console_ReenabledGBABUS(void);

#else

static inline void _consolePrint(const char* s)
{
}

static inline void _consolePrintf(const char* format, ...)
{
}

static inline void _console_ReenabledGBABUS(void)
{
}

#endif

#endif
