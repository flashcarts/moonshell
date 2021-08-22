/*
 * TTAError.h
 *
 * Description: Errors processor internal interface
 *
 * Copyright (c) 2004 Alexander Djourik. All rights reserved.
 * Copyright (c) 2004 Pavel Zhilin. All rights reserved.
 *
 */

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * aint with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Please see the file COPYING in this directory for full copyright
 * information.
 */

#pragma once

	enum TTAError 
	{
		TTA_NO_ERROR = 0, 
		FORMAT_ERROR,
		FILE_ERROR,
		FIND_ERROR,
		CREATE_ERROR,
		OPEN_ERROR,
		WRITE_ERROR,
		READ_ERROR,
		MEMORY_ERROR,
		TTA_CANCELED
	};

static TTAError errNo;

static const char *TTAErrorsStr[] = {
	"no errors found",
	"not compatible file format",
	"file is corrupted",
	"file(s) not found",
	"problem creating directory",
	"can't open file",
	"can't write to output file",
	"can't read from input file",
	"insufficient memory available",
	"operation canceled"
};

static const char *
GetErrStr (TTAError err) 
{
  return TTAErrorsStr[err];
}

#define throw 
static void TTAException(TTAError err)
{
  errNo=err;
  _consolePrintf("%s\n",GetErrStr(errNo));
  ShowLogHalt();
}


