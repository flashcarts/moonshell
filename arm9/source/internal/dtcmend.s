;;; Copyright ARM Ltd 2002. All rights reserved.

        AREA   DTCMEND, DATA, NOINIT

        EXPORT dtcmend

; Create dummy variable used to locate bottom of heap

dtcmend    DCD 0

        END

