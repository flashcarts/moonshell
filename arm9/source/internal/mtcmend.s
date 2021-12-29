;;; Copyright ARM Ltd 2002. All rights reserved.

        AREA   MTCMEND, DATA, NOINIT

        EXPORT mtcmend

; Create dummy variable used to locate bottom of heap

mtcmend    DCD 0

        END

