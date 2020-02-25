      ******************************************************************
      * Author: Andrew Wimer
      * Date: 2/20/2020
      * Purpose: Read 50 words from a file, store in array,
      *           remove duplicate words.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.  *>division for IDing program info
       PROGRAM-ID. storing-words.   *>name of program


       ENVIRONMENT DIVISION. *>computer and I/O configs go here
                             *>ED is optional
       INPUT-OUTPUT SECTION. *>linkage between program and external
                             *>resources in this section
       FILE-CONTROL.         *>determines how files are loaded and
                             *>handled
           SELECT file-in ASSIGN to "input.dat"
           *>identifies file and assigns location to find file

               FILE STATUS IS input-file-status
               *>assigns name to item we use to determine
               *>status of file (errors or end of file)
               ORGANIZATION IS LINE SEQUENTIAL.
               *>file will be read line by line


       DATA DIVISION.*>begin defining data we will use
       FILE SECTION. *>begin section for file I/O
       FD file-in.   *>File Description, describes file layout
       01  IN-FILE. *> File input header
           05 FILE-ELEMENT PIC X(10). *> 05 Describes record entry.
       *>PIC X(10): each entry has 10 characters

       WORKING-STORAGE SECTION. *>Define variables, tables, records etc
       01 input-file-status PIC 99. *>FIle statuses can be 2 digits
           88 file-ok    VALUE 0. *> 88 means condition name
           *> Value always 0 when file is ok to load
           88 file-end   VALUE 10.
           *> Value always 10 when file is at end

       01  line-number PIC 9(6) VALUE 0. *>Counter variable for
                                    *> incrementing by line number

       *> declare our table
       01  WS-TABLE.
           05 WS-TABLE-ELEMENT PIC X(10) occurs 50 TIMES INDEXED BY i.
           *>50 lines in table

       *> declare our second table for unique tree entries
       01  WS-UNIQUE.
           05 WS-NAME-UNIQUE PIC X(10) occurs 1 to 50 times
           depending on UNIQUE-COUNT.
       *>table length will vary depending on how many unique words found

       *>Variable list
       01  WS-LIST.
           05 UNIQUE-COUNT PIC 99 VALUE 1. *>number of unique words

       *> Subscripts for looping through tables
       01  SUBSCRIPTS.
           05 j PIC 99 VALUE 1.
           05 k PIC 99 VALUE 1.

       *> Here is where the program is executed
       *> Main procedure, subprograms, functions go here
       PROCEDURE DIVISION.



       *>Check the input file
       OPEN INPUT file-in *> open our input file
       IF NOT file-ok     *>see if file c
           DISPLAY "Could not open file." *>Display error message
           GOBACK              *>basically "end program"
       END-IF

       *> Read first file entry and move into first table element
       READ file-in
       MOVE FILE-ELEMENT TO WS-TABLE-ELEMENT(1)
       *> Perform previous step but repeat for each line in file
       *> until the end of the file (loop, sorta like do-until)
       PERFORM VARYING line-number FROM 1 BY 1 UNTIL file-end
           READ file-in
           MOVE FILE-ELEMENT TO WS-TABLE-ELEMENT(line-number)
       END-PERFORM
       *>Denote end of loop

       SORT WS-TABLE-ELEMENT DESCENDING *>Sort table, alphabetic order

       CLOSE file-in. *> Close our file input

       *> Display all 50 words in table, now sorted
       PERFORM VARYING i FROM 1 BY 1 UNTIL i=50
           DISPLAY WS-TABLE-ELEMENT(i)
       END-PERFORM

       *>compare each element of table to previous until end of table
       *> if the elements are the same, increase subscript j and iterate
       *> to next element.
       *>if the elements are not the same, add the element to our new
       *> table of unique words, then increase our counter of
       *> how many unique words are stored.
       PERFORM VARYING i FROM 2 BY 1 UNTIL i=50
           IF WS-TABLE-ELEMENT(i) NOT EQUAL WS-TABLE-ELEMENT(j)
              MOVE WS-TABLE-ELEMENT(i) TO WS-NAME-UNIQUE(UNIQUE-COUNT)
              ADD 1 TO UNIQUE-COUNT
           END-IF

           ADD 1 TO j
       END-PERFORM

       *> line break
       DISPLAY " "

       *>Loops and displays each element of unique words table
       PERFORM VARYING k FROM 1 BY 1 UNTIL k=UNIQUE-COUNT
       DISPLAY WS-NAME-UNIQUE(k)
       END-PERFORM.
       *> End of procedures.
       STOP RUN.
       *> ENd of program
       END PROGRAM storing-words.
