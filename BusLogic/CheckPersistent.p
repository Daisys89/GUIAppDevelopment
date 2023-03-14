DEFINE VARIABLE hproc           AS HANDLE     NO-UNDO.
DEFINE VARIABLE cFileNames      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount          AS INTEGER    NO-UNDO.

hProc = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(hProc):
  IF hProc:FILE-NAME BEGINS "DataUtil":U OR 
     hProc:FILE-NAME BEGINS "PersistentProc":U  OR
     hProc:FILE-NAME BEGINS "wCustomerSelect":U OR 
     hProc:FILE-NAME BEGINS "wOrderSelect":U THEN
       cFileNames = cFileNames + hProc:FILE-NAME + "  ".
  hProc = hProc:NEXT-SIBLING.
END.

IF cFileNames NE "" THEN
    MESSAGE "Running files: ":U SKIP(1) cFileNames 
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
ELSE
    MESSAGE "No persistent files running.":U.
