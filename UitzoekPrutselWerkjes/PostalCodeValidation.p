/* 
DEFINE VARIABLE cPostalCode         AS CHARACTER INITIAL "10551-EN    " NO-UNDO.
DEFINE VARIABLE iPCLength           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPCNumbers          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPCLetters          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iPCNumCorrect       AS LOGICAL   NO-UNDO.

DEFINE VARIABLE CorrectedPostalCode AS CHARACTER NO-UNDO.

IF cPostalCode = "" THEN RETURN "".

cPostalCode = REPLACE(STRING(cPostalCode)," ","").
cPostalCode = REPLACE(STRING(cPostalCode),"-","").
cPostalCode = TRIM(cPostalCode).
MESSAGE "cPostalCode is:" cPostalCode VIEW-AS ALERT-BOX.

iPCLength = LENGTH(cPostalCode).
MESSAGE "iPCLength is:" iPCLength VIEW-AS ALERT-BOX.

iPCNumbers = INTEGER(SUBSTRING(cPostalCode,1,4)).
MESSAGE "iPCNumbers is:" iPCNumbers VIEW-AS ALERT-BOX.

cPCLetters = SUBSTRING(cPostalCode,5,6).
MESSAGE "cPCLetters is:" cPCLetters VIEW-AS ALERT-BOX.

IF LENGTH(cPostalCode) = 6 AND LENGTH(iPCNumbers) = 4 AND LENGTH(cPCLetters) = 2 THEN 
DO: 
    iPCNumCorrect = YES.
    MESSAGE "iPCNumCorrect is:" iPCNumCorrect VIEW-AS ALERT-BOX.
    // return true.
END.
   
IF LENGTH(cPostalCode) <> 6 OR LENGTH(iPCNumbers) <> 4 OR LENGTH(cPCLetters) <> 2 THEN 
DO: 
    iPCNumCorrect = NO.
    MESSAGE "iPCNumCorrect is:" iPCNumCorrect VIEW-AS ALERT-BOX.
    // return false.
END.   
*/

// Correct email FORMAT //
DEFINE VARIABLE cPostalCode AS CHARACTER INITIAL "10551-nn" NO-UNDO.
DEFINE VARIABLE cOutput AS CHARACTER NO-UNDO.

DEFINE VARIABLE iPCNumbers AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPCLetters AS CHARACTER NO-UNDO.

IF cPostalCode = "" THEN RETURN "".

cPostalCode = REPLACE(STRING(cPostalCode)," ","").
cPostalCode = REPLACE(STRING(cPostalCode),"-","").
cPostalCode = TRIM(cPostalCode).
MESSAGE "cPostalCode is:" cPostalCode VIEW-AS ALERT-BOX.

iPCNumbers = INTEGER(SUBSTRING(cPostalCode,1,4)).
cPCLetters = SUBSTRING(cPostalCode,5,6).

DO: 
    cOutput = STRING(iPCNumbers) + CAPS(cPCLetters).
    MESSAGE "cOutput is:" cOutput VIEW-AS ALERT-BOX.
END.    

RETURN cOutput.
