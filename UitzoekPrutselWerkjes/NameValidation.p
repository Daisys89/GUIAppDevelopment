DEFINE VARIABLE cInput             AS CHARACTER INITIAL "kees smit de boer" NO-UNDO.
DEFINE VARIABLE cInput1            AS CHARACTER INITIAL "KEES SMIT DE BOER" NO-UNDO.
DEFINE VARIABLE iCount             AS INTEGER                               NO-UNDO.
DEFINE VARIABLE iCountLC           AS INTEGER                               NO-UNDO.
DEFINE VARIABLE cOutputENTRY       AS CHARACTER                             NO-UNDO.
DEFINE VARIABLE cOutputENTRYLC     AS CHARACTER                             NO-UNDO.
DEFINE VARIABLE cOutputSUBSTRING   AS CHARACTER                             NO-UNDO.
DEFINE VARIABLE cOutputSUBSTRINGLC AS CHARACTER                             NO-UNDO.
DEFINE VARIABLE cOutputCAPS        AS CHARACTER                             NO-UNDO.
DEFINE VARIABLE cOutputLC          AS CHARACTER                             NO-UNDO.
DEFINE VARIABLE cOutputLENGTH      AS INTEGER                               NO-UNDO.
        
/*DO iCount = 1 TO NUM-ENTRIES(cInput, " "): // loops through user input, gives number of entries .
    ASSIGN 
        cOutput = cOutput + CAPS(SUBSTRING(ENTRY(iCount,cInput," "),1,1)) +
                            LC(SUBSTRING(ENTRY(iCount,cInput," "),2, LENGTH(ENTRY(iCount,cInput," ")))) + " ".
END.        
RETURN cOutput.   */                      

/*
// TURN ALL OTHER LETTERS IN LOWER CASE //
DO 
iCountLC = 1 TO NUM-ENTRIES(cInput1, " "):
MESSAGE "iCountLC is:" iCountLC VIEW-AS ALERT-BOX.

cOutputENTRYLC = ENTRY(iCountLC, cInput1, " ").
MESSAGE "cOutputENTRYLC is:" cOutputENTRYLC VIEW-AS ALERT-BOX. 

cOutputLENGTH = LENGTH(ENTRY(iCountLC,cInput1, " ")).
MESSAGE "cOutputLENGTH is:" cOutputLENGTH VIEW-AS ALERT-BOX.

cOutputSUBSTRINGLC = SUBSTRING(ENTRY(iCountLC, cInput1, " "),2).
MESSAGE "cOutputSUBSTRINGLC is:" cOutputSUBSTRINGLC VIEW-AS ALERT-BOX.

cOutputLC = LC(SUBSTRING(ENTRY(iCountLC,cInput1, " "),2, LENGTH(ENTRY(iCountLC,cInput1, " ")))).
MESSAGE "cOutputLC is:" cOutputLC VIEW-AS ALERT-BOX.
*/


// TURN FIRST LETTER IN CAPITAL //
DO iCount = 1 TO NUM-ENTRIES(cInput, " "):
MESSAGE "iCount is:" iCount VIEW-AS ALERT-BOX.

cOutputENTRY = ENTRY(iCount,cInput," ").
MESSAGE "cOutputENTRY is:" cOutputENTRY VIEW-AS ALERT-BOX.

cOutputSUBSTRING = SUBSTRING(ENTRY(iCount,cInput," "),1,1).
MESSAGE "cOutputSUBSTRING is:" cOutputSUBSTRING VIEW-AS ALERT-BOX.

cOutputCAPS = CAPS(SUBSTRING(ENTRY(iCount,cInput," "),1,1)).
MESSAGE "cOutputCAPS is:" cOutputCAPS VIEW-AS ALERT-BOX.

