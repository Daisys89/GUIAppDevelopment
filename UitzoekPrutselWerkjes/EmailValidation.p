//FUNCTION ValidateEmail RETURNS LOGICAL
    //( INPUT cCorrectEmail AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cCorrectEmail   AS CHARACTER INITIAL "dph.simons@gmail.com" NO-UNDO.
DEFINE VARIABLE cInCorrectEmail AS CHARACTER INITIAL "dph.simonsgmail.com" NO-UNDO.
    
DEFINE VARIABLE iChar           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLength         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLeft           AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE cRight          AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE iAt             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDot            AS INTEGER   NO-UNDO.
        
DEFINE VARIABLE lEmailValidated AS LOGICAL   NO-UNDO.    
        
cInCorrectEmail = TRIM(cInCorrectEmail).
MESSAGE "cInCorrectEmail is:" cInCorrectEmail VIEW-AS ALERT-BOX.

iLength = LENGTH(cInCorrectEmail).
MESSAGE "iLength is:" iLength VIEW-AS ALERT-BOX. 
      
IF iLength < 5 THEN 
    lEmailValidated = NO.

iAt = INDEX(cInCorrectEmail,"@").
MESSAGE "iAt is:" iAt VIEW-AS ALERT-BOX.

cLeft = SUBSTRING(cInCorrectEmail,1,(iAt - 1)).
MESSAGE "cLeft is:" cLeft VIEW-AS ALERT-BOX.

cRight = SUBSTRING(cInCorrectEmail, (iAt + 1), (iLength - (iAt ))).
MESSAGE "cRight is:" cRight VIEW-AS ALERT-BOX.

iDot = INDEX(cRight,".").
MESSAGE "iDot is:" iDot VIEW-AS ALERT-BOX.
        
IF iAt = 0 OR iDot = 0 OR LENGTH(cLeft) = 0 OR LENGTH(cRight) = 0 THEN 
DO:
    lEmailValidated = NO.
    MESSAGE "EmailValidated:" lEmailValidated VIEW-AS ALERT-BOX.
END.
        
DO iChar = 1 TO LENGTH(cLeft):
    MESSAGE "iChar Left is:" iChar VIEW-AS ALERT-BOX.
    IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-", CAPS(SUBSTRING(cLeft,iChar,1))) = 0 THEN 
    DO:
        MESSAGE "CAPS SUBSTRING" CAPS(SUBSTRING(cLeft,iChar,1)) VIEW-AS ALERT-BOX.
        lEmailValidated = NO.
        MESSAGE "EmailValidated:" lEmailValidated VIEW-AS ALERT-BOX.
    END.
END.  
    
DO iChar = 1 TO LENGTH(cRight):
    MESSAGE "iChar Right is:" iChar VIEW-AS ALERT-BOX.
    IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-", CAPS(SUBSTRING(cRight,iChar,1))) = 0 THEN 
    DO:
        MESSAGE "CAPS SUBSTRING" CAPS(SUBSTRING(cRight,iChar,1)) VIEW-AS ALERT-BOX.
        lEmailValidated = NO.
        MESSAGE "EmailValidated:" lEmailValidated VIEW-AS ALERT-BOX.
    END.            
END.    
lEmailValidated = YES . 
MESSAGE "EmailValidated:" lEmailValidated VIEW-AS ALERT-BOX.   