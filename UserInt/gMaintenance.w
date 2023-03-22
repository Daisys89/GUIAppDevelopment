&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCustomerUpd NO-UNDO LIKE Customer
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent.
DEFINE TEMP-TABLE ttSalesrep NO-UNDO LIKE Salesrep
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER pcMode    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER phProcLib AS HANDLE     NO-UNDO.
DEFINE INPUT PARAMETER prowRowid AS ROWID      NO-UNDO.

DEFINE OUTPUT PARAMETER TABLE FOR ttCustomerUpd.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE glResponse AS LOGICAL NO-UNDO.
DEFINE VARIABLE ghDataUtil AS HANDLE  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomerUpd

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame ttCustomerUpd.CustNum ~
ttCustomerUpd.Name ttCustomerUpd.Address ttCustomerUpd.PostalCode ~
ttCustomerUpd.City ttCustomerUpd.State ttCustomerUpd.Country ~
ttCustomerUpd.Phone ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame ttCustomerUpd.Name ~
ttCustomerUpd.Address ttCustomerUpd.PostalCode ttCustomerUpd.City ~
ttCustomerUpd.State ttCustomerUpd.Country ttCustomerUpd.Phone ~
ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame ttCustomerUpd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame ttCustomerUpd
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ttCustomerUpd SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ttCustomerUpd SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ttCustomerUpd
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ttCustomerUpd


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ttCustomerUpd.Name ttCustomerUpd.Address ~
ttCustomerUpd.PostalCode ttCustomerUpd.City ttCustomerUpd.State ~
ttCustomerUpd.Country ttCustomerUpd.Phone ttCustomerUpd.EmailAddress ~
ttCustomerUpd.SalesRep 
&Scoped-define ENABLED-TABLES ttCustomerUpd
&Scoped-define FIRST-ENABLED-TABLE ttCustomerUpd
&Scoped-Define ENABLED-OBJECTS Btn_Save Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS ttCustomerUpd.CustNum ttCustomerUpd.Name ~
ttCustomerUpd.Address ttCustomerUpd.PostalCode ttCustomerUpd.City ~
ttCustomerUpd.State ttCustomerUpd.Country ttCustomerUpd.Phone ~
ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep 
&Scoped-define DISPLAYED-TABLES ttCustomerUpd
&Scoped-define FIRST-DISPLAYED-TABLE ttCustomerUpd


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CorrectCustomerInput Dialog-Frame 
FUNCTION CorrectCustomerInput RETURNS CHARACTER
    ( INPUT cInput AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CorrectPostalCodeInput Dialog-Frame 
FUNCTION CorrectPostalCodeInput RETURNS CHARACTER
    ( INPUT cPostalCode AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateEmail Dialog-Frame 
FUNCTION ValidateEmail RETURNS LOGICAL
    ( INPUT cEmail AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidatePostalCode Dialog-Frame 
FUNCTION ValidatePostalCode RETURNS LOGICAL
    ( INPUT cPostalCode AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Save AUTO-GO 
     LABEL "Save" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ttCustomerUpd SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ttCustomerUpd.CustNum AT ROW 1.48 COL 13 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     ttCustomerUpd.Name AT ROW 2.48 COL 13 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Address AT ROW 3.48 COL 13 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          BGCOLOR 14 
     ttCustomerUpd.PostalCode AT ROW 4.48 COL 13 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 14 
     ttCustomerUpd.City AT ROW 5.48 COL 13 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 14 
     ttCustomerUpd.State AT ROW 6.48 COL 13 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ttCustomerUpd.Country AT ROW 7.48 COL 13 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Phone AT ROW 8.48 COL 13 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ttCustomerUpd.EmailAddress AT ROW 9.48 COL 13 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          BGCOLOR 14 
     ttCustomerUpd.SalesRep AT ROW 10.48 COL 13 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "None","None"
          DROP-DOWN-LIST
          SIZE 16 BY 1
          BGCOLOR 14 
     Btn_Save AT ROW 1.95 COL 77
     Btn_Cancel AT ROW 3.33 COL 77
     SPACE(3.19) SKIP(7.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Customer Maintenance"
         DEFAULT-BUTTON Btn_Save WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttCustomerUpd T "?" NO-UNDO sports2000 Customer
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
          INDEX RowIdent RowIdent
      END-FIELDS.
      TABLE: ttSalesrep T "?" NO-UNDO sports2000 Salesrep
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
          INDEX RowIdent RowIdent
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ttCustomerUpd.CustNum IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "Temp-Tables.ttCustomerUpd"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Customer Maintenance */
DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttCustomerUpd.Address
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.Address Dialog-Frame
ON LEAVE OF ttCustomerUpd.Address IN FRAME Dialog-Frame /* Address */
DO:
        ttCustomerUpd.Address:SCREEN-VALUE = CorrectCustomerInput(ttCustomerUpd.Address:SCREEN-VALUE).  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
        DEFINE VARIABLE lAnswer AS LOGICAL NO-UNDO.
        
        MESSAGE SUBSTITUTE("Are you sure you want to loose the changes you made to customer '&1' ?", ttCustomerUpd.Name)
            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lAnswer. 
        
        IF lAnswer THEN
            APPLY "CLOSE":U TO THIS-PROCEDURE. 
        ELSE 
            RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save Dialog-Frame
ON CHOOSE OF Btn_Save IN FRAME Dialog-Frame /* Save */
DO:
        DEFINE VARIABLE lAnswer              AS LOGICAL NO-UNDO.  
        DEFINE VARIABLE lEmailValidated      AS LOGICAL NO-UNDO.
        DEFINE VARIABLE lPostalCodeValidated AS LOGICAL NO-UNDO.        
        
        lEmailValidated      = ValidateEmail(ttCustomerUpd.EmailAddress:SCREEN-VALUE). 
        lPostalCodeValidated = ValidatePostalCode(ttCustomerUpd.PostalCode:SCREEN-VALUE).
              
        IF ttCustomerUpd.Name:SCREEN-VALUE = "" THEN 
        DO:
            MESSAGE "You forgot to enter the name.":U
                VIEW-AS ALERT-BOX.
            APPLY "ENTRY":U TO ttCustomerUpd.Name IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY .   
        END.  
        IF ttCustomerUpd.Address:SCREEN-VALUE = "" THEN 
        DO:
            MESSAGE "You forgot to enter the address.":U
                VIEW-AS ALERT-BOX.
            APPLY "ENTRY":U TO ttCustomerUpd.Address IN FRAME {&FRAME-NAME}. 
            RETURN NO-APPLY .   
        END.
        IF NOT lPostalCodeValidated THEN 
        DO:
            MESSAGE "You have entered an incorrect postal code. Please use format 1234AB."
                VIEW-AS ALERT-BOX.        
            APPLY "ENTRY":U TO ttCustomerUpd.PostalCode IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
              
        END.
        IF ttCustomerUpd.PostalCode:SCREEN-VALUE = "" THEN 
        DO: 
            MESSAGE "You forgot to enter the postal code.":U
                VIEW-AS ALERT-BOX.
            APPLY "ENTRY":U TO ttCustomerUpd.PostalCode IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.    
        END.
        IF ttCustomerUpd.City:SCREEN-VALUE = "" THEN 
        DO:
            MESSAGE "You forgot to enter the city.":U
                VIEW-AS ALERT-BOX.
            APPLY "ENTRY":U TO ttCustomerUpd.City IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
        IF ttCustomerUpd.Country:SCREEN-VALUE = "" THEN 
        DO:
            MESSAGE "You forgot to enter the country.":U
                VIEW-AS ALERT-BOX. 
            APPLY "ENTRY":U TO ttCustomerUpd.Country IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
        IF NOT lEmailValidated THEN 
        DO:
            MESSAGE "You have entered an invalid email address."
                VIEW-AS ALERT-BOX.
            APPLY "ENTRY":U TO ttCustomerUpd.EmailAddress IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
            
        IF ttCustomerUpd.EmailAddress:INPUT-VALUE = "" THEN 
        DO: 
            MESSAGE "You forgot to enter the email address.":U
                VIEW-AS ALERT-BOX.
            APPLY "ENTRY":U TO ttCustomerUpd.EmailAddress IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END. 
        IF ttCustomerUpd.SalesRep:INPUT-VALUE = "" THEN 
        DO:
            MESSAGE "You forgot to select the salesrep.":U 
                VIEW-AS ALERT-BOX.
            APPLY "ENTRY":U TO ttCustomerUpd.SalesRep IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
        ELSE 
            MESSAGE "Are you sure you want to save this information?"
                VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lAnswer. 
            
        IF lAnswer THEN 
        DO:                          
            /* Copy screen values to memory */
            ASSIGN {&DISPLAYED-FIELDS}.
            /* Commit the data to the database */
            RUN SaveCustRecord IN ghDataUtil (INPUT-OUTPUT TABLE ttCustomerUpd, 
                INPUT pcMode).
            IF RETURN-VALUE <> "" THEN
            DO:
                MESSAGE RETURN-VALUE
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN RETURN-VALUE.
            END.
            ELSE
            DO:
                FIND FIRST ttCustomerUpd.
                IF pcMode = "New":U THEN 
                DO:
                    pcMode = "Mod":U.   /* Set mode to Modify after creation */
                END.
                /* Refresh the display */
                DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.
            END.
        END.
        ELSE 
            RETURN NO-APPLY.    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttCustomerUpd.City
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.City Dialog-Frame
ON LEAVE OF ttCustomerUpd.City IN FRAME Dialog-Frame /* City */
DO:
        ttCustomerUpd.City:SCREEN-VALUE = CorrectCustomerInput(ttCustomerUpd.City:SCREEN-VALUE).  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttCustomerUpd.Country
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.Country Dialog-Frame
ON LEAVE OF ttCustomerUpd.Country IN FRAME Dialog-Frame /* Country */
DO:
        ttCustomerUpd.Country:SCREEN-VALUE = CorrectCustomerInput(ttCustomerUpd.Country:SCREEN-VALUE).  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttCustomerUpd.Name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.Name Dialog-Frame
ON LEAVE OF ttCustomerUpd.Name IN FRAME Dialog-Frame /* Name */
DO:
        ttCustomerUpd.Name:SCREEN-VALUE = CorrectCustomerInput(ttCustomerUpd.NAME:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttCustomerUpd.PostalCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.PostalCode Dialog-Frame
ON LEAVE OF ttCustomerUpd.PostalCode IN FRAME Dialog-Frame /* Postal Code */
DO:   
        ttCustomerUpd.PostalCode:SCREEN-VALUE = CorrectPostalCodeInput(ttCustomerUpd.PostalCode:SCREEN-VALUE).   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttCustomerUpd.State
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.State Dialog-Frame
ON LEAVE OF ttCustomerUpd.State IN FRAME Dialog-Frame /* State */
DO:
        ttCustomerUpd.State:SCREEN-VALUE = CorrectCustomerInput(ttCustomerUpd.State:SCREEN-VALUE).  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN InitializeObjects.
    RUN enable_UI.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  IF AVAILABLE ttCustomerUpd THEN 
    DISPLAY ttCustomerUpd.CustNum ttCustomerUpd.Name ttCustomerUpd.Address 
          ttCustomerUpd.PostalCode ttCustomerUpd.City ttCustomerUpd.State 
          ttCustomerUpd.Country ttCustomerUpd.Phone ttCustomerUpd.EmailAddress 
          ttCustomerUpd.SalesRep 
      WITH FRAME Dialog-Frame.
  ENABLE ttCustomerUpd.Name ttCustomerUpd.Address ttCustomerUpd.PostalCode 
         ttCustomerUpd.City ttCustomerUpd.State ttCustomerUpd.Country 
         ttCustomerUpd.Phone ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep 
         Btn_Save Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects Dialog-Frame 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
                              Purpose:     
                              Parameters:  <none>
                              Notes:       
                            ------------------------------------------------------------------------------*/
    ghDataUtil = DYNAMIC-FUNCTION('RunPersistent' IN phProcLib, "DataUtil.p":U).
    DO WITH FRAME {&FRAME-NAME}:
        ttCustomerUpd.SalesRep:DELIMITER = ";":U.
        RUN GetRepData IN ghDataUtil(OUTPUT TABLE ttSalesRep).
        FOR EACH ttSalesRep:
            ttCustomerUpd.SalesRep:ADD-LAST(ttSalesRep.RepName, ttSalesRep.SalesRep).
        END.
    END.
 
    IF pcMode = "Mod":U THEN
    DO:
        RUN GetCustRecord IN ghDataUtil (OUTPUT TABLE ttCustomerUpd,
            INPUT prowRowId).
        IF RETURN-VALUE = "" THEN
            FIND FIRST ttCustomerUpd.  
        FRAME Dialog-Frame:TITLE = "Edit Customer: ":U + ttCustomerUpd.Name. 
    END.
    ELSE 
    DO:
        CREATE ttCustomerUpd.
        ttCustomerUpd.Country = "".
        FRAME Dialog-Frame:TITLE = "Create New Customer ":U + ttCustomerUpd.Name.
    END.
    

    PUBLISH "CloseWindows":U .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CorrectCustomerInput Dialog-Frame 
FUNCTION CorrectCustomerInput RETURNS CHARACTER
    ( INPUT cInput AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOutput AS CHARACTER NO-UNDO.
        
    IF cInput = "" THEN RETURN "".
        
    DO iCount = 1 TO NUM-ENTRIES(cInput, " "): // loops through user input, gives number of entries .
        ASSIGN 
            cOutput = cOutput + CAPS(SUBSTRING(ENTRY(iCount,cInput," "),1,1)) +
                                LC(SUBSTRING(ENTRY(iCount,cInput," "),2, LENGTH(ENTRY(iCount,cInput," ")))) + " ".
    END.                           
        
    RETURN cOutput.                         

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CorrectPostalCodeInput Dialog-Frame 
FUNCTION CorrectPostalCodeInput RETURNS CHARACTER
    ( INPUT cPostalCode AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cOutputEmail AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPCNumbers   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPCLetters   AS CHARACTER NO-UNDO.

    IF cPostalCode = "" THEN RETURN "".
    
    cPostalCode = REPLACE(
        REPLACE(
        REPLACE(
        REPLACE(
        REPLACE(
        REPLACE(
        REPLACE(
        REPLACE(cPostalCode, " ", ""), "-", ""), ",", ""), ".", ""), "=", ""), "/", ""), "\", ""), "+", "").

    cPostalCode = TRIM(cPostalCode).

    cPCNumbers = SUBSTRING(cPostalCode,1,4).
    cPCLetters = SUBSTRING(cPostalCode,5,6).

    DO: 
        cOutputEmail = cPCNumbers + CAPS(cPCLetters).
    END.    

    RETURN cOutputEmail.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateEmail Dialog-Frame 
FUNCTION ValidateEmail RETURNS LOGICAL
    ( INPUT cEmail AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iChar   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLength AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cLeft   AS CHARACTER FORMAT "x(50)" NO-UNDO.
    DEFINE VARIABLE cRight  AS CHARACTER FORMAT "x(50)" NO-UNDO.
    DEFINE VARIABLE iAt     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iDot    AS INTEGER   NO-UNDO.
        
    cEmail = TRIM(cEmail).
    iLength = LENGTH(cEmail).
        
    IF iLength < 5 THEN 
        RETURN FALSE.

    iAt = INDEX(cEmail,"@").
    cLeft = SUBSTRING(cEmail,1,(iAt - 1)).
    cRight = SUBSTRING(cEmail, (iAt + 1), (iLength - (iAt ))).
    iDot = INDEX(cRight,".").
        
    IF iAt = 0 OR iDot = 0 OR LENGTH(cLeft) = 0 OR LENGTH(cRight) = 0 THEN 
    DO:
        RETURN FALSE.
    END.
        
    DO iChar = 1 TO LENGTH(cLeft):
        IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-", CAPS(SUBSTRING(cLeft,iChar,1))) = 0 THEN 
        DO:
            RETURN FALSE.
        END.
    END.  
    
    DO iChar = 1 TO LENGTH(cRight):
        IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-", CAPS(SUBSTRING(cRight,iChar,1))) = 0 THEN 
        DO:
            RETURN FALSE.
        END.            
    END.    
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidatePostalCode Dialog-Frame 
FUNCTION ValidatePostalCode RETURNS LOGICAL
    ( INPUT cPostalCode AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Only check if its NL customer.
     Notes:  ADD AN INDEX? TO VERIFY IF FIRST 4 ARE NUMBERS AND LAST 2 ARE LETTERS?? 
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIndex     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cPCNumbers AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPCLetters AS CHARACTER NO-UNDO.
  
    cPCNumbers = SUBSTRING(cPostalCode,1,4).
    cPCLetters = SUBSTRING(cPostalCode,5,6).    

    IF LENGTH(cPostalCode) = 6 AND LENGTH(cPCNumbers) = 4 AND LENGTH(cPCLetters) = 2 THEN 
    DO: 
        RETURN TRUE.
    END.
   
    IF LENGTH(cPostalCode) <> 6 OR LENGTH(cPCNumbers) <> 4 OR LENGTH(cPCLetters) <> 2 THEN 
    DO: 
        RETURN FALSE.
    END.      
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

