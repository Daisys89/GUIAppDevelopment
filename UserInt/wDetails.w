&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCustomer NO-UNDO LIKE Customer
       FIELD RowIdent AS ROWID
       FIELD Orders AS INTEGER
       INDEX RowIdent RowIdent.
DEFINE TEMP-TABLE ttSalesrep NO-UNDO LIKE Salesrep
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

//DEFINE OUTPUT PARAMETER TABLE FOR ttCustomer.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ghProcLib  AS HANDLE        NO-UNDO.
DEFINE VARIABLE ghDataUtil AS HANDLE        NO-UNDO.

DEFINE VARIABLE lLastButtons  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFirstButtons AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomer

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME ttCustomer.CustNum ~
ttCustomer.Name ttCustomer.Address ttCustomer.PostalCode ttCustomer.City ~
ttCustomer.State ttCustomer.Country ttCustomer.Phone ~
ttCustomer.EmailAddress ttCustomer.SalesRep 
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ttCustomer SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ttCustomer SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ttCustomer
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ttCustomer


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClose btnFirst btnPrev btnNext btnLast 
&Scoped-Define DISPLAYED-FIELDS ttCustomer.CustNum ttCustomer.Name ~
ttCustomer.Address ttCustomer.PostalCode ttCustomer.City ttCustomer.State ~
ttCustomer.Country ttCustomer.Phone ttCustomer.EmailAddress ~
ttCustomer.SalesRep 
&Scoped-define DISPLAYED-TABLES ttCustomer
&Scoped-define FIRST-DISPLAYED-TABLE ttCustomer


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btnFirst 
     LABEL "<<" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btnLast 
     LABEL ">>" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btnNext 
     LABEL ">" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btnPrev 
     LABEL "<" 
     SIZE 5 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ttCustomer SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ttCustomer.CustNum AT ROW 2 COL 13.2 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     btnClose AT ROW 2.19 COL 75 WIDGET-ID 22
     ttCustomer.Name AT ROW 3 COL 13.2 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     btnFirst AT ROW 3.62 COL 75 WIDGET-ID 24
     btnPrev AT ROW 3.62 COL 80 WIDGET-ID 26
     btnNext AT ROW 3.62 COL 85 WIDGET-ID 28
     btnLast AT ROW 3.62 COL 90 WIDGET-ID 30
     ttCustomer.Address AT ROW 4 COL 13.2 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     ttCustomer.PostalCode AT ROW 5 COL 13.2 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     ttCustomer.City AT ROW 6 COL 13.2 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     ttCustomer.State AT ROW 7 COL 13.2 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ttCustomer.Country AT ROW 8 COL 13.2 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ttCustomer.Phone AT ROW 9 COL 13.2 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ttCustomer.EmailAddress AT ROW 10 COL 13.2 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     ttCustomer.SalesRep AT ROW 11 COL 13.2 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "None","None"
          DROP-DOWN-LIST
          SIZE 27.8 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96 BY 12.38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttCustomer T "?" NO-UNDO sports2000 Customer
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
          FIELD Orders AS INTEGER
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

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer Details"
         HEIGHT             = 12.24
         WIDTH              = 98.4
         MAX-HEIGHT         = 18.24
         MAX-WIDTH          = 99.6
         VIRTUAL-HEIGHT     = 18.24
         VIRTUAL-WIDTH      = 99.6
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN ttCustomer.Address IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.City IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.Country IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.CustNum IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.EmailAddress IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.Name IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.Phone IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.PostalCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX ttCustomer.SalesRep IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.State IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "Temp-Tables.ttCustomer"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer Details */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer Details */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst C-Win
ON CHOOSE OF btnFirst IN FRAME DEFAULT-FRAME /* << */
DO:
        FIND FIRST ttCustomer NO-LOCK.
        IF AVAILABLE ttCustomer THEN 
            DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.   
       
        PUBLISH "CustomerDetailsChanged":U (ttCustomer.CustNum, "First":U). // per default FROM THIS-PROCEDURE.
        PUBLISH "FetchCurrentCust":U(ttCustomer.CustNum).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast C-Win
ON CHOOSE OF btnLast IN FRAME DEFAULT-FRAME /* >> */
DO:
        FIND LAST ttCustomer NO-LOCK.
        IF AVAILABLE ttCustomer THEN 
            DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.
            
        PUBLISH "CustomerDetailsChanged":U (ttCustomer.CustNum, "Last":U).
        PUBLISH "FetchCurrentCust":U (ttCustomer.CustNum).  

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext C-Win
ON CHOOSE OF btnNext IN FRAME DEFAULT-FRAME /* > */
DO:
        FIND NEXT ttCustomer NO-LOCK. 
        IF AVAILABLE ttCustomer THEN 
            DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.

        PUBLISH "CustomerDetailsChanged":U (ttCustomer.CustNum, "Next":U).
        PUBLISH "FetchCurrentCust":U (ttCustomer.CustNum).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrev C-Win
ON CHOOSE OF btnPrev IN FRAME DEFAULT-FRAME /* < */
DO:
        FIND PREV ttCustomer.        
        IF AVAILABLE(ttCustomer) THEN 
            DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.
        
        PUBLISH "CustomerDetailsChanged":U(ttCustomer.CustNum, "Prev":U).
        PUBLISH "FetchCurrentCust":U (ttCustomer.CustNum).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN InitializeObjects.
    RUN enable_UI.
    SUBSCRIBE TO "ValueChangedbrCustomers":U    IN SOURCE-PROCEDURE.
    SUBSCRIBE TO "FetchCurrentCust":U           ANYWHERE.
    SUBSCRIBE TO "CloseWindows":U               ANYWHERE.
    SUBSCRIBE TO "SetButtons":U                 IN SOURCE-PROCEDURE.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseWindows C-Win 
PROCEDURE CloseWindows :
/*------------------------------------------------------------------------------
                      Purpose:     
                      Parameters:  <none>
                      Notes:       
                    ------------------------------------------------------------------------------*/
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE ttCustomer THEN 
    DISPLAY ttCustomer.CustNum ttCustomer.Name ttCustomer.Address 
          ttCustomer.PostalCode ttCustomer.City ttCustomer.State 
          ttCustomer.Country ttCustomer.Phone ttCustomer.EmailAddress 
          ttCustomer.SalesRep 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnClose btnFirst btnPrev btnNext btnLast 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FetchCurrentCust C-Win 
PROCEDURE FetchCurrentCust :
/*------------------------------------------------------------------------------
                      Purpose:     
                      Parameters:  <none>
                      Notes:       
                    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piCustNum AS INTEGER.
    
    FIND FIRST ttCustomer WHERE ttCustomer.CustNum = piCustNum NO-LOCK.
    IF AVAILABLE ttCustomer THEN 
        DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.
    {&WINDOW-NAME}:TITLE = "Details of Customer ":U + ttCustomer.NAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects C-Win 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
                      Purpose:     
                      Parameters:  <none>
                      Notes:       
                    ------------------------------------------------------------------------------*/
    RUN PersistentProc.p PERSISTENT SET ghProcLib.

    ghDataUtil = DYNAMIC-FUNCTION('RunPersistent' IN ghProcLib, "DataUtil.p":U).

    RUN GetCustData IN ghDataUtil(OUTPUT TABLE ttCustomer).

    DO WITH FRAME {&FRAME-NAME}:
        ttCustomer.SalesRep:DELIMITER = ";":U.
        RUN GetRepData IN ghDataUtil(OUTPUT TABLE ttSalesRep).
        FOR EACH ttSalesRep:
            ttCustomer.SalesRep:ADD-LAST(ttSalesRep.RepName, ttSalesRep.SalesRep).
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetButtons C-Win 
PROCEDURE SetButtons :
/*------------------------------------------------------------------------------
Purpose:
Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cButtonSwitch AS CHARACTER NO-UNDO.
    CASE cButtonSwitch:
        WHEN "DisableFirst" THEN 
            lFirstButtons = NO. 
        WHEN "EnableFirst" THEN 
            lFirstButtons = YES.
        WHEN "DisableLast" THEN 
            lLastButtons = NO.
        WHEN "EnableLast" THEN 
            lLastButtons = YES.           
    END CASE.     
    
    IF lFirstButtons THEN 
    DO: 
        ENABLE btnFirst btnPrev WITH FRAME {&FRAME-NAME}.
    END.
    ELSE 
    DO: 
        DISABLE btnFirst btnPrev WITH FRAME {&FRAME-NAME}.        
    END.
    
    IF lLastButtons THEN 
    DO:
        ENABLE btnNext btnLast WITH FRAME {&FRAME-NAME}.
    END.
    ELSE 
    DO: 
        DISABLE btnNext btnLast WITH FRAME {&FRAME-NAME}.
    END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedbrCustomers C-Win 
PROCEDURE ValueChangedbrCustomers :
/*------------------------------------------------------------------------------
                      Purpose:     
                      Parameters:  <none>
                      Notes:       
                    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piCustNum AS INTEGER.
    FIND FIRST ttCustomer WHERE ttCustomer.CustNum = piCustNum.
    DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

