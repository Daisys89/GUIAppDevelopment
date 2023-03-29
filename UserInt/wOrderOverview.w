&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttOrder NO-UNDO LIKE Order
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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ghProcLib  AS HANDLE        NO-UNDO.
DEFINE VARIABLE ghDataUtil AS HANDLE        NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brOrder

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttOrder

/* Definitions for BROWSE brOrder                                       */
&Scoped-define FIELDS-IN-QUERY-brOrder ttOrder.CustNum ttOrder.Ordernum ~
ttOrder.OrderDate ttOrder.OrderStatus ttOrder.PromiseDate ttOrder.ShipDate 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brOrder 
&Scoped-define QUERY-STRING-brOrder FOR EACH ttOrder NO-LOCK
&Scoped-define OPEN-QUERY-brOrder OPEN QUERY brOrder FOR EACH ttOrder NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brOrder ttOrder
&Scoped-define FIRST-TABLE-IN-QUERY-brOrder ttOrder


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brOrder}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brOrder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brOrder FOR 
      ttOrder SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brOrder C-Win _STRUCTURED
  QUERY brOrder NO-LOCK DISPLAY
      ttOrder.CustNum FORMAT ">>>>9":U
      ttOrder.Ordernum FORMAT "zzzzzzzzz9":U WIDTH 10.2
      ttOrder.OrderDate COLUMN-LABEL "Order Date" FORMAT "99/99/99":U
            WIDTH 11.2
      ttOrder.OrderStatus FORMAT "x(20)":U WIDTH 17.2
      ttOrder.PromiseDate COLUMN-LABEL "Promise Date" FORMAT "99/99/99":U
            WIDTH 13.2
      ttOrder.ShipDate COLUMN-LABEL "Ship Date" FORMAT "99/99/9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 8.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brOrder AT ROW 1.48 COL 3 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91 BY 16.43 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttOrder T "?" NO-UNDO sports2000 Order
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
         TITLE              = "Order Overview"
         HEIGHT             = 10.19
         WIDTH              = 91.2
         MAX-HEIGHT         = 48.43
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48.43
         VIRTUAL-WIDTH      = 384
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
/* BROWSE-TAB brOrder 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brOrder
/* Query rebuild information for BROWSE brOrder
     _TblList          = "Temp-Tables.ttOrder"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.ttOrder.CustNum
     _FldNameList[2]   > Temp-Tables.ttOrder.Ordernum
"ttOrder.Ordernum" ? ? "integer" ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ttOrder.OrderDate
"ttOrder.OrderDate" "Order Date" ? "date" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.ttOrder.OrderStatus
"ttOrder.OrderStatus" ? ? "character" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.ttOrder.PromiseDate
"ttOrder.PromiseDate" "Promise Date" ? "date" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.ttOrder.ShipDate
"ttOrder.ShipDate" "Ship Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brOrder */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Order Overview */
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
ON WINDOW-CLOSE OF C-Win /* Order Overview */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brOrder
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
    SUBSCRIBE TO "CustomerDetailsChanged":U     ANYWHERE.
    SUBSCRIBE TO "FetchCurrentCust":U           IN SOURCE-PROCEDURE.
    //SUBSCRIBE TO "CloseWindows":U               ANYWHERE.
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
     Notes:
    ------------------------------------------------------------------------------*/
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustomerDetailsChanged C-Win 
PROCEDURE CustomerDetailsChanged :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piCustNum AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pcCustDetailsChanged AS CHARACTER NO-UNDO.

    CASE pcCustDetailsChanged:
        WHEN "First":U THEN
            DO:
                OPEN QUERY brOrder FOR EACH ttOrder NO-LOCK  
                    WHERE ttOrder.CustNum = piCustNum.    
            END.
        WHEN "Prev":U THEN
            DO:
                OPEN QUERY brOrder FOR EACH ttOrder NO-LOCK  
                    WHERE ttOrder.CustNum = piCustNum.    
            END.
        WHEN "Next":U THEN
            DO:
                OPEN QUERY brOrder FOR EACH ttOrder NO-LOCK  
                    WHERE ttOrder.CustNum = piCustNum.    
            END.
        WHEN "Last":U THEN
            DO:
                OPEN QUERY brOrder FOR EACH ttOrder NO-LOCK  
                    WHERE ttOrder.CustNum = piCustNum.   
            END.
    END CASE.

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
  ENABLE brOrder 
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
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piCustNum AS INTEGER.
    DEFINE INPUT PARAMETER piName AS CHARACTER.
    
    RUN getOrderData IN ghDataUtil (INPUT piCustNum,OUTPUT TABLE ttOrder).
    {&OPEN-QUERY-brOrder}
    
/*    FIND FIRST Customer WHERE Customer.CustNum = piCustNum NO-LOCK.*/
/*    IF AVAILABLE Customer THEN                                     */
/*        OPEN QUERY brOrder FOR EACH ttOrder NO-LOCK                */
/*            WHERE ttOrder.CustNum = piCustNum.                     */

    {&WINDOW-NAME}:TITLE = "Orders of Customer ":U + piName.

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

    /* Run the business logic utility persistently and retrieve its handle*/
    ghDataUtil = DYNAMIC-FUNCTION('RunPersistent' IN ghProcLib, "DataUtil.p":U).

    // RUN getOrderData IN ghDataUtil (OUTPUT TABLE ttOrder).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedbrCustomers C-Win 
PROCEDURE ValueChangedbrCustomers :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piCustNum AS INTEGER.
    OPEN QUERY brOrder FOR EACH ttOrder NO-LOCK  
        WHERE ttOrder.CustNum = piCustNum.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

