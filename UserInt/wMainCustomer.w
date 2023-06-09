&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCustomer NO-UNDO LIKE Customer
       FIELD RowIdent AS ROWID
       FIELD NumOrders AS INTEGER
       INDEX CustNum CustNum
       INDEX RowIdent RowIdent.
DEFINE TEMP-TABLE ttOrder NO-UNDO LIKE Order
       FIELD RowIdent AS ROWID.
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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ghProcLib     AS HANDLE    NO-UNDO.
DEFINE VARIABLE ghDataUtil    AS HANDLE    NO-UNDO.

DEFINE VARIABLE hDetails      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hOrders       AS HANDLE    NO-UNDO.

DEFINE VARIABLE gcWhereClause AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcSortClause  AS CHARACTER NO-UNDO.



DEFINE VARIABLE giCustNum     AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttCustomerUpd NO-UNDO LIKE ttCustomer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brCustomer

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomer

/* Definitions for BROWSE brCustomer                                    */
&Scoped-define FIELDS-IN-QUERY-brCustomer ttCustomer.CustNum ~
ttCustomer.Name NumOrders 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brCustomer 
&Scoped-define QUERY-STRING-brCustomer FOR EACH ttCustomer NO-LOCK
&Scoped-define OPEN-QUERY-brCustomer OPEN QUERY brCustomer FOR EACH ttCustomer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brCustomer ttCustomer
&Scoped-define FIRST-TABLE-IN-QUERY-brCustomer ttCustomer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brCustomer}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brCustomer fiCustNum fiCustName btnOrders 
&Scoped-Define DISPLAYED-OBJECTS fiCustNum fiCustName fiRepName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetWhereClause C-Win 
FUNCTION GetWhereClause RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Customer 
       MENU-ITEM m_Details      LABEL "Details"       
       MENU-ITEM m_New          LABEL "New"           
       MENU-ITEM m_Edit         LABEL "Edit"          
       RULE
       MENU-ITEM m_Delete       LABEL "Delete"        
       RULE
       MENU-ITEM m_Exit         LABEL "Exit"          .

DEFINE SUB-MENU m_Navigate 
       MENU-ITEM m_First        LABEL "First"         
       MENU-ITEM m_Last         LABEL "Last"          .

DEFINE SUB-MENU m_Sort_By 
       MENU-ITEM m_Cust_Num     LABEL "Cust Num"      
       MENU-ITEM m_Name         LABEL "Name"          
       MENU-ITEM m_NumOrders    LABEL "NumOrders"     .

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_Customer     LABEL "Customer"      
       SUB-MENU  m_Navigate     LABEL "Navigate"      
       SUB-MENU  m_Sort_By      LABEL "Sort By"       .

DEFINE MENU POPUP-MENU-brCustomer 
       MENU-ITEM m_brEdit       LABEL "Edit"          
       RULE
       MENU-ITEM m_brDelete     LABEL "Delete"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnOrders 
     LABEL "Show orders" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiCustName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE fiCustNum AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiRepName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rep Name" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brCustomer FOR 
      ttCustomer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brCustomer C-Win _STRUCTURED
  QUERY brCustomer NO-LOCK DISPLAY
      ttCustomer.CustNum FORMAT ">>>>9":U WIDTH 11.4
      ttCustomer.Name FORMAT "x(30)":U WIDTH 54.2
      NumOrders COLUMN-LABEL "NumOrders"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86.2 BY 8.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brCustomer AT ROW 1.43 COL 2.8 WIDGET-ID 200
     fiCustNum AT ROW 9.81 COL 2.8 NO-LABEL WIDGET-ID 2
     fiCustName AT ROW 9.81 COL 15.4 NO-LABEL WIDGET-ID 4
     fiRepName AT ROW 11.48 COL 15 COLON-ALIGNED WIDGET-ID 70
     btnOrders AT ROW 11.48 COL 73.8 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1
         SIZE 100.8 BY 14.33 WIDGET-ID 100.


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
          FIELD NumOrders AS INTEGER
          INDEX CustNum CustNum
          INDEX RowIdent RowIdent
      END-FIELDS.
      TABLE: ttOrder T "?" NO-UNDO sports2000 Order
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
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
         TITLE              = "Customers"
         HEIGHT             = 12.19
         WIDTH              = 91.2
         MAX-HEIGHT         = 48.43
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48.43
         VIRTUAL-WIDTH      = 384
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brCustomer 1 DEFAULT-FRAME */
ASSIGN 
       brCustomer:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU POPUP-MENU-brCustomer:HANDLE
       brCustomer:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR FILL-IN fiCustName IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiCustNum IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiRepName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brCustomer
/* Query rebuild information for BROWSE brCustomer
     _TblList          = "Temp-Tables.ttCustomer"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ttCustomer.CustNum
"ttCustomer.CustNum" ? ? "integer" ? ? ? ? ? ? no ? no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ttCustomer.Name
"ttCustomer.Name" ? ? "character" ? ? ? ? ? ? no ? no no "54.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"NumOrders" "NumOrders" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brCustomer */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customers */
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
ON WINDOW-CLOSE OF C-Win /* Customers */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brCustomer
&Scoped-define SELF-NAME brCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomer C-Win
ON DEFAULT-ACTION OF brCustomer IN FRAME DEFAULT-FRAME
DO:
        IF NOT VALID-HANDLE(hDetails)
            THEN DO:
                RUN wDetails.w PERSISTENT SET hDetails.   
                SUBSCRIBE TO "CustomerDetailsChanged":U IN hDetails.
            END.
        RUN "SwitchNavButtons". 
        PUBLISH "FetchCurrentCust":U (ttCustomer.CustNum, ttCustomer.Name).  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomer C-Win
ON START-SEARCH OF brCustomer IN FRAME DEFAULT-FRAME
DO:
        DEFINE VARIABLE rRowid AS ROWID NO-UNDO.
        DEFINE VARIABLE iRow   AS INTEGER NO-UNDO.
        
        ASSIGN rRowid = ROWID(ttCustomer)
               iRow = BROWSE brCustomer:FOCUSED-ROW.
        
        gcSortClause = "BY " + BROWSE brCustomer:CURRENT-COLUMN:NAME.
        RUN ReopenQuery.
        
        brCustomer:SET-REPOSITIONED-ROW (iRow).
        brCustomer:QUERY:REPOSITION-TO-ROWID (rRowid).        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomer C-Win
ON VALUE-CHANGED OF brCustomer IN FRAME DEFAULT-FRAME
DO:                
        FOR EACH SalesRep WHERE Salesrep.SalesRep = ttCustomer.SalesRep:
            fiRepName = Salesrep.RepName.
        END.
        
        DISPLAY fiRepName WITH FRAME {&FRAME-NAME}.
        
        RUN "SwitchNavButtons". 
        PUBLISH "ValueChangedbrCustomers":U (ttCustomer.CustNum). 
        PUBLISH "FetchCurrentCust":U (ttCustomer.CustNum, ttCustomer.Name).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOrders C-Win
ON CHOOSE OF btnOrders IN FRAME DEFAULT-FRAME /* Show orders */
DO:
        IF NOT VALID-HANDLE(hOrders)
            THEN RUN wOrderOverview.w PERSISTENT SET hOrders.
        
        PUBLISH "FetchCurrentCust":U (ttCustomer.CustNum, ttCustomer.Name).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustName C-Win
ON VALUE-CHANGED OF fiCustName IN FRAME DEFAULT-FRAME
DO:  
    RUN ReopenQuery.
                                                                                           
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustNum C-Win
ON VALUE-CHANGED OF fiCustNum IN FRAME DEFAULT-FRAME
DO:
        RUN ReopenQuery.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_brDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_brDelete C-Win
ON CHOOSE OF MENU-ITEM m_brDelete /* Delete */
DO:
        RUN DeleteCustomer.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_brEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_brEdit C-Win
ON CHOOSE OF MENU-ITEM m_brEdit /* Edit */
DO:
        RUN EditCustomer.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Cust_Num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Cust_Num C-Win
ON CHOOSE OF MENU-ITEM m_Cust_Num /* Cust Num */
DO:
  gcSortClause = "BY ttCustomer.CustNum".
  RUN ReopenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Delete C-Win
ON CHOOSE OF MENU-ITEM m_Delete /* Delete */
DO:
        RUN DeleteCustomer. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Details
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Details C-Win
ON CHOOSE OF MENU-ITEM m_Details /* Details */
DO:
        APPLY "DEFAULT-ACTION":U TO brCustomer IN FRAME DEFAULT-FRAME.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Edit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Edit C-Win
ON CHOOSE OF MENU-ITEM m_Edit /* Edit */
DO:
        RUN EditCustomer.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit C-Win
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_First
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_First C-Win
ON CHOOSE OF MENU-ITEM m_First /* First */
DO:
    APPLY "HOME" TO BROWSE {&BROWSE-NAME}.
    APPLY "VALUE-CHANGED" TO brCustomer IN FRAME {&FRAME-NAME}.
    PUBLISH "FetchCurrentCust":U (ttCustomer.CustNum, ttCustomer.Name).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Last C-Win
ON CHOOSE OF MENU-ITEM m_Last /* Last */
DO:
    APPLY "END" TO BROWSE {&BROWSE-NAME}.
    APPLY "VALUE-CHANGED" TO brCustomer IN FRAME {&FRAME-NAME}.
    PUBLISH "FetchCurrentCust":U (ttCustomer.CustNum, ttCustomer.Name).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Name C-Win
ON CHOOSE OF MENU-ITEM m_Name /* Name */
DO:
  gcSortClause = "BY ttCustomer.Name".
  RUN ReopenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_New
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_New C-Win
ON CHOOSE OF MENU-ITEM m_New /* New */
DO:
        RUN NewCustomer.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_NumOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_NumOrders C-Win
ON CHOOSE OF MENU-ITEM m_NumOrders /* NumOrders */
DO:
    gcSortClause = "BY ttCustomer.NumOrders".
    RUN ReopenQuery.
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
    APPLY "VALUE-CHANGED":U TO brCustomer.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustomerDetailsChanged C-Win 
PROCEDURE CustomerDetailsChanged :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piCustNum AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pcCustDetailsChanged AS CHARACTER NO-UNDO.

    CASE pcCustDetailsChanged:
        WHEN "First":U THEN
            APPLY "HOME" TO BROWSE {&BROWSE-NAME}.    
        WHEN "Prev":U THEN
            BROWSE brCustomer:SELECT-PREV-ROW () NO-ERROR.
        WHEN "Next":U THEN
            BROWSE brCustomer:SELECT-NEXT-ROW () NO-ERROR.
        WHEN "Last":U THEN
            APPLY "END" TO BROWSE {&BROWSE-NAME}.
    END CASE.
    
    APPLY "VALUE-CHANGED" TO brCustomer IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteCustomer C-Win 
PROCEDURE DeleteCustomer :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE glResponse AS LOGICAL NO-UNDO.

    MESSAGE "Are you sure you want to delete this customer?":U 
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE glResponse.
    IF glResponse THEN 
    DO: 
        RUN DeleteCustomer IN ghDataUtil(INPUT ttCustomer.RowIdent).
        IF RETURN-VALUE = "" OR RETURN-VALUE MATCHES "*deleted*":U THEN
        DO:
            DELETE ttCustomer.
            brCustomer:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME}.
        END.
        ELSE 
        DO:
            MESSAGE RETURN-VALUE 
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END.
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditCustomer C-Win 
PROCEDURE EditCustomer :
/*------------------------------------------------------------------------------
    Purpose:
    Notes: EXCLUSIVE LOCK?
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rowRowIdent AS ROWID NO-UNDO.
    
    RUN gMaintenance.w (INPUT "Mod":U,
        INPUT ghProcLib,
        INPUT ttCustomer.RowIdent,
        OUTPUT TABLE ttCustomerUpd). 
        
    FIND FIRST ttCustomerUpd.
    rowRowIdent = ttCustomerUpd.rowIdent.
    BUFFER-COPY ttCustomerUpd TO ttCustomer.
    
    RUN ReopenQuery.
    
    FIND ttCustomer WHERE ttCustomer.rowIdent = rowRowIdent.
    REPOSITION brCustomer TO ROWID ROWID(ttCustomer).       
    
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
  DISPLAY fiCustNum fiCustName fiRepName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brCustomer fiCustNum fiCustName btnOrders 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
 
    RUN GetCustData IN ghDataUtil (OUTPUT TABLE ttCustomer).
 
    brCustomer:LOAD-MOUSE-POINTER("Glove":U) IN FRAME {&FRAME-NAME}.
    
    APPLY "VALUE-CHANGED" TO brCustomer.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewCustomer C-Win 
PROCEDURE NewCustomer :
/*------------------------------------------------------------------------------
    Purpose:
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rowRowIdent AS ROWID NO-UNDO.
    
    RUN gMaintenance.w (INPUT "New":U,
        INPUT ghProcLib,
        INPUT ttCustomer.RowIdent,
        OUTPUT TABLE ttCustomerUpd).
    FIND FIRST ttCustomerUpd NO-ERROR.
    IF AVAILABLE ttCustomerUpd THEN 
    DO:
        rowRowIdent = ttCustomerUpd.rowIdent.
        CREATE ttCustomer.
        BUFFER-COPY ttCustomerUpd TO ttCustomer.
        RUN ReopenQuery.
        FIND ttCustomer WHERE ttCustomer.rowIdent = rowRowIdent.
        REPOSITION brCustomer TO ROWID ROWID(ttCustomer).
    END.    
            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReopenQuery C-Win 
PROCEDURE ReopenQuery :
/*------------------------------------------------------------------------------
    Purpose:
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hQueryHandle  AS HANDLE    NO-UNDO.
    
    
    hQueryHandle = BROWSE brCustomer:QUERY.
    hQueryHandle:QUERY-CLOSE ().
    hQueryHandle:QUERY-PREPARE(SUBSTITUTE("PRESELECT EACH ttCustomer NO-LOCK &1 &2":U, gcSortClause, GetWhereClause())).
    hQueryHandle:QUERY-OPEN().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SwitchNavButtons C-Win 
PROCEDURE SwitchNavButtons :
/*------------------------------------------------------------------------------
    Purpose:
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCurrentRow AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLastRow    AS INTEGER NO-UNDO.

    iCurrentRow = CURRENT-RESULT-ROW("brCustomer").
    // NUM-RESULTS not yet working as expected.
    iLastRow = NUM-RESULTS("brCustomer").

    IF iCurrentRow = 1 THEN 
        PUBLISH "SetButtons"("DisableFirst").
    ELSE 
        PUBLISH "SetButtons"("EnableFirst").          
    
    IF iCurrentRow = iLastRow THEN 
        PUBLISH "SetButtons"("DisableLast"). 
    ELSE 
        PUBLISH "SetButtons"("EnableLast").                    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetWhereClause C-Win 
FUNCTION GetWhereClause RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        DEFINE VARIABLE cWhereClause AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cName        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iCustNum     AS INTEGER NO-UNDO.
      
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN iCustNum = INTEGER(fiCustNum:SCREEN-VALUE) 
                   cName = fiCustName:SCREEN-VALUE.
        END.
        
        IF iCustNum > 0 THEN 
            ASSIGN cWhereClause = SUBSTITUTE("ttCustomer.CustNum >= &1 ":U,iCustNum).
            
        IF cName > "" THEN 
            ASSIGN cWhereClause = TRIM(SUBSTITUTE("&1 &2 &3",
                                             cWhereClause,
                                             (IF cWhereClause > "" THEN "AND":U ELSE ""),
                                             SUBSTITUTE("ttCustomer.Name BEGINS &1 ":U,QUOTER(cName)))).    
        
        IF cWhereClause > "" THEN 
            ASSIGN cWhereClause = "WHERE " + cWhereClause.
        
        RETURN cWhereClause.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

