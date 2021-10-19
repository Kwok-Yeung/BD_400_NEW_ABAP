*&---------------------------------------------------------------------*
*& Report zbd400_new_abap_ex_open_sql_s
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbd400_new_abap_ex_open_sql_s.

PARAMETERS p_mtart TYPE mara-mtart.
PARAMETERS p_matnr TYPE mara-matnr.

START-OF-SELECTION.

*  BREAK-POINT.
*  "--- Aufgabe "New Open SQL"
*
*  "Aufgabe 1:
*  "Selektieren Sie alle Felder (außer MANDT und SPRAS) der Tabelle MAKT
*  "in eine interne Tabelle für alle Datensätze in der Anmeldesprache.
  SELECT matnr, maktx, maktg
  FROM makt
  INTO TABLE @DATA(lt_makt)
  WHERE spras = @sy-langu.

*  "Aufgabe 2:
*  "Selektieren Sie die Felder MEINS und MTART aus Tabelle MARA
*  "und alle Felder der Tabelle MAKT (ohne sie einzeln aufzulisten)
*  "für alle Datensätze, die die selektierte Materialart haben.
  SELECT mara~meins, mara~mtart, makt~*
  FROM mara
  INNER JOIN makt
    ON mara~matnr = makt~matnr
  INTO TABLE @DATA(lt_marat)
  WHERE mtart = @p_mtart.

*  "Aufgabe 3:
*  "Selektieren Sie die Felder MATNR, MEINS und MTART aus Tabelle MARA
*  "für alle Datensätze,
*  "die mit dem Material aus dem Selektionsfeld
*  "in der Basismengeneinheit (Feld MEINS) übereinstimmen.
*  "Verwenden Sie dafür nur eine einzige SELECT-Anweisung
*  "in neuer Open SQL-Syntax.
  SELECT matnr, meins, mtart
  FROM mara
  WHERE meins = ( SELECT meins FROM mara WHERE matnr = @p_matnr )
  INTO TABLE @DATA(lt_makt2).

*  "--- Aufgabe "New Open SQL: UNION"

  "Aufgabe 1:
  "Selektieren Sie die Felder ID, NAME, CUSTTYPE und EMAIL der Tabelle SCUSTOM
  "für alle Geschäftskunden (CUSTTYPE = 'B') aus Italien (COUNTRY = 'IT')
  "und
  "die Felder ID, NAME, CUSTTYPE und TELEPHONE der Tabelle SCUSTOM
  "für alle Privatkunden (CUSTTYPE = 'P') mit Anrede 'Dr.' (FORM = 'Dr.')
  "mit einer einzigen SELECT-Anweisung
  "in eine interne Tabelle mit 4 Feldern.
  TYPES: BEGIN OF ts_cust,
           id       TYPE scustom-id,
           name     TYPE scustom-name,
           custtype TYPE scustom-custtype,
           contact  TYPE scustom-email,
         END OF ts_cust.
  DATA lt_cust TYPE STANDARD TABLE OF ts_cust.

  SELECT id, name, custtype, email AS contact FROM scustom
  WHERE custtype = 'B'
  AND   country  = 'IT'
  UNION
  SELECT id, name, custtype, telephone AS contact FROM scustom
  WHERE custtype = 'P'
  AND   form     = 'Dr.'
  INTO TABLE @lt_cust.

  cl_demo_output=>display( lt_cust ).
