*&---------------------------------------------------------------------*
*& Report zbd400_new_abap_ex_open_sql
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbd400_new_abap_ex_open_sql.

PARAMETERS p_mtart TYPE mara-mtart.
PARAMETERS p_matnr TYPE mara-matnr.

START-OF-SELECTION.

*  "--- Aufgabe "New Open SQL"
*
*  "Aufgabe 1:
*  "Selektieren Sie alle Felder (außer MANDT und SPRAS) der Tabelle MAKT
*  "in eine interne Tabelle für alle Datensätze in der Anmeldesprache.


*  "Aufgabe 2:
*  "Selektieren Sie die Felder MEINS und MTART aus Tabelle MARA
*  "und alle Felder der Tabelle MAKT (ohne sie einzeln aufzulisten)
*  "für alle Datensätze, die die selektierte Materialart haben.


*  "Aufgabe 3:
*  "Selektieren Sie die Felder MATNR, MEINS und MTART aus Tabelle MARA
*  "für alle Datensätze,
*  "die mit dem Material aus dem Selektionsfeld
*  "in der Basismengeneinheit (Feld MEINS) übereinstimmen.
*  "Verwenden Sie dafür nur eine einzige SELECT-Anweisung
*  "in neuer Open SQL-Syntax.


*  "--- Aufgabe "New Open SQL: UNION"

  "Aufgabe 1:
  "Selektieren Sie die Felder ID, NAME, CUSTTYPE und EMAIL der Tabelle SCUSTOM
  "für alle Geschäftskunden (CUSTTYPE = 'B') aus Italien (COUNTRY = 'IT')
  "und
  "die Felder ID, NAME, CUSTTYPE und TELEPHONE der Tabelle SCUSTOM
  "für alle Privatkunden (CUSTTYPE = 'P') mit Anrede 'Dr.' (FORM = 'Dr.')
  "mit einer einzigen SELECT-Anweisung
  "in eine interne Tabelle mit 4 Feldern.
