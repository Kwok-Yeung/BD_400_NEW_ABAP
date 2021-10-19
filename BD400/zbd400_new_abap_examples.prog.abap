*&---------------------------------------------------------------------*
*& Report zbd400_new_abap_examples
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbd400_new_abap_examples.

PARAMETERS p_itsele RADIOBUTTON GROUP tas. "Intene Tabelle SELECT
PARAMETERS p_grloag RADIOBUTTON GROUP tas. "Gruppenbildung LOOP AT GROOP
PARAMETERS p_covalu RADIOBUTTON GROUP tas. "Konstruktorausdruck VALUE
PARAMETERS p_cocorr RADIOBUTTON GROUP tas. "Konstruktorausdruck CORRESPONDING
PARAMETERS p_cofilt RADIOBUTTON GROUP tas. "Konstruktorausdruck FILTER
PARAMETERS p_coalph RADIOBUTTON GROUP tas. "Konstruktorausdruck ALPHA
PARAMETERS p_cerais RADIOBUTTON GROUP tas. "Klassenbasierte Ausnahmen
PARAMETERS p_dmdynm RADIOBUTTON GROUP tas. "Dynamisches Mapping
"! 'X' = Ship to fields to be used.
PARAMETERS p_ag AS CHECKBOX. "Auftraggeber
PARAMETERS p_adadoc RADIOBUTTON GROUP tas. "ABAP Doc
PARAMETERS p_oscase RADIOBUTTON GROUP tas. "New Open SQL: CASE-Anweisung
PARAMETERS p_osster RADIOBUTTON GROUP tas. "New Open SQL: tabelle~*
PARAMETERS p_osarit RADIOBUTTON GROUP tas. "New Open SQL: Arithmetische Berechnung
PARAMETERS p_oslpad RADIOBUTTON GROUP tas. "New Open SQL: LPAD, RPAD
PARAMETERS p_oscoal RADIOBUTTON GROUP tas. "New Open SQL: COALESCE
PARAMETERS p_ostemp RADIOBUTTON GROUP tas. "New Open SQL: Temporäre Tabelle
PARAMETERS p_cdscds RADIOBUTTON GROUP tas. "CDS-View: SELECT
PARAMETERS p_cdcurr RADIOBUTTON GROUP tas. "CDS-View: CURRENCY_CONVERSION
PARAMETERS p_waers TYPE vbap-waerk DEFAULT 'EUR'.
PARAMETERS p_ntwmin TYPE vbap-netwr DEFAULT 5.

*P_ADADOC    ABAP Doc
*P_AG    Aufraggeber
*P_CDCURR    CDS:CURRENCY_CONVERSION
*P_CDSCDS    CDS:SELECT from cds-view
*P_CERAIS    Klassenbasierte Ausnahme
*P_COALPH    Konstr.:ALPHA
*P_COCORR    Konstr.:CORRESPONDING
*P_COFILT    Konstr.:FILTER
*P_COVALU    Konstr.:VALUE
*P_DMDYNM    Dynamisches Mapping
*P_GRLOAG    Gruppen:LOOP ATGROUP
*P_ITSELE    Interne Tab.:SELECT FROM itab
*P_NTWMIN    Mindest-Nettowert
*P_OSARIT    Open SQL:Arithmetik
*P_OSCASE    Open SQL:CASE
*P_OSCOAL    Open SQL:COALESCE
*P_OSLPAD    Open SQL:LPAD, RPAD
*P_OSSTER    Open SQL:db~*
*P_OSTEMP    Open SQL:Globale Temp. Tabelle
*P_WAERS Währung

CLASS lcl_main DEFINITION DEFERRED.

DATA go_main TYPE REF TO lcl_main.

CLASS lcl_main DEFINITION FINAL.

  PUBLIC SECTION.

    "! <h1>Ganz wichtiger Wert!</h1>
    "! <h2>Ich weiß nicht mehr wofür.</h2>
    "! Vielleicht war es doch nicht so wichtig.
    CONSTANTS c_importante TYPE string VALUE 'WISCHTISCH!'.

    METHODS ev_start_of_selection.


  PRIVATE SECTION.

    METHODS itab_select_from_itab.
    METHODS group_loop_at_group.
    METHODS constr_corresponding.
    METHODS constr_filter.
    METHODS constr_value.
    METHODS constr_alpha.
    METHODS clbasex_raise_with_message.
    METHODS dynmap_dynamic_mapping.
    "!In this method several objects with ABAP doc texts will be analysed.
    METHODS abap_doc.
    METHODS sql_arithmetic.
    METHODS sql_case.
    METHODS sql_coalesce.
    METHODS sql_temp_table.
    METHODS sql_lpad.
    METHODS sql_stern.
    METHODS cds_select.
    METHODS cds_currency_conversion.

    METHODS _help_mult10
      IMPORTING
                !if_i       TYPE i
      RETURNING VALUE(rf_i) TYPE i.
    "! <p>Multiply given number with it self.</p>
    "! <p>Examples</p>
    "! <ul>
    "!   <li>1 -> 1</li>
    "!   <li>3 -> 9</li>
    "!   <li>7 -> 49</li>
    "!   <li>10 -> 100</li>
    "! </ul>
    METHODS _help_pow2
      IMPORTING
                !if_i       TYPE i
      RETURNING VALUE(rf_i) TYPE i.
    "! Get source file name for given target file name.
    "! @parameter if_target_fnam | Field name in target structure
    "! @parameter rf_source_fnam | Field name in source structure
    METHODS _help_get_source_fnam
      IMPORTING
                !if_target_fnam       TYPE string
      RETURNING VALUE(rf_source_fnam) TYPE string.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD ev_start_of_selection.
    CASE abap_true.
      WHEN p_itsele.
        itab_select_from_itab( ).
      WHEN p_grloag.
        group_loop_at_group( ).
      WHEN p_cocorr.
        constr_corresponding( ).
      WHEN p_cofilt.
        constr_filter( ).
      WHEN p_covalu.
        constr_value( ).
      WHEN p_coalph.
        constr_alpha( ).
      WHEN p_cerais.
        clbasex_raise_with_message( ).
      WHEN p_dmdynm.
        dynmap_dynamic_mapping( ).
      WHEN p_adadoc.
        abap_doc( ).
      WHEN p_osarit.
        sql_arithmetic( ).
      WHEN p_osster.
        sql_stern( ).
      WHEN p_oscase.
        sql_case( ).
      WHEN p_oscoal.
        sql_coalesce( ).
      WHEN p_ostemp.
        sql_temp_table( ).
      WHEN p_oslpad.
        sql_lpad( ).
      WHEN p_cdscds.
        cds_select( ).
      WHEN p_cdcurr.
        cds_currency_conversion( ).
    ENDCASE.
  ENDMETHOD.

  METHOD constr_corresponding.
    DATA: BEGIN OF ls_1,
            kunnr TYPE kna1-kunnr, "mapping
            name1 TYPE kna1-name1, "move-corresponding
            stras TYPE kna1-stras, "move-corresponding
            pstlz TYPE kna1-pstlz, "mapping
            erdat TYPE kna1-erdat, "exclude
            erzet TYPE sy-uzeit,   "exclude
          END OF ls_1.

    DATA: BEGIN OF ls_2,
            stras    TYPE kna1-stras,
            plz      TYPE kna1-pstlz,
            customer TYPE kna1-kunnr,
            erdat    TYPE c LENGTH 8,
            erzet    TYPE c LENGTH 6,
            name1    TYPE kna1-name1,
          END OF ls_2.
    DATA ls_2b LIKE ls_2.

    ls_1-kunnr    = '0815'.
    ls_1-erdat    = sy-datum.
    ls_1-erzet    = sy-uzeit.
    ls_1-name1    = 'Vogel'.
    ls_1-pstlz    = '12345'.
    ls_1-stras    = 'Königsbreede'.

    ls_2b-customer = 'NTT'.
    ls_2b-erdat    = 'Heute'.
    ls_2b-erzet    = 'Jetzt'.
    ls_2b-name1    = 'Herbert'.
    ls_2b-plz      = '47110'.
    ls_2b-stras    = 'Hauptstrasse'.

    BREAK-POINT.

    ls_2 = ls_2b.

    ls_2 = CORRESPONDING #( ls_1 ).
    WRITE / ls_2. "CUSTOMER will be cleared.

    ls_2 = CORRESPONDING #( BASE ( ls_2b )
                            ls_1 ).
    WRITE / ls_2. "CUSTOMER = LS_2B-CUSTOMER

    ls_2 = CORRESPONDING #( BASE ( ls_2b )
                            ls_1
                            MAPPING customer = kunnr
                                    plz      = pstlz
                            EXCEPT  erdat
                                    erzet ).
    WRITE / ls_2. "CUSTOMER = LS_1-KUNNR
  ENDMETHOD.

  METHOD constr_value.
    TYPES: BEGIN OF ts_int,
             name    TYPE c LENGTH 10,
             alter   TYPE i,
             groesse TYPE i,
           END OF ts_int.
    DATA ls_msg TYPE msg_return.
    DATA lt_int TYPE STANDARD TABLE OF ts_int.
    DATA lt_r_matnr TYPE RANGE OF mara-matnr.

    BREAK-POINT.

    "Struktur
    ls_msg = VALUE #( msgid = '00' msgno = '208' msgty = 'I' ).

    ls_msg = VALUE #( BASE ls_msg
                      msgty = 'E' ).

    ls_msg = VALUE #( msgty = 'E' ).

    "Intere Tabelle
    lt_r_matnr = VALUE #(
      ( sign = 'I' option = 'EQ' low = '4711' )
      ( sign = 'I' option = 'EQ' low = '4712' )
    ).

    lt_r_matnr = VALUE #( sign = 'E' option = 'BT'
      ( low = '4711' high = '4799')
      ( low = '4911' high = '4999')
    ).

    "BASE als Startwert (auch für Strukturen)
    lt_r_matnr = VALUE #( BASE lt_r_matnr
      ( sign = 'I' option = 'EQ' low = '4711' )
      ( sign = 'I' option = 'EQ' low = '4712' )
    ).

    "FOR als Schleife mit Zähler
    "Hilfsvariable mit Startwert 1.
    "Hilfsvariable wird immer um 1 erhöht.
    "Wenn Hilfsvariable > 9, dann ist Schluss.
    lt_int = VALUE #( FOR i = 3 THEN i + 1 UNTIL i > 9
                          ( alter = i
                            name  = 'Franz' )
                    ).

    "FOR als Schleife über eine interne Tabelle
    lt_int = VALUE #( FOR <wa> IN lt_r_matnr
                        ( name  = <wa>-low
                          alter = 17 )
                      ).

    "LET ermöglich die Berechnung dynamischer Werte.
    "Hier auch ohne LET möglich.
    "Hinter THEN und ELSE in den bedingten Ausdrücken COND und SWITCH ist LET wichtiger.
    lt_int = VALUE #( FOR j = 1 THEN j + 1 UNTIL j > 10
                      LET power  = _help_pow2( j )
                          mult10 = j * 10 IN
                        ( name    = j
                          alter   = power
                          groesse = mult10 ) ).

    cl_demo_output=>display( lt_int ).
  ENDMETHOD.

  METHOD _help_mult10.
    rf_i = if_i * 10.
  ENDMETHOD.

  METHOD _help_pow2.
    " 1 -> 1
    " 2 -> 4
    " 3 -> 9
    " 4 -> 16
    rf_i = ipow( base = if_i exp = 2 ).
  ENDMETHOD.

  METHOD group_loop_at_group.
    DATA lt_makt TYPE STANDARD TABLE OF makt.

    lt_makt = VALUE #(
      ( spras = 'D' matnr = 'M1' maktx = 'Nagel 1-2')
      ( spras = 'I' matnr = 'M1' maktx = 'Chiodo 1-2')
      ( spras = 'F' matnr = 'M1' maktx = 'Clou 1-2')
      ( spras = 'D' matnr = 'M2' maktx = 'Vater 3')
      ( spras = 'I' matnr = 'M2' maktx = 'Padre 3')
      ( spras = 'F' matnr = 'M2' maktx = 'Pere 3')
    ).

    BREAK-POINT.

    LOOP AT lt_makt REFERENCE INTO DATA(lr_makt1)
        GROUP BY ( key1 = lr_makt1->spras   "key2 = ...
                   inde1 = GROUP INDEX
                   size1 = GROUP SIZE )
        DESCENDING
        REFERENCE INTO DATA(lr_group).

      "WRITE / lr_makt1->spras. => Short dump
      WRITE: / lr_group->key1, lr_group->inde1, lr_group->size1.

      LOOP AT GROUP lr_group REFERENCE INTO DATA(lr_makt2).
        WRITE: /5 lr_makt2->matnr, lr_makt2->maktx.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD constr_filter.
    TYPES: BEGIN OF ts_source,
             kunnr TYPE kna1-kunnr,
             land1 TYPE kna1-land1,
             xcpdk TYPE kna1-xcpdk,
           END OF ts_source.
    TYPES: BEGIN OF ts_filter,
             land TYPE kna1-land1,
             cpd  TYPE kna1-xcpdk,
           END OF ts_filter.
    DATA lt_source_s TYPE SORTED TABLE OF ts_source WITH UNIQUE KEY land1 xcpdk kunnr.
    DATA lt_filter_s TYPE SORTED TABLE OF ts_filter WITH UNIQUE KEY cpd land.
    DATA lt_target TYPE STANDARD TABLE OF ts_source.

    lt_source_s = VALUE #(
      ( kunnr = 'K4' land1 = 'DE' xcpdk = ' ' )
      ( kunnr = 'K1' land1 = 'DE' xcpdk = 'X' )
      ( kunnr = 'K5' land1 = 'DE' xcpdk = 'X' )
      ( kunnr = 'K2' land1 = 'FR' xcpdk = 'X' )
      ( kunnr = 'K6' land1 = 'IT' xcpdk = ' ' )
      ( kunnr = 'K3' land1 = 'IT' xcpdk = 'X' )
    ).

    lt_filter_s = VALUE #(
      ( cpd = 'X' land = 'DE' )
      ( cpd = ' ' land = 'IT' )
    ).

    BREAK-POINT.

    lt_target = FILTER #( lt_source_s
                          WHERE land1 = 'DE ' ).

    lt_target = FILTER #( lt_source_s IN lt_filter_s
                          WHERE land1 = land
                          AND   xcpdk = cpd ).

  ENDMETHOD.

  METHOD dynmap_dynamic_mapping.
    TYPES: BEGIN OF ts_struct1,
             vorna    TYPE vorna,
             nachn    TYPE nachn,
             vorna_ag TYPE vorna,
             nachn_ag TYPE nachn,
             augen    TYPE text20,
             erdat    TYPE erdat,
             ernam    TYPE ernam,
           END OF ts_struct1.

    TYPES: BEGIN OF ts_struct2,
             nachname   TYPE nachn,
             vorname    TYPE vorna,
             augenfarbe TYPE text20,
             erdat      TYPE erdat,
             ernam      TYPE ernam,
           END OF ts_struct2.

    DATA: ls_struct1 TYPE ts_struct1,
          ls_struct2 TYPE ts_struct2.

    "--- Mapping erzeugen (ausführlich, dynamisch)
    DATA ls_mapping TYPE cl_abap_corresponding=>mapping_info.
    DATA lt_mapping TYPE cl_abap_corresponding=>mapping_table.

    BREAK-POINT.

    "LT_MAPPING füllen
    IF p_ag = abap_true.
      ls_mapping = VALUE #( level   = 0
                            kind    = cl_abap_corresponding=>mapping_component
                            srcname = 'VORNA_AG'
                            dstname = 'VORNAME' ).
      APPEND ls_mapping TO lt_mapping.
      ls_mapping = VALUE #( level   = 0
                            kind    = cl_abap_corresponding=>mapping_component
                            srcname = 'NACHN_AG'
                            dstname = 'NACHNAME' ).
      APPEND ls_mapping TO lt_mapping.
    ELSE.
      ls_mapping = VALUE #( level   = 0
                            kind    = cl_abap_corresponding=>mapping_component
                            srcname = 'VORNA'
                            dstname = 'VORNAME' ).
      APPEND ls_mapping TO lt_mapping.
      ls_mapping = VALUE #( level   = 0
                            kind    = cl_abap_corresponding=>mapping_component
                            srcname = 'NACHN'
                            dstname = 'NACHNAME' ).
      APPEND ls_mapping TO lt_mapping.
    ENDIF.

    ls_mapping = VALUE #( level   = 0
                          kind    = cl_abap_corresponding=>mapping_except_component
                          srcname = 'ERDAT' ).
    APPEND ls_mapping TO lt_mapping.

    DATA(lo_map) = cl_abap_corresponding=>create(
                     source      = ls_struct1
                     destination = ls_struct2
                     mapping     = lt_mapping ).


    "--- Mapping erzeugen (Kurzform, statisches Beispiel)

    DATA(lo_map2) = cl_abap_corresponding=>create(
                      source = ls_struct1
                      destination = ls_struct2
                      mapping = VALUE #( ( level   = 0
                                           kind    = cl_abap_corresponding=>mapping_component
                                           srcname = 'NACHN'
                                           dstname = 'NACHNAME' )
                                         ( level   = 0
                                           kind    = cl_abap_corresponding=>mapping_except_component
                                           srcname = 'ERDAT' ) )
                    ).

    "--- Mapping erzeugen (Kurzform, dynamisches Beispiel)

    DATA(lo_map3) = cl_abap_corresponding=>create(
                      source = ls_struct1
                      destination = ls_struct2
                      mapping = VALUE #( ( level   = 0
                                           kind    = cl_abap_corresponding=>mapping_component
                                           srcname = _help_get_source_fnam( 'NACHNAME' )
                                           dstname = 'NACHNAME' )
                                         ( level   = 0
                                           kind    = cl_abap_corresponding=>mapping_component
                                           srcname = _help_get_source_fnam( 'VORNAME' )
                                           dstname = 'VORNAME' )
                                         ( level   = 0
                                           kind    = cl_abap_corresponding=>mapping_except_component
                                           srcname = 'ERDAT' ) )
                    ).


    "--- Testdaten erzegen und Mapping anwenden

    DATA(lo_out) = cl_demo_output=>new( ). "Daten am Bildschirm ausgeben

    lo_out->write( 'P_AG:' && p_ag ). "Daten am Bildschirm ausgeben

    "Testdaten füllen
    ls_struct1-vorna    = 'Herbert'.
    ls_struct1-nachn    = 'Vogel'.
    ls_struct1-vorna_ag = 'Donald'.
    ls_struct1-nachn_ag = 'Duck'.
    ls_struct1-augen    = 'grau'.
    ls_struct1-erdat    = sy-datum.
    ls_struct1-ernam    = sy-uname.

    lo_out->write( ls_struct1 ). "Daten am Bildschirm ausgeben

    "Mapping anwenden
    lo_map->execute( EXPORTING source      = ls_struct1
                     CHANGING  destination = ls_struct2 ).
    lo_out->write( ls_struct2 ). "Daten am Bildschirm ausgeben


    "Testdaten ändern
    ls_struct1-nachn    = 'Grönemeier'.
    ls_struct1-nachn_ag = 'Müller'.
    lo_out->write( ls_struct1 ). "Daten am Bildschirm ausgeben

    "Mapping anwenden
    lo_map->execute( EXPORTING source      = ls_struct1
                     CHANGING  destination = ls_struct2 ).
    lo_out->write( ls_struct2 ). "Daten am Bildschirm ausgeben

    lo_out->display( ). "Daten am Bildschirm ausgeben
  ENDMETHOD.

  METHOD _help_get_source_fnam.
    CASE if_target_fnam.
      WHEN 'NACHNAME'.
        IF p_ag = abap_true.
          rf_source_fnam = 'NACHN_AG'.
        ELSE.
          rf_source_fnam = 'NACHN'.
        ENDIF.
      WHEN 'VORNAME'.
        IF p_ag = abap_true.
          rf_source_fnam = 'VORNA_AG'.
        ELSE.
          rf_source_fnam = 'VORNA'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD abap_doc.
    "! Customer <strong>number</strong> in<br> <em>internal</em> format.
    DATA lf_kunnr_int TYPE kunnr.

    BREAK-POINT.
    "Lassen Sie sich die Dokumentation der folgenden Objekte anzeigen:
    " ^ Methode ABAP_DOC
    " v Variable P_AG  (Dokumentation nicht anzeigbar.)
    " v Kostante C_IMPORTANTE
    " v Variable LF_KUNNR_INT           "fett oder kursiv
    " v Methode _HELP_POW2              "mit unsortierter Liste
    " v Methode _HELP_GET_SOURCE_FNAM   "mit Parametern

    WRITE: p_ag, c_importante.

    lf_kunnr_int = '0000004711'.

    DATA(lf_i) = _help_pow2( if_i = 4 ).

    DATA(lf_fnam) = _help_get_source_fnam( if_target_fnam = 'KUNNR' ).
  ENDMETHOD.

  METHOD sql_arithmetic.
    BREAK-POINT.

    SELECT vbeln, posnr, zmeng - kwmeng AS rest
    FROM vbap INTO TABLE @DATA(lt_vbap).

  ENDMETHOD.

  METHOD sql_case.
    BREAK-POINT.

    DATA(else) = '!!!'.

    SELECT matnr, meins, mtart,
           CASE mtart
             WHEN 'HAWA' THEN ( meins && '-' && matkl )
             WHEN 'FERT' THEN ( matkl && meins )
             ELSE @else
           END AS text
           FROM mara
           INTO TABLE @DATA(lt_mara).

    IF sy-subrc = 0.
      cl_demo_output=>display( lt_mara ).
    ENDIF.
  ENDMETHOD.

  METHOD sql_coalesce.
    TYPES: BEGIN OF ts_mara,
             matnr  TYPE mara-matnr,
             mtart  TYPE mara-mtart,
             meins  TYPE mara-meins,
             mtartx TYPE c LENGTH 50,
           END OF ts_mara.
    DATA lt_mara TYPE STANDARD TABLE OF ts_mara.

    BREAK-POINT.

    "Versuch 1: ohne COALESCE
    SELECT FROM mara
    FIELDS matnr,
           mtart,
           meins,
           concat( meins,
                   CASE mtart
                      WHEN 'CSU1' THEN 'C1'
                      WHEN 'CSU2' THEN 'C2'
                      END ) AS mtartx
    INTO TABLE @lt_mara.
    "=> Feld MARTX ist leer, obwohl MEINS gefüllt ist.

    "Versuch 1: mit COALESCE
    SELECT FROM mara
    FIELDS matnr,
           mtart,
           meins,
           concat( meins,
                   coalesce(
                     CASE mtart WHEN 'CSU1' THEN 'C1'
                                WHEN 'CSU2' THEN 'C2' END,
                     CASE meins WHEN 'ST'   THEN 'x2' END )
                 ) AS mtartx
    INTO TABLE @lt_mara.
    "=> Feld MARTX ist gefüllt.

    "Versuch 3: COALESCE ohne CONCAT
    SELECT FROM mara
    FIELDS matnr, mtart, meins,
           coalesce( CASE mtart WHEN 'CSU1' THEN 'C1'
                                WHEN 'CSU2' THEN 'C2' END,
                     CASE meins WHEN 'KG'   THEN 'Kilo'
                                ELSE 'nix' END
                   ) AS mtartx
    INTO TABLE @lt_mara.
  ENDMETHOD.

  METHOD constr_alpha.
    DATA lf_kunnr_ext TYPE kunnr.
    DATA lf_kunnr_int TYPE kunnr.
    DATA lf_char15    TYPE c LENGTH 15.

    lf_kunnr_ext = '199000'.

    BREAK-POINT.

    "--- ALPHA-Konvertierung ins das interne Format.

    "Variable füllen
    lf_kunnr_int = |{ lf_kunnr_ext ALPHA = IN }|.
    lf_kunnr_int = |{ lf_kunnr_ext ALPHA = IN WIDTH = 8 }|.

    "Verwendung in SELECT-Anweisung
    SELECT SINGLE name1 FROM kna1
    WHERE kunnr = @lf_kunnr_ext
    INTO @DATA(lf_name1).
    "=> Datensatz NICHT gefunden

    SELECT SINGLE name1 FROM kna1
    WHERE kunnr = @( |{ lf_kunnr_ext ALPHA = IN WIDTH = 10 }| )
    INTO @lf_name1.
    "=> Datensatz gefunden, wenn Kundennummer '0000199000' existiert.

    CLEAR lf_name1.

    "Verwendung in SELECT-Anweisung
    lf_char15 = '000000000199000'.

    SELECT SINGLE name1 FROM kna1
    WHERE kunnr = @( |{ lf_char15 ALPHA = IN WIDTH = 10 }| )
    INTO @lf_name1.
    "=> Datensatz gefunden, wenn Kundennummer '0000199000' existiert.


    "--- ALPHA-Konvertierung ins das externe Format.

    CLEAR lf_name1.

    "Variable füllen
    lf_kunnr_int = '0000012345'.
    lf_kunnr_ext = |{ lf_kunnr_int ALPHA = OUT }|.

    "Verwendung in SELECT-Anweisung
    lf_char15 = '000000000199000'.

    SELECT SINGLE name1 FROM kna1
    WHERE kunnr = @( |{ lf_char15 ALPHA = IN WIDTH = 10 }| )
    INTO @lf_name1.
    "=> Datensatz gefunden, wenn Kundennummer '0000199000' existiert.

    CLEAR lf_name1.

    SELECT SINGLE name1 FROM kna1
    WHERE kunnr = @lf_char15
    INTO @lf_name1.
    "=> Kurzdump: SAPSQL_DATA_LOSS
  ENDMETHOD.

  METHOD clbasex_raise_with_message.
    BREAK-POINT.

    TRY.
        "RAISE EXCEPTION mit MESSAGE (Langform)
        RAISE EXCEPTION TYPE cx_fi_corr_actvt_auth
          MESSAGE ID '00' TYPE 'I' NUMBER '208' WITH 'Bu'.
      CATCH cx_fi_corr_actvt_auth INTO DATA(lo_x).
        DATA(lf_string) = lo_x->get_text( ).
    ENDTRY.

    TRY.
        "RAISE EXCEPTION mit MESSAGE (Kurzform)
        RAISE EXCEPTION TYPE cx_fi_corr_actvt_auth
          MESSAGE i208(00) WITH 'Huch'.
      CATCH cx_fi_corr_actvt_auth INTO lo_x.
        lf_string = lo_x->get_text( ).
    ENDTRY.

    TRY.
        "RAISE EXCEPTION mit USING MESSAGE
        MESSAGE i022(me) WITH '4711' INTO lf_string.

        RAISE EXCEPTION TYPE cx_fi_corr_actvt_auth
          USING MESSAGE.
      CATCH cx_fi_corr_actvt_auth INTO lo_x.
        CLEAR lf_string.
        lf_string = lo_x->get_text( ).

        "MESSAGE ausnahme.
        MESSAGE lo_x.

        "RAISE EXCEPTION ausnahme.
*        RAISE EXCEPTION lo_x.
    ENDTRY.
  ENDMETHOD.

  METHOD itab_select_from_itab.
    TYPES: BEGIN OF ts_kna1,
             kunnr TYPE kna1-kunnr,
             name1 TYPE kna1-name1,
             land1 TYPE kna1-land1,
           END OF ts_kna1.
    TYPES: BEGIN OF ts_knb1,
             kunnr TYPE knb1-kunnr,
             bukrs TYPE knb1-bukrs,
             zterm TYPE knb1-zterm,
           END OF ts_knb1.
    DATA lt_kna1 TYPE SORTED TABLE OF ts_kna1 WITH UNIQUE KEY kunnr.
    DATA lt_knb1 TYPE SORTED TABLE OF ts_knb1 WITH UNIQUE KEY kunnr bukrs.

    "Testdaten
    lt_kna1 = VALUE #(
      ( kunnr = 'K1' name1 = 'Donald'   land1 = 'EL' )
      ( kunnr = 'K2' name1 = 'Dagobert' land1 = 'EL' )
      ( kunnr = 'K3' name1 = 'Darth'    land1 = 'SW' )
    ).
    lt_knb1 = VALUE #(
      ( kunnr = 'K1' bukrs = '1010' zterm = '0001' )
      ( kunnr = 'K1' bukrs = 'B2'   zterm = '0001' )
      ( kunnr = 'K2' bukrs = '1010' zterm = '0001' )
      ( kunnr = 'K2' bukrs = 'B3'   zterm = 'Z1' )
      ( kunnr = 'K3' bukrs = 'B1'   zterm = 'A1' )
    ).

    BREAK-POINT.

    SELECT a~kunnr, a~name1, a~land1 FROM @lt_kna1 AS a
    WHERE a~land1 = 'EL'
    INTO TABLE @DATA(lt_1).

*    cl_demo_output=>display( lt_1 ).

*    "Pro SQL Befehl kann nur 1 interne Tabelle als Quelle dienen.
*    SELECT a~kunnr, b~bukrs, a~name1, a~land1, b~zterm
*    FROM @lt_kna1 AS a
*    INNER JOIN @lt_knb1 AS b
*      ON a~kunnr = b~kunnr
*    WHERE a~land1 = 'EL'
*    INTO TABLE @DATA(lt_1).

    SELECT b~kunnr, b~bukrs, b~zterm, t001~waers
    FROM @lt_knb1 AS b
    LEFT OUTER JOIN t001
      ON b~bukrs = t001~bukrs
    WHERE b~zterm = '0001'
    INTO TABLE @DATA(lt_2).

    cl_demo_output=>display( lt_2 ).

  ENDMETHOD.

  METHOD cds_select.
    BREAK-POINT.

    "CDS-View lesen, nur oberste Ebene.
    SELECT material, materialtype, materialbaseunit
    FROM c_materialvaluehelp
    INTO TABLE @DATA(lt_1).

    "CDS-View lesen, inkl. Assoziation _text
    SELECT material, materialtype, materialbaseunit, \_text-ProductName
    FROM c_materialvaluehelp
    WHERE \_text-language = @sy-langu
    INTO TABLE @DATA(lt_2).

    cl_demo_output=>display( lt_2 ).
  ENDMETHOD.

  METHOD cds_currency_conversion.
    BREAK-POINT.

    SELECT * FROM zbd400_sales_order_item( p_unit = @p_waers, p_date = @sy-datum )
    INTO TABLE @DATA(lt_1)
    WHERE netwr2 >= @p_ntwmin
    ORDER BY vbeln, posnr.

    WRITE: / 'Belege mit einem Nettowert von mindestens', p_ntwmin, p_waers NO-GAP, ':'.
    WRITE: 'VBELN', 'POSNR', 'MATNR', 'NETWR', 'WAERK', 'NETWR2', 'WAERK2'.

    LOOP AT lt_1 REFERENCE INTO DATA(lr_1).
      WRITE: / lr_1->vbeln,
               lr_1->posnr,
               lr_1->matnr,
               lr_1->netwr CURRENCY lr_1->waerk,
               lr_1->waerk,
               lr_1->netwr2 CURRENCY lr_1->waerk2,
               lr_1->waerk2.
    ENDLOOP.
  ENDMETHOD.

  METHOD sql_stern.
    BREAK-POINT.

    SELECT mara~matnr, mara~mtart, mara~meins, makt~*
    FROM mara
    INNER JOIN makt
      ON mara~matnr = makt~matnr
    WHERE makt~spras = @sy-langu
    INTO TABLE @DATA(lt_mat).
  ENDMETHOD.

  METHOD sql_lpad.
    BREAK-POINT.

    SELECT kunnr,
           lpad( kunnr, 13, 'c' ) AS kunnr_lpad,
           rpad( kunnr, 13, '.' ) AS kunnr_rpad
    FROM kna1 INTO TABLE @DATA(lt_kna1).
  ENDMETHOD.

  METHOD sql_temp_table.
    DATA(lf_sap_luw_key) = cl_system_transaction_state=>get_sap_luw_key( ).
    DATA ls_temp TYPE zbd400_temp.

    BREAK-POINT.

    SELECT * FROM zbd400_temp INTO TABLE @DATA(lt_temp).

    ls_temp = VALUE #( mandt = sy-mandt kunnr = 'K1' name1 = 'Ernie' land1 = 'DE' ).

    INSERT zbd400_temp FROM @ls_temp.

    SELECT * FROM zbd400_temp INTO TABLE @lt_temp.

    BREAK-POINT.

    COMMIT WORK AND WAIT.

*    "Verbuchung abwarten
*    DO.
*      SELECT COUNT( * ) FROM vbmod UP TO 1 ROWS
*      WHERE vbkey = lf_sap_luw_key.
*      IF sy-subrc = 0.
*        WAIT UP TO 1 SECONDS.
*      ELSE.
*        EXIT.
*      ENDIF.
*    ENDDO.

    SELECT * FROM zbd400_temp INTO TABLE @lt_temp.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  go_main = NEW #( ).

START-OF-SELECTION.
  go_main->ev_start_of_selection( ).
