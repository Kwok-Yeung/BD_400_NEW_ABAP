REPORT ZBD400_NEW_OPENSQL_TASK1_TMP.

"! main Class for app control
CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    TYPES:
      "! range table for constructor
      tt_range_vbeln TYPE RANGE OF vbap-vbeln,
      "! range table for constructor
      tt_range_matnr TYPE RANGE OF vbap-matnr.

    TYPES:
      "! output structure for data alv
      BEGIN OF ts_output,
        auart TYPE vbak-auart,
        vbeln TYPE vbap-vbeln,
        posnr TYPE vbap-posnr,
        matnr TYPE vbap-matnr,
        maktx TYPE makt-maktx,
        pstyv TYPE vbap-pstyv,
        aedat TYPE vbap-aedat,
        color TYPE char4,
      END OF ts_output,
      tt_output TYPE TABLE OF ts_output WITH EMPTY KEY,

      "! output structure for summary alv
      BEGIN OF ts_output_sum,
        pstyv TYPE vbap-pstyv,
        sum   TYPE i,
      END OF ts_output_sum,
      tt_output_sum TYPE TABLE OF ts_output_sum WITH EMPTY KEY,

      tt_vbak       TYPE SORTED TABLE OF vbak WITH UNIQUE KEY vbeln,
      tt_vbap       TYPE SORTED TABLE OF vbap WITH UNIQUE KEY vbeln posnr,
      tt_makt       TYPE SORTED TABLE OF makt WITH UNIQUE KEY matnr.

    METHODS: constructor IMPORTING it_vbeln TYPE tt_range_vbeln
                                   it_matnr TYPE tt_range_matnr.

  PRIVATE SECTION.

    DATA:
      "! reference to selectoption
      mr_vbeln      TYPE REF TO tt_range_vbeln,
      "! reference to selectoption
      mr_matnr      TYPE REF TO tt_range_matnr,
      "! buffered salesdoc items
      mt_vbap       TYPE tt_vbap,
      "! buffered salesdoc header
      mt_vbak       TYPE tt_vbak,
      "! bufferd material text
      mt_makt       TYPE tt_makt,
      "! output table for data alv
      mt_output     TYPE tt_output,
      "! output table for summary alv
      mt_output_sum TYPE tt_output_sum.


    METHODS:
      "! select sales document data by vbeln range
      _select_sales_data
        IMPORTING
          it_vbeln TYPE lcl_app=>tt_range_vbeln
        EXPORTING
          et_vbap  TYPE tt_vbap
          et_vbak  TYPE tt_vbak,

      "! select and return material text
      _select_material_text
        RETURNING VALUE(rt_makt) TYPE tt_makt,

      "! create alv
      _init_alv CHANGING ct_out     TYPE tt_output
                         ct_out_sum TYPE tt_output_sum,

      "! create and return fieldcatalog
      _get_fcat
        RETURNING
          VALUE(rt_fcat) TYPE lvc_t_fcat,

      "! create and return layout
      _get_layo
        RETURNING
          VALUE(rs_layo) TYPE lvc_s_layo,

      "! build output itab from selected data
      _create_output IMPORTING it_matnr      TYPE tt_range_matnr
                     CHANGING  ct_vbak       TYPE tt_vbak
                               ct_vbap       TYPE tt_vbap
                               ct_makt       TYPE tt_makt
                     RETURNING VALUE(rt_out) TYPE tt_output,

      "! build output itab for summary alv based
      "! on data output itab
      _create_output_sum
        IMPORTING
          it_out               TYPE lcl_app=>tt_output
        RETURNING
          VALUE(rt_output_sum) TYPE tt_output_sum,

      "! create and return fieldcatalog for summary alv
      _get_fcat_sum
        RETURNING
          VALUE(rt_fcat) TYPE lvc_t_fcat.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.

    " get reference of selectionparameter to internal
    mr_vbeln = REF #( it_vbeln ).
    mr_matnr = REF #( it_matnr ).

    " read sales data from database
    _select_sales_data( EXPORTING it_vbeln = mr_vbeln->*
                        IMPORTING et_vbak  = mt_vbak
                                  et_vbap  = mt_vbap ).

    " read materialtext from database
    mt_makt = _select_material_text( ).

    " generate output itab
    mt_output = _create_output( EXPORTING it_matnr = mr_matnr->*
                                CHANGING  ct_vbak  = mt_vbak
                                          ct_vbap  = mt_vbap
                                          ct_makt  = mt_makt    ).

    " generate output itab for summary of itemtyp
    mt_output_sum = _create_output_sum( it_out = mt_output ).

    " create alv and display
    _init_alv( CHANGING ct_out     = mt_output
                        ct_out_sum = mt_output_sum ).

  ENDMETHOD.


  METHOD _select_sales_data.

    SELECT * FROM vbak
        INTO CORRESPONDING FIELDS OF TABLE et_vbak
        WHERE vbeln IN it_vbeln.

    SELECT * FROM vbap
        INTO CORRESPONDING FIELDS OF TABLE et_vbap
        WHERE vbeln IN it_vbeln.

  ENDMETHOD.


  METHOD _init_alv.

    DATA: lo_splitter TYPE REF TO cl_gui_splitter_container,
          lo_alv_sum  TYPE REF TO cl_gui_alv_grid,
          lo_alv      TYPE REF TO cl_gui_alv_grid.
          "ls_fcat     TYPE LINE OF lvc_t_fcat.

* create container instance
**********************************************************************
    lo_splitter = new #( parent                  = cl_gui_container=>screen0 " Parent Container
                         rows                    = 2                         " Anzahl zu zeigender Zeilen
                         columns                 = 1                         " Anzahl zu zeigender Spalten
                         no_autodef_progid_dynnr = 'X' ).                    " dont autodefine progid and dynnr? )

**********************************************************************
* create alv for summary of itemtyps
* and set upper container
**********************************************************************
    lo_alv_sum = new #( i_parent = lo_splitter->get_container( row    = 1
                                                               column = 1 ) ).

* get fieldcatalog for summary ALV
*----------------------------*
    DATA(lt_fcat_sum) = _get_fcat_sum( ).

* set output to summary ALV
*----------------------------*
    lo_alv_sum->set_table_for_first_display( CHANGING it_outtab       = ct_out_sum     " Ausgabetabelle
                                                      it_fieldcatalog = lt_fcat_sum ). " Feldkatalog

**********************************************************************
* create alv for sales data
* and set to lower container
**********************************************************************
    lo_alv = NEW cl_gui_alv_grid( i_parent = lo_splitter->get_container( row    = 2
                                                                         column = 1 ) ).
* get fieldcatalog and layout
*----------------------------*
    DATA(lt_fcat) = _get_fcat( ).

* no display for color field
*----------------------------*
    MODIFY lt_fcat FROM value #( no_out = 'X'
                                 tech   = 'X' ) TRANSPORTING no_out tech WHERE fieldname = 'COLOR'.

* set data to alv and display out
*----------------------------*
    lo_alv->set_table_for_first_display( EXPORTING is_layout       = _get_layo( )     " Layout
                                         CHANGING  it_outtab       = ct_out      " Ausgabetabelle
                                                   it_fieldcatalog = lt_fcat  ). " Feldkatalog

  ENDMETHOD.


  METHOD _get_fcat.

    rt_fcat = VALUE #( ref_table = 'VBAK' ( fieldname = 'VBELN' ref_field = 'VBELN' )
                                          ( fieldname = 'AUART' ref_field = 'AUART' )
                       ref_table = 'VBAP' ( fieldname = 'POSNR' ref_field = 'POSNR' )
                                          ( fieldname = 'PSTYV' ref_field = 'PSTYV' )
                                          ( fieldname = 'MATNR' ref_field = 'MATNR' )
                       ref_table = 'MAKT' ( fieldname = 'MAKTX' ref_field = 'MAKTX' )
                       ref_table = 'VBAP' ( fieldname = 'AEDAT' ref_field = 'AEDAT' )
                       ref_table = ''     ( fieldname = 'COLOR' inttype = 'C' outputlen = 4  ) ).

  ENDMETHOD.


  METHOD _get_layo.

    " set zebra and field with color information
    rs_layo-zebra      = 'X'.
    rs_layo-info_fname = 'COLOR'.

  ENDMETHOD.

  METHOD _select_material_text.

    SELECT * FROM makt
        INTO TABLE rt_makt
        FOR ALL ENTRIES IN mt_vbap
        WHERE matnr = mt_vbap-matnr
        AND   spras = sy-langu.

  ENDMETHOD.


  METHOD _create_output_sum.

    LOOP AT it_out ASSIGNING FIELD-SYMBOL(<out>)
        GROUP BY ( pstyv = <out>-pstyv sum = GROUP SIZE )
        WITHOUT MEMBERS ASSIGNING FIELD-SYMBOL(<group>).

      APPEND INITIAL LINE TO rt_output_sum ASSIGNING FIELD-SYMBOL(<sum>).
      <sum>-pstyv = <group>-pstyv.
      <sum>-sum   = <group>-sum.

    ENDLOOP.

  ENDMETHOD.

  METHOD _get_fcat_sum.

    rt_fcat = VALUE #( ( fieldname = 'PSTYV' ref_table = 'VBAP' ref_field = 'PSTYV' outputlen = 15 )
                       ( fieldname = 'SUM' inttype   = 'I' scrtext_s = 'Anzahl'
                                                           scrtext_m = 'Anzahl'
                                                           scrtext_l = 'Anzahl'
                                                           outputlen = 10 )
                     ).

  ENDMETHOD.


  METHOD _create_output.

* create output
**********************************************************************
    rt_out = VALUE #( FOR <vbak> IN ct_vbak
                         FOR <vbap> IN ct_vbap WHERE ( vbeln = <vbak>-vbeln )
                               "let <makt> = <vbap>-matnr ]-maktx in
                             ( vbeln = <vbap>-vbeln
                               posnr = <vbap>-posnr
                               auart = <vbak>-auart
                               matnr = <vbap>-matnr
                               pstyv = <vbap>-pstyv
                               maktx = ct_makt[ matnr = <vbap>-matnr ]-maktx
                               color = COND #( WHEN it_matnr IS INITIAL THEN space
                                               WHEN <vbap>-matnr IN it_matnr THEN 'C500'  ) ) ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* Selection Screen
**********************************************************************
DATA: gf_matnr TYPE vbap-matnr,
      gf_vbeln TYPE vbap-vbeln,
      go_app   TYPE REF TO lcl_app.


SELECT-OPTIONS: s_vbeln FOR gf_vbeln.
SELECT-OPTIONS: s_matnr FOR gf_matnr.


**********************************************************************
INITIALIZATION.
**********************************************************************
  DEFINE parameter_txt.
    %_&1_%_APP_%-text = &2.
  END-OF-DEFINITION.

  parameter_txt: s_vbeln 'Vertriebsbeleg',
                 s_matnr 'Besondere Materialien'.

**********************************************************************
START-OF-SELECTION.
**********************************************************************
  CREATE OBJECT go_app
    EXPORTING
      it_vbeln = s_vbeln[]
      it_matnr = s_matnr[].

  CALL SCREEN 100.

**********************************************************************
* PAI / PBO
**********************************************************************
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'S100'.
  SET TITLEBAR 'T100'.
ENDMODULE.
