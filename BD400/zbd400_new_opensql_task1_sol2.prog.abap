REPORT zbd400_new_opensql_task1_sol2.

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
      tt_output_sum TYPE TABLE OF ts_output_sum WITH EMPTY KEY.
*
*      tt_vbak       TYPE SORTED TABLE OF vbak WITH UNIQUE KEY vbeln,
*      tt_vbap       TYPE SORTED TABLE OF vbap WITH UNIQUE KEY vbeln posnr,
*      tt_makt       TYPE SORTED TABLE OF makt WITH UNIQUE KEY matnr.

    METHODS: constructor IMPORTING it_vbeln TYPE tt_range_vbeln
                                   it_matnr TYPE tt_range_matnr.

  PRIVATE SECTION.

    DATA:
      "! reference to selectoption
      mr_vbeln      TYPE REF TO tt_range_vbeln,
      "! reference to selectoption
      mr_matnr      TYPE REF TO tt_range_matnr,
      "! output table for data alv
      mt_output     TYPE tt_output,
      "! output table for summary alv
      mt_output_sum TYPE tt_output_sum.


    METHODS:
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

      "! build output itab for summary alv based
      "! on data output itab
      _create_output_sum
        IMPORTING
          it_out               TYPE lcl_app=>tt_output
        RETURNING
          VALUE(rt_output_sum) TYPE tt_output_sum,

      "! create output itab by select
      _create_output_db IMPORTING it_vbeln         TYPE tt_range_vbeln
                                  it_matnr         TYPE tt_range_matnr
                        RETURNING VALUE(rt_output) TYPE tt_output,

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

    mt_output = _create_output_db( EXPORTING it_vbeln = mr_vbeln->*
                                             it_matnr = mr_matnr->* ).

    " generate output itab for summary of itemtyp
    mt_output_sum = _create_output_sum( it_out = mt_output ).

    " create alv and display
    _init_alv( CHANGING ct_out     = mt_output
                        ct_out_sum = mt_output_sum ).

  ENDMETHOD.

  METHOD _init_alv.

    DATA: lo_splitter TYPE REF TO cl_gui_splitter_container,
          lo_alv_sum  TYPE REF TO cl_gui_alv_grid,
          lo_alv      TYPE REF TO cl_gui_alv_grid.
    "ls_fcat     TYPE LINE OF lvc_t_fcat.

* create container instance
**********************************************************************
    lo_splitter = NEW #( parent                  = cl_gui_container=>screen0 " Parent Container
                         rows                    = 2                         " Anzahl zu zeigender Zeilen
                         columns                 = 1                         " Anzahl zu zeigender Spalten
                         no_autodef_progid_dynnr = 'X' ).                    " dont autodefine progid and dynnr? )

**********************************************************************
* create alv for summary of itemtyps
* and set upper container
**********************************************************************
    lo_alv_sum = NEW #( i_parent = lo_splitter->get_container( row    = 1
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
    MODIFY lt_fcat FROM VALUE #( no_out = 'X'
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

  METHOD _create_output_db.

* solution 1: NW < 7.50
*----------------------------------------------------------------------*
*    SELECT * FROM vbak AS h
*    INNER JOIN vbap AS i
*      ON h~vbeln = i~vbeln
*    LEFT OUTER JOIN makt AS t
*      ON  t~matnr = i~matnr
*      AND t~spras = @sy-langu
*    WHERE h~vbeln IN @it_vbeln
*    INTO CORRESPONDING FIELDS OF TABLE @rt_output.

*    SELECT h~vbeln, h~auart,
*           i~posnr, i~pstyv, i~matnr, i~aedat,
*           t~maktx
*    FROM vbak AS h
*    INNER JOIN vbap AS i
*      ON h~vbeln = i~vbeln
*    LEFT OUTER JOIN makt AS t
*      ON  t~matnr = i~matnr
*      AND t~spras = @sy-langu
*    WHERE h~vbeln IN @it_vbeln
*    INTO CORRESPONDING FIELDS OF TABLE @rt_output.

    SELECT vbeln, auart, posnr, pstyv, matnr, aedat, maktx
    FROM zbd400_sales_order_item2_00
    WHERE vbeln IN @it_vbeln
    AND   spras = @sy-langu
    INTO CORRESPONDING FIELDS OF TABLE @rt_output.

    LOOP AT rt_output ASSIGNING FIELD-SYMBOL(<out>).

      IF <out>-matnr IN it_matnr.
        <out>-color = 'C500'.
      ENDIF.

    ENDLOOP.


* solution 2: NW >= 7.50 / UNION
*----------------------------------------------------------------------*
*    SELECT h~vbeln, h~auart ,i~posnr, i~pstyv, i~matnr, i~aedat , t~maktx , 'C500' AS color FROM vbak as h
*       INNER JOIN vbap as i ON h~vbeln = i~vbeln
*       LEFT OUTER JOIN makt as t on  t~matnr = i~matnr
*                                 AND t~spras = @sy-langu
*    WHERE h~vbeln in @it_vbeln
*    AND   i~matnr in @it_matnr
*    UNION
*    SELECT h~vbeln, h~auart ,i~posnr, i~pstyv, i~matnr, i~aedat , t~maktx , @space AS color FROM vbak as h
*       INNER JOIN vbap as i ON h~vbeln = i~vbeln
*       LEFT OUTER JOIN makt as t on  t~matnr = i~matnr
*                                 AND t~spras = @sy-langu
*    WHERE h~vbeln in @it_vbeln
*    AND   i~matnr NOT in @it_matnr
*    ORDER BY vbeln, posnr
*    INTO CORRESPONDING FIELDS OF TABLE @rt_output.


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
    %_&1_%_app_%-text = &2.
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
