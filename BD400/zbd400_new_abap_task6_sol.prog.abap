*&---------------------------------------------------------------------*
*& Report  ZBD400_NEW_ABAP_TASK6_TMP
*&---------------------------------------------------------------------*
REPORT ZBD400_NEW_ABAP_TASK6_SOL.

TYPES: BEGIN OF ts_struct1,
        vbeln TYPE vbak-vbeln,
        posnr TYPE vbap-posnr,
        empty TYPE abap_bool,
        matnr TYPE mara-matnr,
        maktx TYPE makt-maktx,
       END OF ts_struct1.

TYPES: BEGIN OF ts_struct2,
        vbeln TYPE vbak-vbeln,
        posnr TYPE vbap-posnr,
        no_stock TYPE abap_bool,
        matnr TYPE mara-matnr,
        maktx TYPE makt-maktx,
       END OF ts_struct2.

DATA: ls_struct1 TYPe ts_struct1,
      ls_struct2 TYPE ts_struct2.

* set value
**********************************************************************
ls_struct1-matnr = '4711'.
ls_struct1-vbeln = '123456789'.
ls_struct1-posnr = '40'.
ls_struct1-empty = 'X'.
ls_struct1-maktx = 'Kölnisch Wasser'.

ls_struct2-matnr = '4711'.
ls_struct2-maktx = 'Aqua Cologne'.

* create and use mapping
**********************************************************************
DATA(lo_map) = cl_abap_corresponding=>create(
                 source = ls_struct1
                 destination = ls_struct2
                 mapping = value #( ( level   = 0
                                      kind    = cl_abap_corresponding=>MAPPING_COMPONENT
                                      srcname = 'EMPTY'
                                      dstname = 'NO_STOCK' )
                                    ( level   = 0
                                      kind    = cl_abap_corresponding=>MAPPING_EXCEPT_COMPONENT
                                      srcname = 'MAKTX' )
                 )
               ).

  lo_map->execute( EXPORTING source      = ls_struct1
                   CHANGING  destination = ls_struct2 ).

* display
**********************************************************************
DATA(lo_out) = cl_demo_output=>new( ).

lo_out->write(
  EXPORTING
    data   = ls_struct1 ).

lo_out->write(
  EXPORTING
    data   = ls_struct2 ).

lo_out->display( ).
