REPORT zbd400_new_abap_task6_tmp.

TYPES: BEGIN OF ts_struct1,
         vbeln TYPE vbak-vbeln,
         posnr TYPE vbap-posnr,
         empty TYPE abap_bool,
         matnr TYPE mara-matnr,
         maktx TYPE makt-maktx,
       END OF ts_struct1.

TYPES: BEGIN OF ts_struct2,
         vbeln    TYPE vbak-vbeln,
         posnr    TYPE vbap-posnr,
         no_stock TYPE abap_bool,
         matnr    TYPE mara-matnr,
         maktx    TYPE makt-maktx,
       END OF ts_struct2.

DATA: ls_struct1 TYPE ts_struct1,
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
