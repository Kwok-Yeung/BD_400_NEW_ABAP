CLASS zcl_bd400_class_with_abap_doc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Default language. It will be used if a text does not exist in the session language.
    CONSTANTS c_default_langu TYPE sy-langu VALUE 'D'.

    "! Get a materials short text.
    "! @parameter if_matnr | Material number
    "! @parameter rf_descr | Short text
    METHODS get_description
      IMPORTING
                if_matnr        TYPE mara-matnr
      RETURNING VALUE(rf_descr) TYPE makt-maktx.


  PROTECTED SECTION.

    "! ALV table's type
    TYPES tt_alv TYPE STANDARD TABLE OF makt.


    "! Print alv list data.
    "! @parameter if_small | abap_true = Print the most important fields. abap_false = Print all fields.
    "! @parameter it_alv | ALV list
    METHODS print_data
      IMPORTING
        if_small TYPE abap_bool
        it_alv   TYPE tt_alv.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bd400_class_with_abap_doc IMPLEMENTATION.

  METHOD get_description.
    "! Temporary language
    DATA lf_langu_tmp TYPE sy-langu.


  ENDMETHOD.

  METHOD print_data.

  ENDMETHOD.

ENDCLASS.
