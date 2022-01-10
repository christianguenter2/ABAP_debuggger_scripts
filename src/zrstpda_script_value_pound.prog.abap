*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZRSTPDA_SCRIPT_VALUE_POUND</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Generate VALUE # Statement for content of internal table</SCRIPT_COMMENT>
*<SINGLE_STEP>X</SINGLE_STEP>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super  .

  PUBLIC SECTION.
    DATA gv_tabname    TYPE tabname.
    DATA gv_tab_from   TYPE i.
    DATA gv_tab_to     TYPE i.
    DATA gt_content    TYPE tpda_scr_table_content_it.
    DATA gt_components TYPE tpda_scr_table_comp_it.
    DATA gv_all_fields TYPE xfeld.
    DATA gv_field_cnt  TYPE i.

    METHODS prologue  REDEFINITION.
    METHODS init      REDEFINITION.
    METHODS script    REDEFINITION.
    METHODS end       REDEFINITION.

    METHODS get_table_fields.
    METHODS build_val_pound_expression.

ENDCLASS.                    "lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.
  METHOD prologue.
*** generate abap_source (source handler for ABAP)
    super->prologue( ).
  ENDMETHOD.                    "prolog

  METHOD init.
    DATA lt_sval   TYPE STANDARD TABLE OF sval.
    DATA lv_answer TYPE c LENGTH 1.
    DATA lv_title  TYPE string.
    DATA ls_popup  TYPE ZDEBUGGER_SCRIPTING_S. "Just for the where used list

    IMPORT tabname TO gv_tabname FROM MEMORY ID sy-repid.

    lv_title = 'Script Control Parameters'(000).

    lt_sval = VALUE #( tabname = 'ZDEBUGGER_SCRIPTING_S'
                       ( fieldname = 'ITAB'             fieldtext = 'Internal Table Name'(001)  value = gv_tabname field_obl = abap_true )
                       ( fieldname = 'ALL_TABLE_FIELDS' fieldtext = 'Use all table fields'(002) value = abap_true                        )
                       ( fieldname = 'ITAB_FROM'        fieldtext = 'From record'(003)          value = '1'                              )
                       ( fieldname = 'ITAB_TO'          fieldtext = 'To record'(004)            value = '9999'                           )
                     ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = lv_title
      IMPORTING
        returncode      = lv_answer
      TABLES
        fields          = lt_sval
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc IS NOT INITIAL.
      raise_error( ).
    ENDIF.

    IF lv_answer = 'A'.
*     User canceled
      raise_error( ).
    ENDIF.

    LOOP AT lt_sval INTO DATA(ls_sval).
      CASE sy-tabix.
        WHEN 1.
          gv_tabname   = ls_sval-value.
          EXPORT tabname FROM gv_tabname TO MEMORY ID sy-repid.

        WHEN 2.
          gv_all_fields = ls_sval-value.

        WHEN 3.
          gv_tab_from  = ls_sval-value.

        WHEN 4.
          gv_tab_to    = ls_sval-value.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.                    "init
  METHOD script.
    DATA lo_tab     TYPE REF TO cl_tpda_script_tabledescr.
    DATA lt_val     TYPE tpda_scr_table_content_it.
    DATA lt_comp    TYPE tpda_scr_table_comp_it.
    DATA lv_records TYPE i.

    TRY.
        lo_tab ?= cl_tpda_script_data_descr=>factory( CONV #( gv_tabname ) ).

        lv_records = lo_tab->linecnt( ).

        IF lv_records < gv_tab_to.
          gv_tab_to = lv_records.
        ENDIF.

        lo_tab->content( EXPORTING p_line_from   = gv_tab_from
                                   p_line_to     = gv_tab_to
                         IMPORTING p_it_comp_val = gt_content ).

        gt_components = lo_tab->components( ).

        get_table_fields( ).

        build_val_pound_expression( ).

      CATCH cx_root INTO DATA(lo_root).
        RETURN.
    ENDTRY.

  ENDMETHOD.                    "script

  METHOD get_table_fields.
    DATA lt_sval   TYPE STANDARD TABLE OF spopli.
    DATA lv_answer TYPE c LENGTH 1.
    DATA lv_title  TYPE string.

    gv_field_cnt = lines( gt_components ).

    IF gv_all_fields = abap_true.
      RETURN.
    ENDIF.

    lv_title = 'Select Table Fields'(003).

    LOOP AT gt_components INTO DATA(ls_comp).
      APPEND VALUE #( selflag = abap_true varoption = ls_comp-compname ) TO lt_sval.
    ENDLOOP.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        mark_flag          = abap_true
        mark_max           = 100
        textline1          = 'Fieldname'(005)
        titel              = 'Select Table Fields'(006)
      IMPORTING
        answer             = lv_answer
      TABLES
        t_spopli           = lt_sval
      EXCEPTIONS
        not_enough_answers = 1
        too_much_answers   = 2
        too_much_marks     = 3
        OTHERS             = 4.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF lv_answer = 'A'.
*     User canceled
      RETURN.
    ENDIF.

    CLEAR gv_field_cnt.

    LOOP AT lt_sval INTO DATA(ls_sval).
      IF ls_sval-selflag IS INITIAL.
        READ TABLE gt_components INDEX sy-tabix ASSIGNING FIELD-SYMBOL(<ls_comp>).
        CHECK sy-subrc IS INITIAL.

        CLEAR <ls_comp>-compname.
      ELSE.
        ADD 1 TO gv_field_cnt.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD build_val_pound_expression.
    DATA lv_max_fields_per_line TYPE n LENGTH 2 VALUE '10'.
    DATA lv_max_field_length    TYPE i.

    DATA BEGIN OF ls_value.
    DATA record TYPE c LENGTH 255.
    DATA END OF ls_value.

    DATA lt_value LIKE STANDARD TABLE OF ls_value.

    LOOP AT gt_components INTO DATA(ls_comp).
      DATA(lv_length) = strlen( ls_comp-compname ).

      IF lv_length > lv_max_field_length.
        lv_max_field_length = lv_length.
      ENDIF.

    ENDLOOP.

    APPEND VALUE #( record = |{ gv_tabname CASE = LOWER } = VALUE #( | ) TO lt_value.

    LOOP AT gt_content ASSIGNING FIELD-SYMBOL(<ls_tabrecord>).
      IF gv_field_cnt > lv_max_fields_per_line.
        APPEND VALUE #( record  = '(' ) TO lt_value.
      ELSE.
        ls_value-record = '( '.
      ENDIF.

      LOOP AT <ls_tabrecord>-fields INTO DATA(lv_field).
        READ TABLE gt_components INDEX sy-tabix INTO ls_comp.
        CHECK sy-subrc IS INITIAL AND ls_comp-compname IS NOT INITIAL.

        CASE ls_comp-typid.
          WHEN 'D' OR 'T'.
            CHECK lv_field CN '0 '.

          WHEN 'I' OR 'N' OR 'P'.
            CHECK lv_field CN '0., '.

          WHEN 'C'.
            CHECK lv_field <> space.

        ENDCASE.

        IF gv_field_cnt > lv_max_fields_per_line.
          IF ls_comp-compname <> 'MANDT'.
            ls_value-record = | { ls_comp-compname WIDTH = lv_max_field_length } = '{ lv_field }' |.
          ELSE.
            ls_value-record = | { ls_comp-compname WIDTH = lv_max_field_length } = sy-mandt |.
          ENDIF.

          APPEND ls_value TO lt_value.
        ELSE.
          IF ls_comp-compname <> 'MANDT'.
            ls_value-record = ls_value-record && | { ls_comp-compname } = '{ lv_field }' |.
          ELSE.
            ls_value-record = ls_value-record && | { ls_comp-compname } = sy-mandt |.
          ENDIF.
        ENDIF.

      ENDLOOP.
      IF gv_field_cnt <= lv_max_fields_per_line
        AND ls_value IS NOT INITIAL.
        ls_value-record = ls_value-record && ' )'.
        APPEND ls_value TO lt_value.
      ELSE.
        APPEND VALUE #( record = ')' ) TO lt_value.
      ENDIF.

      CLEAR ls_value.
    ENDLOOP.

    APPEND VALUE #( record = |).| ) TO lt_value.

    EDITOR-CALL FOR lt_value.

  ENDMETHOD.

  METHOD end.
*** insert your code which shall be executed at the end of the scripting (before trace is saved)
*** here

  ENDMETHOD.                    "end
ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION​
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
