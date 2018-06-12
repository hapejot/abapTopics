CLASS zcl_pdftex_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS initialize.
    METHODS user_command
      IMPORTING
        i_ucomm TYPE syst-ucomm.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF lts_stxhd,
        tdid      TYPE stxh-tdid,
        tdobject  TYPE stxh-tdobject,
        tdname    TYPE stxh-tdname,
        tdspras   TYPE stxh-tdspras,
        tdtitle   TYPE stxh-tdtitle,
        updflg(1) TYPE c,
        style     TYPE lvc_t_styl,
      END OF lts_stxhd .
    TYPES:
      ty_lt_lines TYPE STANDARD TABLE OF line WITH DEFAULT KEY .

    DATA mr_viewer TYPE REF TO cl_gui_html_viewer .
    DATA mr_split TYPE REF TO cl_gui_splitter_container .
    DATA mr_edit TYPE REF TO cl_gui_textedit .
    DATA m_pdf TYPE xstring .
    DATA mt_pdf_data TYPE lt_pdf_table .
    DATA mt_pdf_line TYPE lt_pdf_line .
    DATA m_offset TYPE i .
    DATA:
      m_url(1000) TYPE c .
    DATA m_len TYPE i .
    DATA:
      mt_stxh     TYPE STANDARD TABLE OF lts_stxhd WITH DEFAULT KEY .
    DATA mr_split2 TYPE REF TO cl_gui_splitter_container .
    DATA mr_grid TYPE REF TO cl_gui_alv_grid .
    DATA mr_tpc_main TYPE REF TO zcl_tpc_main .
    DATA mt_fieldcat TYPE lvc_t_fcat .

    METHODS on_row_select
          FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
          !e_row
          !e_column
          !es_row_no .
    METHODS pdf_doc_render .
    METHODS on_toolbar_def
          FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
          !e_object
          !e_interactive .
    METHODS on_cell_change
          FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
          !er_data_changed
          !e_onf4
          !e_onf4_before
          !e_onf4_after
          !e_ucomm .
    METHODS on_user_command
          FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
          !e_ucomm .
ENDCLASS.



CLASS ZCL_PDFTEX_MAIN IMPLEMENTATION.


  METHOD initialize.
    DATA: lt_fieldcat TYPE lvc_t_fcat.
*lr_split->set_row_mode( mode = cl_gui_splitter_container=>mode_relative ).
*cl_gui_container=>default_screen->set_row_height( id = 1 height = 50 ).
    mr_split = NEW cl_gui_splitter_container(
        parent                  =  cl_gui_container=>default_screen
        rows                    = 1
        columns                 = 2
        no_autodef_progid_dynnr = abap_true
    ).

    mr_split2 = NEW cl_gui_splitter_container(
            parent = mr_split->get_container(
                                              row       =     1
                                              column    =     1 )
            rows = 2
            columns = 1
    ).

    CREATE OBJECT mr_viewer
      EXPORTING
        parent = mr_split->get_container(
                                          row       =     1
                                          column    =     2 )
      EXCEPTIONS
        OTHERS = 5.

    mr_edit = NEW cl_gui_textedit(
        parent                     = mr_split2->get_container(
                                          row       =     2
                                          column    =     1
                                      )
    ).
    mr_edit->set_font_fixed( ).

    mr_grid = NEW cl_gui_alv_grid(
        i_parent          = mr_split2->get_container( row = 1 column = 1 )

    ).
    SET HANDLER on_row_select FOR mr_grid.
    SET HANDLER on_toolbar_def FOR mr_grid.
    SET HANDLER on_user_command FOR mr_grid.
    SET HANDLER on_cell_change FOR mr_grid.
* Register events
    CALL METHOD mr_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD mr_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'STXH'
      CHANGING
        ct_fieldcat            = lt_fieldcat    " Field Catalog with Field Descriptions
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    APPEND lt_fieldcat[ fieldname = 'TDNAME' ] TO mt_fieldcat.
    APPEND VALUE #( BASE lt_fieldcat[ fieldname = 'TDTITLE' ] edit = abap_true ) TO mt_fieldcat.
    APPEND lt_fieldcat[ fieldname = 'TDSPRAS' ] TO mt_fieldcat.

    SELECT * FROM stxh
    INTO CORRESPONDING FIELDS OF TABLE @mt_stxh
    WHERE
    tdobject = 'TEXT'
    AND tdid = 'ZZTC'.

    CALL METHOD mr_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = VALUE lvc_s_layo(
                    edit         = abap_false
                    no_vgridln   = abap_true
                    cwidth_opt   = abap_true
                    stylefname   = 'STYLE'
                )
      CHANGING
        it_outtab                     = mt_stxh    " Output Table
        it_fieldcatalog               = mt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno.
    ENDIF.
    mr_tpc_main = NEW zcl_tpc_main( ).
    mr_tpc_main->set_editor( i_m_editor = mr_edit ).

  ENDMETHOD.


  METHOD on_cell_change.
    FIELD-SYMBOLS:
    <ft_mod_rows> TYPE STANDARD TABLE.
    DATA(x) = er_data_changed->mt_mod_cells.
    DATA(mod1) = REF #( x[ 1 ] OPTIONAL ).
    IF mod1 IS BOUND.
      MESSAGE |Zeile { mod1->row_id } bearbeitet.| TYPE 'S'.
      ASSIGN er_data_changed->mp_mod_rows->* TO <ft_mod_rows>.
      mr_tpc_main->text_set(
          i_header = CORRESPONDING #( <ft_mod_rows>[ mod1->tabix ] )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD on_row_select.

    DATA(lr_stxh) = REF #( mt_stxh[ e_row-index ] ).
    mr_tpc_main->save_editor( ).
    mr_tpc_main->set_name(  i_m_name = lr_stxh->tdname
                            i_id = lr_stxh->tdid
                            i_lang = lr_stxh->tdspras ).
    mr_tpc_main->load_editor( ).

  ENDMETHOD.


  METHOD on_toolbar_def.

    APPEND VALUE #( butn_type = 0
                    icon = icon_execute_object
                    function = 'ADD'
                    quickinfo = 'add'
                    text = 'Add' ) TO e_object->mt_toolbar.

  ENDMETHOD.


  METHOD on_user_command.
    " MESSAGE e_ucomm TYPE 'I'.
    CASE e_ucomm.
      WHEN 'ADD'.
        APPEND VALUE #(
              tdid = 'ZZTC'
              tdspras = sy-langu
              style = VALUE #(
                      ( fieldname = 'TDNAME' style = cl_gui_alv_grid=>mc_style_enabled )
                      )
              ) TO mt_stxh.
        mr_grid->refresh_table_display(
*  EXPORTING
*    is_stable      =     " With Stable Rows/Columns
*    i_soft_refresh =     " Without Sort, Filter, etc.
*  EXCEPTIONS
*    finished       = 1
*    others         = 2
        ).
        IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD pdf_doc_render.

    DATA:
      lt_lines  TYPE ty_lt_lines,
      l_text    TYPE string,
      l_session TYPE char40,
      l_subrc   TYPE syst_subrc,
      lt_text   TYPE zcl_tpc_main=>ltt_text.

    CALL FUNCTION 'Z_PDFTEX_OPEN' DESTINATION 'PRG_PJL'
      IMPORTING
        e_sessionid = l_session.

    LOOP AT mt_stxh INTO DATA(ls_stxh).
      CALL METHOD mr_tpc_main->get_text
        EXPORTING
          i_tdid    = ls_stxh-tdid
          i_tdspras = ls_stxh-tdspras
          i_name    = ls_stxh-tdname
        IMPORTING
*         es_head   =
          et_text   = lt_text.

      CALL METHOD mr_tpc_main->parsed_text_get
        EXPORTING
          it_text    = lt_text
        IMPORTING
          et_strings = DATA(lt_tdtab).

      CONCATENATE LINES OF lt_tdtab INTO l_text.
      CLEAR lt_tdtab[].
      CALL FUNCTION 'Z_PDFTEX_ATTACH' DESTINATION 'PRG_PJL'
        EXPORTING
          i_sessionid = l_session
          i_data      = l_text
          i_encoding  = 'utf8'
          i_name      = CONV char0064( |{ ls_stxh-tdname CASE = LOWER }.tex| ).
    ENDLOOP.

    l_text = |\\input "{ mr_tpc_main->get_name( ) CASE = LOWER }"|.


    CALL FUNCTION 'Z_PDFTEX_RUN' DESTINATION 'PRG_PJL'
      EXPORTING
        i_sessionid           = l_session
        i_source              = l_text
      IMPORTING
        e_pdf                 = m_pdf
        e_rc                  = l_subrc
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2.

    IF l_subrc IS NOT INITIAL.
      MESSAGE |TeX process returned with errors.| TYPE 'I'.
    ENDIF.

*    CALL FUNCTION 'Z_PDFTEX_CLOSE' DESTINATION 'PRG_PJL'
*      EXPORTING
*        i_sessionid = l_session.

    m_len = xstrlen( m_pdf ).
    CLEAR m_offset.
    CLEAR mt_pdf_data[].
    WHILE m_len >= 1000.
      mt_pdf_line = m_pdf+m_offset(1000).
      APPEND mt_pdf_line TO mt_pdf_data.
      ADD 1000 TO m_offset.
      SUBTRACT 1000 FROM m_len.
    ENDWHILE.

    IF m_len > 0.
      mt_pdf_line = m_pdf+m_offset(m_len).
      APPEND mt_pdf_line TO mt_pdf_data.
    ENDIF.

    m_len = xstrlen( m_pdf ).

    CALL METHOD mr_viewer->load_data
      EXPORTING
        url                    = 'test.pdf'
        size                   = m_len
        type                   = 'application'
        subtype                = 'pdf'
      IMPORTING
        assigned_url           = m_url
      CHANGING
        data_table             = mt_pdf_data
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5.
    CALL METHOD mr_viewer->show_url
      EXPORTING
        url    = m_url
      EXCEPTIONS
        OTHERS = 5.

  ENDMETHOD.


  METHOD user_command.

    DATA: lt_lines  TYPE STANDARD TABLE OF line,
          l_text    TYPE string,
          l_session TYPE char40.

    CASE sy-ucomm.
      WHEN 'BACK' OR 'RETURN' OR 'CANCEL'.
        SET SCREEN 0.

      WHEN 'PDF'.
        mr_tpc_main->save_editor( ).
        pdf_doc_render( ).

      WHEN 'SAVE'.
        mr_tpc_main->save_editor( ).
        mr_tpc_main->save_text_all( ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
