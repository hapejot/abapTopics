CLASS zcl_tpc_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ltt_strings TYPE STANDARD TABLE OF string WITH DEFAULT KEY .
    TYPES:
      ltt_text TYPE STANDARD TABLE OF tline WITH DEFAULT KEY .
    TYPES:
      BEGIN OF lts_text_entry,
        thead     TYPE thead,
        text      TYPE ltt_text,
        updflg(1) TYPE c,
      END OF lts_text_entry .

    CLASS-METHODS:
      parse_text_test
        IMPORTING
          i_name     TYPE stxh-tdname
        EXPORTING
          et_token   TYPE tsftoken
          et_strings TYPE zcl_tpc_main=>ltt_strings.

    METHODS parsed_text_get
      IMPORTING
        !it_text    TYPE ltt_text
      EXPORTING
        !et_token   TYPE tsftoken
        !et_strings TYPE ltt_strings .
    METHODS constructor .
    METHODS start_of_selection
      IMPORTING
        !i_name TYPE stxh-tdname .
    METHODS user_command
      IMPORTING
        !i_ucomm TYPE syst-ucomm .
    METHODS get_editor
      RETURNING
        VALUE(r_result) TYPE REF TO cl_gui_textedit .
    METHODS set_editor
      IMPORTING
        !i_m_editor TYPE REF TO cl_gui_textedit .
    METHODS text_set
      IMPORTING
        !i_text   TYPE ltt_text OPTIONAL
        !i_header TYPE thead .
    METHODS save_editor .
    METHODS save_text_all .
    METHODS get_name
      RETURNING
        VALUE(r_result) TYPE stxh-tdname .
    METHODS get_text
      IMPORTING
        !i_tdid    TYPE thead-tdid
        !i_tdspras TYPE thead-tdspras
        !i_name    TYPE tdobname
      EXPORTING
        !es_head   TYPE thead
        !et_text   TYPE ltt_text .
    METHODS text_read
      IMPORTING
        !i_header      TYPE thead
      RETURNING
        VALUE(lr_text) TYPE REF TO lts_text_entry .
    METHODS set_name
      IMPORTING
        !i_m_name TYPE stxh-tdname
        !i_id     TYPE tdid DEFAULT 'ZZTC'
        !i_lang   TYPE syst_langu DEFAULT sy-langu .
    METHODS load_editor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA m_editor TYPE REF TO cl_gui_textedit .
    DATA mt_text TYPE ltt_text .
    DATA m_text_name TYPE stxh-tdname .
    DATA m_text_id TYPE thead-tdid .
    DATA m_text_lang TYPE thead-tdspras .
    DATA:
      m_text_ds TYPE STANDARD TABLE OF lts_text_entry,
      ms_header TYPE thead.

    METHODS read_text .
    METHODS move_text_to_note_editor .
    METHODS read_text_from_note_editor .
    METHODS save_text .
ENDCLASS.



CLASS zcl_tpc_main IMPLEMENTATION.


  METHOD constructor.

  ENDMETHOD.


  METHOD get_editor.

    r_result = me->m_editor.

  ENDMETHOD.


  METHOD get_name.

    r_result = me->m_text_name.

  ENDMETHOD.


  METHOD get_text.
    DATA: lt_text TYPE ltt_text.

    DATA(lr_txt) = REF #( m_text_ds[ thead-tdname = i_name ] OPTIONAL ).
    IF lr_txt IS NOT BOUND.
      APPEND VALUE #( thead = VALUE #(
                            tdobject    = 'TEXT'
                            tdid        = i_tdid
                            tdspras     = i_tdspras
                            tdname = |{ i_name CASE = UPPER }| )
                      updflg = space ) TO m_text_ds REFERENCE INTO lr_txt.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = i_tdid
          language                = i_tdspras
          name                    = i_name
          object                  = 'TEXT'
        IMPORTING
          header                  = lr_txt->thead
        TABLES
          lines                   = lr_txt->text
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_msg).
      ENDIF.
    ENDIF.
    et_text = lr_txt->text.
    es_head = lr_txt->thead.

  ENDMETHOD.


  METHOD load_editor.

    read_text( ).
    move_text_to_note_editor( ).

  ENDMETHOD.


  METHOD move_text_to_note_editor.
    DATA: notetab TYPE STANDARD TABLE OF notetab_line.

* convert itf text to stream text
    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      TABLES
        itf_text    = mt_text
        text_stream = notetab.
* and send text to note editor
    CALL METHOD m_editor->set_text_as_stream
      EXPORTING
        text   = notetab
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
*   add your handling
    ENDIF.

  ENDMETHOD.


  METHOD parsed_text_get.

    TYPES: itf_token TYPE stxtoken.

* token-codes
    CONSTANTS:                                                    " L  S
      c_token_element_begin   TYPE itf_token-code VALUE '{E', " n ...
      c_token_element_end     TYPE itf_token-code VALUE '}E', " n ...
      c_token_paragraph_begin TYPE itf_token-code VALUE '{P', " 2  xx
      c_token_paragraph_end   TYPE itf_token-code VALUE '}P', " 2  xx
      c_token_line_begin      TYPE itf_token-code VALUE '(L', " 0
      c_token_line_end        TYPE itf_token-code VALUE ')L', " 0
      c_token_tab             TYPE itf_token-code VALUE 'TA', " 0
      c_token_ltag_begin      TYPE itf_token-code VALUE '(H', " n  ...
      c_token_ltag_end        TYPE itf_token-code VALUE ')H', " n  ...
      c_token_ctag_begin      TYPE itf_token-code VALUE '(C', " 2  xx
      c_token_ctag_end        TYPE itf_token-code VALUE ')C', " 2  xx
      c_token_url_begin       TYPE itf_token-code VALUE '(U', " 2  %W
      c_token_url_end         TYPE itf_token-code VALUE ')U', " 2  %W
      c_token_symbol_begin    TYPE itf_token-code VALUE '(&', " 0
      c_token_symbol_end      TYPE itf_token-code VALUE ')&', " 0
      c_token_symbol          TYPE itf_token-code VALUE '&&', " n  ...
      c_token_prefix_begin    TYPE itf_token-code VALUE '(P', " 0
      c_token_prefix_end      TYPE itf_token-code VALUE ')P', " 0
      c_token_option          TYPE itf_token-code VALUE 'OP', " n  ...
      c_token_suffix_begin    TYPE itf_token-code VALUE '(S', " 0
      c_token_suffix_end      TYPE itf_token-code VALUE ')S', " 0
      c_token_sap_char        TYPE itf_token-code VALUE 'SC', " n  ...
      c_token_string          TYPE itf_token-code VALUE 'ST', " n  ...
      c_token_template_string TYPE itf_token-code VALUE 'TS', " n  ...
      c_token_command_begin   TYPE itf_token-code VALUE '[C', " 0
      c_token_command_end     TYPE itf_token-code VALUE ']C', " 0
      c_token_command         TYPE itf_token-code VALUE 'CO', " n  ...
      c_token_comment         TYPE itf_token-code VALUE '**', " n  ...
      c_token_text_end        TYPE itf_token-code VALUE '##', " 0
* nur bei Kommandoanalyse (anstelle von C_TOKEN_COMMAND)
      c_token_ident           TYPE itf_token-code VALUE 'ID', " n  ...
      c_token_cliteral        TYPE itf_token-code VALUE 'CL', " n  ...
      c_token_nliteral        TYPE itf_token-code VALUE 'NL', " n  ...
      c_token_dchar           TYPE itf_token-code VALUE 'DC', " 2  xx
      c_token_char            TYPE itf_token-code VALUE 'CH', " 1  x
      c_token_ill_string      TYPE itf_token-code VALUE 'IS', " n  ...
* nur bei INCLUDE-Auflösung
      c_token_include_begin   TYPE itf_token-code VALUE '!B', " 0
      c_token_include_end     TYPE itf_token-code VALUE '!E', " 0
* niemals hier benutzen, werden von Smart Composer generiert!!!
      c_token_string_char     TYPE itf_token-code VALUE 'NS', " n  ...
      c_token_string_space    TYPE itf_token-code VALUE 'SS', " n  ...
      c_token_keep_space      TYPE itf_token-code VALUE 'KS', " 0  ...
      c_token_xsymbol_begin   TYPE itf_token-code VALUE '(X', " n  ...
      c_token_xsymbol_end     TYPE itf_token-code VALUE ')X', " n  ...
      c_token_xcounter_begin  TYPE itf_token-code VALUE '(G', " n  ...
      c_token_xcounter_end    TYPE itf_token-code VALUE ')G', " n  ...
      c_token_xbarcode_begin  TYPE itf_token-code VALUE '(B', " n  ...
      c_token_xbarcode_end    TYPE itf_token-code VALUE ')B', " n  ...
      c_token_xsapicon        TYPE itf_token-code VALUE 'XI', " n  ...
      c_token_xsapding        TYPE itf_token-code VALUE 'XD', " n  ...
      c_token_link            TYPE itf_token-code VALUE 'LK'. " n  ...


    CONSTANTS:
      c_with_command_scan(1)    VALUE '1',  " Kommandos analysieren
      c_without_command_scan(1) VALUE '2',  " Kommandos nicht analys.
      c_form_text(1)            VALUE '1',  " Formulartext
      c_no_form_text(1)         VALUE '2',  " kein Formulartext
      c_with_url_token          VALUE '1',  " mit URL Erkennung
      c_without_url_token       VALUE '2'.  " ohen URL Erkennung

    DATA:
      lt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      l_str     TYPE string,
      l_line    TYPE string,
      p_token   TYPE stxtoken.

    PERFORM init_itf_scanner IN PROGRAM rstxscan USING c_with_command_scan
                                                 c_no_form_text.

    PERFORM set_itf_text IN PROGRAM rstxscan USING it_text.
    DO.
      PERFORM read_next_itf_token IN PROGRAM rstxscan USING p_token.
      IF p_token-code = c_token_text_end.
        EXIT.
      ENDIF.
      DATA(l_pos) = p_token-len.
*      p_token-string+l_pos(1) = ':'.
      CASE p_token-code.
        WHEN c_token_string.
          l_str = |{ p_token-string WIDTH = p_token-len }|.
          DATA(l_len) = strlen( l_str ).
          ASSERT l_len = p_token-len.
          APPEND l_str TO lt_string.
        WHEN c_token_line_begin.
          CLEAR l_line.
          CLEAR lt_string.
        WHEN c_token_line_end.
          CONCATENATE LINES OF lt_string INTO l_line RESPECTING BLANKS.
          APPEND |{ l_line }\n| TO et_strings.
      ENDCASE.
      APPEND p_token TO et_token.
    ENDDO.
  ENDMETHOD.


  METHOD read_text.


    CALL METHOD me->get_text
      EXPORTING
        i_tdid    = m_text_id
        i_tdspras = m_text_lang
        i_name    = m_text_name
      IMPORTING
        es_head   = ms_header
        et_text   = mt_text.

  ENDMETHOD.


  METHOD read_text_from_note_editor.
    DATA: notetab TYPE STANDARD TABLE OF notetab_line.

* read text from note editor
    CALL METHOD m_editor->get_text_as_stream
      IMPORTING
        text = notetab.
    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        OTHERS = 1.
* and convert text stream into ITF text
    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      TABLES
        text_stream = notetab
        itf_text    = mt_text.

  ENDMETHOD.


  METHOD save_editor.

    read_text_from_note_editor( ).
    save_text( ).

  ENDMETHOD.


  METHOD save_text.

    IF ms_header IS NOT INITIAL.
      text_set(
        EXPORTING
          i_text   = mt_text
          i_header = ms_header    " SAPscript: Text Header
      ).
    ENDIF.

  ENDMETHOD.


  METHOD save_text_all.
    DATA:
          ls_text_entry LIKE LINE OF m_text_ds.
    LOOP AT m_text_ds INTO ls_text_entry.
      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          header   = ls_text_entry-thead
        TABLES
          lines    = ls_text_entry-text
        EXCEPTIONS
          id       = 1
          language = 2
          name     = 3
          object   = 4
          OTHERS   = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_msg).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_editor.

    me->m_editor = i_m_editor.

  ENDMETHOD.


  METHOD set_name.

    m_text_name = i_m_name.
    m_text_id = i_id.
    m_text_lang = i_lang.

  ENDMETHOD.


  METHOD start_of_selection.

    WRITE / space.
    m_editor = NEW cl_gui_textedit(
*        max_number_chars           =
*        style                      = 0
*        wordwrap_mode              = WORDWRAP_AT_WINDOWBORDER
*        wordwrap_position          = -1
*        wordwrap_to_linebreak_mode = FALSE
*        filedrop_mode              = DROPFILE_EVENT_OFF
        parent                     = cl_gui_container=>default_screen
*        lifetime                   =
*        name                       =
    ).

    m_text_name = i_name.

    load_editor( ).

  ENDMETHOD.


  METHOD text_read.

    APPEND VALUE #( ) TO m_text_ds REFERENCE INTO lr_text.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = i_header-tdid
        language                = i_header-tdspras
        name                    = i_header-tdname
        object                  = 'TEXT'
      IMPORTING
        header                  = lr_text->thead
      TABLES
        lines                   = lr_text->text
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_msg).
    ENDIF.

  ENDMETHOD.


  METHOD text_set.

    DATA(lr_txt) = REF #( m_text_ds[ thead-tdname = i_header-tdname ] OPTIONAL ).
    IF lr_txt IS NOT BOUND.
      lr_txt = text_read( i_header ).
    ENDIF.
    lr_txt->thead = VALUE #( BASE i_header
                              tdobject = 'TEXT' ).
    IF i_text IS SUPPLIED.
      lr_txt->text = i_text.
    ENDIF.
    lr_txt->updflg = 'U'.

  ENDMETHOD.


  METHOD user_command.

    MESSAGE i_ucomm TYPE 'I'.

    CASE i_ucomm.
      WHEN 'SAVE'.
        save_editor( ).
    ENDCASE.

  ENDMETHOD.

  METHOD parse_text_test.
    DATA(lr_app) = NEW zcl_tpc_main( ).
    CALL METHOD lr_app->get_text
      EXPORTING
        i_tdid    = 'ZZTC'
        i_tdspras = 'E'
        i_name    = i_name
      IMPORTING
        et_text   = DATA(lt_text).

    CALL METHOD lr_app->parsed_text_get
      EXPORTING
        it_text    = lt_text
      IMPORTING
        et_token   = et_token
        et_strings = et_strings.

  ENDMETHOD.

ENDCLASS.
