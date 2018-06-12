*&---------------------------------------------------------------------*
*& Report ZR_PDFTEX_MAIN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_pdftex_main.

DATA:
  mr_main TYPE REF TO zcl_pdftex_main,
  mr_cont TYPE REF TO zcl_abapdi_container.



INITIALIZATION.
  CREATE OBJECT mr_cont.
  mr_main = CAST zcl_pdftex_main( mr_cont->get_instance( 'ZCL_PDFTEX_MAIN' ) ).
  mr_main->initialize( ).

AT SELECTION-SCREEN.

AT USER-COMMAND.
  mr_main->user_command( sy-ucomm ).


START-OF-SELECTION.
  SET PF-STATUS 'MAIN'.
  WRITE / ' '.
