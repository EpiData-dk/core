{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit epidatacore_visuals;

{$warn 5023 off : no warning about unused units}
interface

uses
  epiv_dataset_viewer_frame, epiv_documentfile, epiv_dataset_viewer_frame_mac, 
  epiv_dataform_treeview, epiv_projecttreeview_frame, epiv_datamodule, 
  epiv_field_list_frame, epiv_checkversionform, epiv_userlogin_form, 
  epiv_custom_statusbar, epiv_statusbar_item_recordcount, 
  epiv_statusbar_item_cycleno, epiv_statusbar_item_savetime, 
  epiv_statusbar_item_selectionnames, epiv_statusbar_item_currentuser, 
  epiv_statusbar_item_progressbar, epiv_statusbar_item_saveicon, 
  epiv_first_run_wizard, wizard_form, wizard_datadir_frame, 
  wizard_welcome_page, wizard_summary_frame, wizard_progress_frame, 
  wizard_confirm_frame, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('epidatacore_visuals', @Register);
end.
