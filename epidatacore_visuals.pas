{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit epidatacore_visuals;

interface

uses
  epiv_dataset_viewer_frame, epiv_documentfile, epiv_dataset_viewer_frame_mac, 
  epiv_dataform_treeview, epiv_projecttreeview_frame, epiv_datamodule, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('epidatacore_visuals', @Register);
end.
