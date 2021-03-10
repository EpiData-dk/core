unit epireport_report_userlist;

{$codepage utf8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epiadmin;

type

  EEpiReportUserList = class(EEpiReportBaseException);

  { TEpiReportUserInfo }

  TEpiReportUserList = class(TEpiReportBase)
  private
    FAdmin: TEpiAdmin;
  protected
    procedure DoSanityCheck; override;
  public
    procedure RunReport; override;
    property Admin: TEpiAdmin read FAdmin write FAdmin;
  end;

implementation

{ TEpiReportUserInfo }

procedure TEpiReportUserList.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if (not Assigned(Admin)) then
    DoError(EEpiReportUserList, 'Admin not assigned');
end;

procedure TEpiReportUserList.RunReport;
var
  Users: TEpiUsers;
  i: Integer;
  U: TEpiUser;
  S: String;
  G: TEpiGroup;
begin
  inherited RunReport;

  Users := Admin.Users;

  DoTableHeader('Users', 8, Users.Count + 1);

  DoTableCell(0, 0, 'Login');
  DoTableCell(1, 0, 'Name');
  DoTableCell(2, 0, 'Created');
  DoTableCell(3, 0, 'Last Modified');
  DoTableCell(4, 0, 'Last Login');
  DoTableCell(5, 0, 'Expires');
  DoTableCell(6, 0, 'Notes');
  DoTableCell(7, 0, 'Groups');

  i := 1;
  for U in Users do
    begin
      DoTableCell(0, i, U.Login);
      DoTableCell(1, i, U.FullName);
      DoTableCell(2, i, DateTimeToStr(U.Created));
      DoTableCell(3, i, DateTimeToStr(U.Modified));

      if U.LastLogin = 0 then
        DoTableCell(4, i, 'N/A')
      else
        DoTableCell(4, i, DateTimeToStr(U.LastLogin));

      if U.ExpireDate = 0 then
        DoTableCell(5, i, 'Never')
      else
        DoTableCell(5, i, DateTimeToStr(U.ExpireDate));

      DoTableCell(6, i, U.Notes);

      S := '';
      for G in U.Groups do
        S += G.Caption.Text + LineEnding;

      DoTableCell(7, i, S);
      Inc(i);
    end;
  DoTableFooter('');

end;

end.

