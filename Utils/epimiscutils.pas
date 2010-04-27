unit epimiscutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes;

const
  EpiTypeCastArray: array[TEpiFieldType, TEpiFieldType] of boolean =
      // Cast To Types
//                ftBoolean, ftInteger, ftAutoInc, ftFloat, ftDMYDate, ftMDYDate, ftYMDDate, ftDMYToday, ftMDYToday, ftYMDToday, ftTime, ftTimeNow, ftString, ftUpperString
{C} {ftBoolean} ((true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{a} {ftInteger}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{s} {ftAutoInc}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{t} {ftFloat}    (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,     true),
    {ftDMYDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{F} {ftMDYDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{r} {ftYMDDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{o} {ftDMYToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{m} {ftMDYToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
    {ftYMDToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
    {ftTime}     (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,     true),
    {ftTimeNow}  (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,     true),
    {ftString}   (true,      false,     false,     false,   false,     false,     false,     false,      false,      false,      false,  false,     true,     true),
  {ftUpperString}(true,      false,     false,     false,   false,     false,     false,     false,      false,      false,      false,  false,     true,     true));

implementation

end.

