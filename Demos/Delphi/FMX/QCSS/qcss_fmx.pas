unit qcss_fmx;

interface

uses classes, rtti, qcss, types, fmx.types;

type
  TQCSSFMXHelper = class(TQCSS)
  protected
    FRttiType: TRttiType;
    procedure DoBeforeStyling; override;
    procedure DoAfterStyled; override;
    procedure DoTransform(AObject: TObject; AValue: TQCSSAttrValue);
    procedure DoBackground(AObject: TObject; AValue: TQCSSAttrValue);
  public
    constructor Create; overload;
  end;

implementation

uses qstring;
{ TQCSSFMXHelper }

constructor TQCSSFMXHelper.Create;
begin
  inherited;
  Register('transform', DoTransform);
  Register('background', DoBackground);
end;

procedure TQCSSFMXHelper.DoAfterStyled;
begin
  inherited;
  FRttiType := nil;
end;

procedure TQCSSFMXHelper.DoBackground(AObject: TObject; AValue: TQCSSAttrValue);
begin
  if StartWithW(PQCharW(AValue.Name), '#', false) then // ÑÕÉ«
  begin

  end;
end;

procedure TQCSSFMXHelper.DoBeforeStyling;
begin
  inherited;
  FRttiType := TRttiContext.Create.GetType(StyingObject.ClassType);
end;

procedure TQCSSFMXHelper.DoTransform(AObject: TObject; AValue: TQCSSAttrValue);
var
  AProp: TRttiProperty;
  procedure DoRotate;
  begin
    if Length(AValue.Params) = 1 then
    begin
      with AValue.Params[0] do
      begin
        if (ParamType = TQCSSAttrValueType.aptUnitValue) and (UnitName = 'deg')
        then
        begin
          AProp := TRttiContext.Create.GetType(AObject.ClassType)
            .GetProperty('RotationAngle');
          if Assigned(AProp) then
            AProp.SetValue(AObject, Double(Value));
        end;
      end;
    end;
  end;
  procedure DoScale;
  var
    AScale: TPosition;
  begin
    if Length(AValue.Params) = 2 then
    begin
      AProp := TRttiContext.Create.GetType(AObject.ClassType)
        .GetProperty('Scale');
      if Assigned(AProp) then
      begin
        AScale := AProp.GetValue(AObject).AsObject as TPosition;
        AScale.Point := PointF(AValue.Params[0].Value, AValue.Params[1].Value);
      end;
    end;
  end;

begin
  if AValue.Name = 'rotate' then // rotate Ö§³Ö
    DoRotate
  else if AValue.Name = 'scale' then
    DoScale;
end;

end.
